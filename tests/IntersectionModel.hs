module IntersectionModel where

import Clash.Prelude

data Color = Red | Yellow | Green
    deriving (Eq, Show, Generic, NFDataX)

data State = MainRed | MainYellow | OtherRed | OtherYellow
    deriving (Eq, Show, Generic, NFDataX)

data Inputs = Inputs {
    carOnOther :: Bool,
    greenTime :: Bool,
    yellowTime :: Bool
}

data Outputs = Outputs {
    mainColor :: Color,
    otherColor :: Color,
    startTimer :: Bool
}

stateMainColor :: State -> Color
stateMainColor MainRed = Red
stateMainColor MainYellow = Yellow
stateMainColor OtherRed = Green
stateMainColor OtherYellow = Red

stateOtherColor :: State -> Color
stateOtherColor MainRed = Green
stateOtherColor MainYellow = Red
stateOtherColor OtherRed = Red
stateOtherColor OtherYellow = Yellow

nextState :: Inputs -> State -> State
nextState i OtherRed | carOnOther i && greenTime i = MainYellow
                     | otherwise = OtherRed
nextState i MainYellow | yellowTime i = MainRed
                       | otherwise = MainYellow
nextState i MainRed | not (carOnOther i) || greenTime i = OtherYellow
                    | otherwise = MainRed
nextState i OtherYellow | yellowTime i = OtherRed
                        | otherwise = OtherYellow

intersectionController :: HiddenClockResetEnable dom
                       => Signal dom Inputs -> Signal dom Outputs
intersectionController i = Outputs <$> (stateMainColor <$> state)
                                   <*> (stateOtherColor <$> state)
                                   <*> ((/=) <$> state <*> next)
    where
    state = register OtherRed next
    next = nextState <$> i <*> state

intersectionCounter :: (KnownNat n, HiddenClockResetEnable dom)
                    => Unsigned n -> Signal dom Bool -> Signal dom Bool
intersectionCounter top s = (== 0) <$> cnt
    where
    cnt = register top ((\sv cntv -> if sv then top else satPred SatBound cntv) <$> s <*> cnt)

otherCarCounter :: (KnownNat n, HiddenClockResetEnable dom)
                => Unsigned n -> Signal dom Color -> Signal dom Bool
otherCarCounter thr col = val
    where
    cnt = register 0 (updateCnt <$> col <*> cnt)
    val = register False (updateVal <$> val <*> cnt)
    updateCnt Green = satPred SatBound
    updateCnt Yellow = id
    updateCnt Red = satSucc SatBound
    updateVal False = (>= thr)
    updateVal True = (> 0)

intersectionModel :: (KnownNat nt, KnownNat ng, KnownNat ny, HiddenClockResetEnable dom)
                  => Unsigned nt
                  -> Unsigned ng
                  -> Unsigned ny
                  -> (Signal dom Inputs -> Signal dom Outputs)
                  -> Signal dom (Color, Color)
intersectionModel thr tG tY inter = (,) <$> (mainColor <$> outs) <*> (otherColor <$> outs)
    where
    outs = inter ins
    ins = Inputs <$> (otherCarCounter thr (otherColor <$> outs))
                 <*> intersectionCounter tG (startTimer <$> outs)
                 <*> intersectionCounter tY (startTimer <$> outs)


