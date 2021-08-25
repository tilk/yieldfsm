import Prelude
import qualified Clash.Prelude as CP
import Test.Tasty
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.Hedgehog as TH
import qualified Test.Tasty.HUnit as TU

import FSM

dup :: [a] -> [a]
dup [] = []
dup (x:xs) = x:x:dup xs

countSlowOpt :: Num a => a -> [Bool] -> [a]
countSlowOpt _ [] = []
countSlowOpt n (False:xs) = n:countSlowOpt (n+1) xs
countSlowOpt n (True:xs) = n:f xs where
    f [] = []
    f (_:xs') = n:countSlowOpt (n+1) xs'

main :: IO ()
main = defaultMain $ testGroup "."
  [ 
    TU.testCase "count" $ CP.simulateN @CP.System 100 countFSM (repeat ()) TU.@?= [(0 :: Integer)..99],
    TU.testCase "countLet" $ CP.simulateN @CP.System 100 countLetFSM (repeat ()) TU.@?= [(0 :: Integer)..99],
    TU.testCase "countSlow" $ CP.simulateN @CP.System 100 countSlowFSM (repeat ()) TU.@?= dup [(0 :: Integer)..49],
    TU.testCase "countSlowLet" $ CP.simulateN @CP.System 100 countSlowLetFSM (repeat ()) TU.@?= dup [(0 :: Integer)..49],
    TH.testProperty "countSlowOpt" $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN @CP.System (length l) countSlowOptFSM l H.=== countSlowOpt 0 l,
    TH.testProperty "countEnMoore" $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN @CP.System (length l) countEnMooreFSM l H.=== take (length l) (scanl (\a b -> if b then a+1 else a) 0 l),
    TH.testProperty "countEnMoore2" $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN @CP.System (length l) countEnMoore2FSM l H.=== take (length l) (scanl (\a b -> if b then a+1 else a) 0 l),
    TH.testProperty "countEnMealy" $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN @CP.System (length l) countEnMealyFSM l H.=== drop 1 (scanl (\a b -> if b then a+1 else a) 0 l),
    TH.testProperty "countUpDown" $ H.property $ do
        m <- H.forAll $ Gen.integral $ Range.constant 1 100
        CP.simulateN @CP.System 100 (countUpDownFSM m) (repeat ()) H.=== take 100 (cycle $ [0..m-1] ++ [m,m-1..1])
  ]

[fsm|countFSM :: (CP.HiddenClockResetEnable dom) 
              => CP.Signal dom () -> CP.Signal dom Integer
inputs ()
fun f i:
    emit i
    ret call f (i+1)
ret call f 0
|]

[fsm|countLetFSM :: (CP.HiddenClockResetEnable dom) 
                 => CP.Signal dom () -> CP.Signal dom Integer
inputs ()
fun f i:
    let ii = i+1
    emit i
    ret call f ii
ret call f 0
|]

[fsm|countSlowFSM :: (CP.HiddenClockResetEnable dom) 
                  => CP.Signal dom () -> CP.Signal dom Integer
inputs ()
fun f i:
    emit i
    emit i
    ret call f (i+1)
ret call f 0
|]

[fsm|countSlowLetFSM :: (CP.HiddenClockResetEnable dom) 
                     => CP.Signal dom () -> CP.Signal dom Integer
inputs ()
fun f i:
    let ii = i+1
    emit i
    emit i
    ret call f ii
ret call f 0
|]

[fsm|countSlowOptFSM :: (CP.HiddenClockResetEnable dom) 
                     => CP.Signal dom Bool -> CP.Signal dom Integer
inputs b
fun f i:
    if b:
      emit i
    emit i
    ret call f (i+1)
ret call f 0
|]

[fsm|countEnMooreFSM :: (CP.HiddenClockResetEnable dom)
                     => CP.Signal dom Bool -> CP.Signal dom Integer
inputs b
fun g (i, j):
    emit i
    ret call f j
fun f i:
    if b:
        ret call g (i, (i+1))
    else
        ret call g (i, i)
ret call f 0
|]

[fsm|countEnMoore2FSM :: (CP.HiddenClockResetEnable dom)
                      => CP.Signal dom Bool -> CP.Signal dom Integer
inputs b
fun f i:
    let bb = b
    emit i
    if bb:
        ret call f (i+1)
    else
        ret call f i
ret call f 0
|]

[fsm|countEnMealyFSM :: (CP.HiddenClockResetEnable dom)
                     => CP.Signal dom Bool -> CP.Signal dom Integer
inputs b
fun g i:
    emit i
    ret call f i
fun f i:
    if b:
        ret call g (i+1)
    else
        ret call g i
ret call f 0
|]

[fsm|countUpDownFSM :: (CP.HiddenClockResetEnable dom)
                    => Integer -> CP.Signal dom () -> CP.Signal dom Integer
param m
inputs ()
fun f i:
    emit i
    if i == m:
        ret call g (i-1)
    else
        ret call f (i+1)
fun g i:
    emit i
    if i == 0:
        ret call f (i+1)
    else
        ret call g (i-1)
ret call f 0
|]

