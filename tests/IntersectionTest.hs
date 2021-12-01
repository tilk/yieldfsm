module IntersectionTest where

import Clash.Prelude
import IntersectionModel
import FSM

import Test.Tasty as T
import qualified Test.Tasty.HUnit as TU

[fsm|intersectionFSM0 :: HiddenClockResetEnable dom
                      => Signal dom Inputs
                      -> Signal dom (Color, Color, Bool)
input i
output mcol = Red
output ocol = Red
output timer = False
var exit = undefined
forever:
    do:
        exit = carOnOther i && greenTime i
        yield<mcol, timer> (Green, exit)
    until exit
    do:
        exit = yellowTime i
        yield<mcol, timer> (Yellow, exit)
    until exit
    do:
        exit = not (carOnOther i) || greenTime i
        yield<ocol, timer> (Green, exit)
    until exit
    do:
        exit = yellowTime i
        yield<ocol, timer> (Yellow, exit)
    until exit
|]

intersectionFSM :: HiddenClockResetEnable dom
                => Signal dom Inputs
                -> Signal dom Outputs
intersectionFSM i = (\(mc, oc, t) -> Outputs mc oc t) <$> intersectionFSM0 i

testIntersection :: forall dom. KnownDomain dom => String -> (HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom Outputs) -> T.TestTree
testIntersection name machine = TU.testCase name $ sampleN @dom 100 model1 TU.@?= sampleN @dom 100 model2
    where
    thr = 20 :: Unsigned 5
    tG = 50 :: Unsigned 6
    tY = 5 :: Unsigned 3
    model1 :: HiddenClockResetEnable dom => Signal dom (Color, Color)
    model1 = intersectionModel thr tG tY intersectionController
    model2 :: HiddenClockResetEnable dom => Signal dom (Color, Color)
    model2 = intersectionModel thr tG tY machine

