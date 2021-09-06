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
main = defaultMain $ testGroup "." [ 
    testOscillator @CP.System "oscilAssign" oscilAssignFSM,
    testOscillator @CP.System "oscilVar" oscilVarFSM,
    testOscillator @CP.System "oscilVar2" oscilVar2FSM,
    testOscillator @CP.System "oscilCall" oscilCallFSM,
    testOscillator @CP.System "oscilLift" oscilLiftFSM,
    testCounter @CP.System "count" countFSM,
    testCounter @CP.System "countLet" countLetFSM,
    testSlowCounter @CP.System "countSlow" countSlowFSM,
    testSlowCounter @CP.System "countSlowLet" countSlowLetFSM,
    testSlowOptCounter @CP.System "countSlowOpt" countSlowOptFSM,
    testSlowOptCounter @CP.System "countSlowOptVar" countSlowOptVarFSM,
    testSlowOptCounter @CP.System "countSlowOptCall" countSlowOptCallFSM,
    testCounterEnMoore @CP.System "countEnMoore" countEnMooreFSM,
    testCounterEnMoore @CP.System "countEnMoore2" countEnMoore2FSM,
    testCounterEnMealy @CP.System "countEnMealy" countEnMealyFSM,
    testCounterUpDown @CP.System "countUpDown" countUpDownFSM]
    where
    testOscillator :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom () -> CP.Signal dom Bool) -> TestTree
    testOscillator name machine = TU.testCase name $ CP.simulateN 100 machine (repeat ()) TU.@?= take 100 (cycle [False, True])
    testCounter :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom () -> CP.Signal dom Integer) -> TestTree
    testCounter name machine = TU.testCase name $ CP.simulateN 100 machine (repeat ()) TU.@?= [(0 :: Integer)..99]
    testSlowCounter :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom () -> CP.Signal dom Integer) -> TestTree
    testSlowCounter name machine = TU.testCase name $ CP.simulateN 100 machine (repeat ()) TU.@?= dup [(0 :: Integer)..49]
    testSlowOptCounter :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testSlowOptCounter name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== countSlowOpt 0 l
    testCounterEnMoore :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testCounterEnMoore name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== take (length l) (scanl (\a b -> if b then a+1 else a) 0 l)
    testCounterEnMealy :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testCounterEnMealy name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== drop 1 (scanl (\a b -> if b then a+1 else a) 0 l)
    testCounterUpDown :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => Integer -> CP.Signal dom () -> CP.Signal dom Integer) -> TestTree
    testCounterUpDown name machine = TH.testProperty name $ H.property $ do
        m <- H.forAll $ Gen.integral $ Range.constant 1 100
        CP.simulateN 100 (machine m) (repeat ()) H.=== take 100 (cycle $ [0..m-1] ++ [m,m-1..1])

[fsm|oscilLiftFSM :: (CP.HiddenClockResetEnable dom)
                  => CP.Signal dom () -> CP.Signal dom Bool
inputs ()
fun f ():
    let x = False
    fun g y:
        emit x == y
        let x = True
        ret call g (not y)
    ret call g True
ret call f ()
|]

[fsm|oscilAssignFSM :: (CP.HiddenClockResetEnable dom)
                    => CP.Signal dom () -> CP.Signal dom Bool
inputs ()
forever
    var x = False
    emit x
    x = not x
    emit x
|]

[fsm|oscilVarFSM :: (CP.HiddenClockResetEnable dom)
                 => CP.Signal dom () -> CP.Signal dom Bool
inputs ()
var x = True
forever
    x = not x
    emit x
|]

[fsm|oscilVar2FSM :: (CP.HiddenClockResetEnable dom)
                 => CP.Signal dom () -> CP.Signal dom Bool
inputs ()
var x = False
forever
    emit x
    x = not x
|]

[fsm|oscilCallFSM :: (CP.HiddenClockResetEnable dom)
                    => CP.Signal dom () -> CP.Signal dom Bool
inputs ()
var x = True
fun n ():
    x = not x
forever
    call n ()
    emit x
|]

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

[fsm|countSlowOptVarFSM :: (CP.HiddenClockResetEnable dom) 
                     => CP.Signal dom Bool -> CP.Signal dom Integer
inputs b
var x = -1
forever
    x = x + 1
    if b:
        emit x
    emit x
|]

[fsm|countSlowOptCallFSM :: (CP.HiddenClockResetEnable dom) 
                         => CP.Signal dom Bool -> CP.Signal dom Integer
inputs b
fun e i:
    emit i
fun f i:
    if b:
      call e i
    call e i
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

