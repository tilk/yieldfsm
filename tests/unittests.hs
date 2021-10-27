{-# LANGUAGE RankNTypes #-}
module Main where

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
    testOscillator @CP.System "oscilCall2" oscilCall2FSM,
    testOscillator @CP.System "oscilLift" oscilLiftFSM,
    testCounter @CP.System "count" countFSM,
    testCounter @CP.System "countLet" countLetFSM,
    testSlowCounter @CP.System "countSlow" countSlowFSM,
    testSlowCounter @CP.System "countSlowVarNoRet" countSlowVarNoRetFSM,
    testSlowCounter @CP.System "countSlowVarRet" countSlowVarRetFSM,
    testSlowCounter @CP.System "countSlowLet" countSlowLetFSM,
    testSlowOptCounter @CP.System "countSlowOpt" countSlowOptFSM,
    testSlowOptCounter @CP.System "countSlowOptVar" countSlowOptVarFSM,
    testSlowOptCounter @CP.System "countSlowOptCall" countSlowOptCallFSM,
    testCounterEnMoore @CP.System "countEnMoore" countEnMooreFSM,
    testCounterEnMoore @CP.System "countEnMoorePrim" countEnMoorePrimFSM,
    testCounterEnMealy @CP.System "countEnMealy" countEnMealyFSM,
    testCounterEnDelay @CP.System "countEnDelay" countEnDelayFSM,
    testCounterEnDelay @CP.System "countEnDelayFlip" countEnDelayFlipFSM,
    testCounterEnDelay @CP.System "countEnDelay2" countEnDelay2FSM,
    testCounterEnDelay @CP.System "countEnDelayTail" countEnDelayTailFSM,
    testCounterEnDelay @CP.System "countEnDelayContinue" countEnDelayContinueFSM,
    testCounterEnMoore @CP.System "countEnDelayContinue" countEnMooreContinueFSM,
    testCounterEnDelay @CP.System "countEnDelayWhile" countEnDelayWhileFSM,
    testCounterEnMoore @CP.System "countEnMooreWhile" countEnMooreWhileFSM,
    testCounterEnMoore @CP.System "countEnMooreTail" countEnMooreTailFSM,
    testCounterEnMoore @CP.System "countEnMooreTail2" countEnMooreTail2FSM,
    testCounterEnMealy @CP.System "countEnMealyTail" countEnMealyTailFSM,
    testCounterUpDown @CP.System "countUpDown" countUpDownFSM,
    testCounterUpDown @CP.System "countUpDownWhile" countUpDownWhileFSM,
    testCounterUpDown @CP.System "countUpDownWhileCall" countUpDownWhileCallFSM,
    testCounterUpDownSlow @CP.System "countUpDownWhileSlow" countUpDownWhileSlowFSM,
    testCounterUpDownSlow @CP.System "countUpDownWhileSlowCall" countUpDownWhileSlowCallFSM]
    where
    testOscillator :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool) -> TestTree
    testOscillator name machine = TU.testCase name $ tail (CP.sampleN 101 machine) TU.@?= take 100 (cycle [False, True])
    testCounter :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Integer) -> TestTree
    testCounter name machine = TU.testCase name $ tail (CP.sampleN 101 machine) TU.@?= [(0 :: Integer)..99]
    testSlowCounter :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Integer) -> TestTree
    testSlowCounter name machine = TU.testCase name $ tail (CP.sampleN 101 machine) TU.@?= dup [(0 :: Integer)..49]
    testSlowOptCounter :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testSlowOptCounter name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== countSlowOpt 0 l
    testCounterEnDelay :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testCounterEnDelay name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== take (length l) (0:drop 1 (scanl (\a b -> if b then a+1 else a) 0 (tail l)))
    testCounterEnMoore :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testCounterEnMoore name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== take (length l) (scanl (\a b -> if b then a+1 else a) 0 l)
    testCounterEnMealy :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => CP.Signal dom Bool -> CP.Signal dom Integer) -> TestTree
    testCounterEnMealy name machine = TH.testProperty name $ H.property $ do
        l <- H.forAll $ Gen.list (Range.linear 1 100) Gen.bool
        CP.simulateN (length l) machine l H.=== drop 1 (scanl (\a b -> if b then a+1 else a) 0 l)
    testCounterUpDown :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => Integer -> CP.Signal dom Integer) -> TestTree
    testCounterUpDown name machine = TH.testProperty name $ H.property $ do
        m <- H.forAll $ Gen.integral $ Range.constant 1 100
        tail (CP.sampleN 101 (machine m)) H.=== take 100 (cycle $ [0..m-1] ++ [m,m-1..1])
    testCounterUpDownSlow :: CP.KnownDomain dom => String -> (CP.HiddenClockResetEnable dom => Integer -> CP.Signal dom Integer) -> TestTree
    testCounterUpDownSlow name machine = TH.testProperty name $ H.property $ do
        m <- H.forAll $ Gen.integral $ Range.constant 1 100
        tail (CP.sampleN 101 (machine m)) H.=== take 100 (dup $ cycle $ [0..m-1] ++ [m,m-1..1])

[fsm|oscilLiftFSM :: (CP.HiddenClockResetEnable dom)
                  => CP.Signal dom Bool
fun f ():
    let x = False
    fun g y:
        yield x == y
        let x = True
        ret call g (not y)
    ret call g True
ret call f ()
|]

[fsm|oscilAssignFSM :: (CP.HiddenClockResetEnable dom)
                    => CP.Signal dom Bool
forever:
    var x = False
    yield x
    x = not x
    yield x
|]

[fsm|oscilVarFSM :: (CP.HiddenClockResetEnable dom)
                 => CP.Signal dom Bool
var x = True
forever:
    x = not x
    yield x
|]

[fsm|oscilVar2FSM :: (CP.HiddenClockResetEnable dom)
                  => CP.Signal dom Bool
var x = False
forever:
    yield x
    x = not x
|]

[fsm|oscilCallFSM :: (CP.HiddenClockResetEnable dom)
                  => CP.Signal dom Bool
var x = True
fun n ():
    x = not x
forever:
    call n ()
    yield x
|]

[fsm|oscilCall2FSM :: (CP.HiddenClockResetEnable dom)
                   => CP.Signal dom Bool
var x = False
fun n ():
    x = not x
forever:
    yield x
    call n ()
|]

[fsm|countFSM :: (CP.HiddenClockResetEnable dom) 
              => CP.Signal dom Integer
fun f i:
    yield i
    ret call f (i+1)
ret call f 0
|]

[fsm|countLetFSM :: (CP.HiddenClockResetEnable dom) 
                 => CP.Signal dom Integer
fun f i:
    let ii = i+1
    yield i
    ret call f ii
ret call f 0
|]

[fsm|countSlowFSM :: (CP.HiddenClockResetEnable dom) 
                  => CP.Signal dom Integer
fun f i:
    yield i
    yield i
    ret call f (i+1)
ret call f 0
|]

[fsm|countSlowVarNoRetFSM :: CP.HiddenClockResetEnable dom
                          => CP.Signal dom Integer
var i = 0
fun f ():
    yield i
    ret call g ()
fun g ():
    yield i
    i = i + 1
    ret call f ()
forever:
    call f ()
|]

[fsm|countSlowVarRetFSM :: CP.HiddenClockResetEnable dom
                        => CP.Signal dom Integer
var i = 0
fun f ():
    yield i
    ret call g ()
fun g ():
    yield i
    i = i + 1
    if i `mod` 2 == 0:
        ret ()
    ret call f ()
forever:
    call f ()
|]

[fsm|countSlowLetFSM :: (CP.HiddenClockResetEnable dom) 
                     => CP.Signal dom Integer
fun f i:
    let ii = i+1
    yield i
    yield i
    ret call f ii
ret call f 0
|]

[fsm|countSlowOptFSM :: (CP.HiddenClockResetEnable dom) 
                     => CP.Signal dom Bool -> CP.Signal dom Integer
input b
fun f i:
    if b:
        yield i
    yield i
    ret call f (i+1)
ret call f 0
|]

[fsm|countSlowOptVarFSM :: (CP.HiddenClockResetEnable dom) 
                     => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0
forever:
    if b:
        yield x
    yield x
    x = x + 1
|]

[fsm|countSlowOptCallFSM :: (CP.HiddenClockResetEnable dom) 
                         => CP.Signal dom Bool -> CP.Signal dom Integer
input b
fun e i:
    yield i
fun f i:
    if b:
        call e i
    call e i
    ret call f (i+1)
ret call f 0
|]

[fsm|countEnMooreFSM :: (CP.HiddenClockResetEnable dom)
                     => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0
forever:
    let bb = b
    yield x
    if bb:
        x = x + 1
|]

[fsm|countEnMoorePrimFSM :: (CP.HiddenClockResetEnable dom)
                         => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0
forever:
    yield x
    if b':
        x = x + 1
|]

[fsm|countEnMealyFSM :: (CP.HiddenClockResetEnable dom)
                     => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0 :: Integer
forever:
    if b:
        x = x + 1
    yield x
|]

[fsm|countEnDelayFSM :: (CP.HiddenClockResetEnable dom)
                     => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0 :: Integer
forever:
    yield x
    if b:
        x = x + 1
|]

[fsm|countEnDelayFlipFSM :: (CP.HiddenClockResetEnable dom)
                         => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0 :: Integer
yield x
forever:
    if b:
        x = x + 1
    yield x
|]

[fsm|countEnDelay2FSM :: (CP.HiddenClockResetEnable dom)
                      => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0 :: Integer
forever:
    yield x
    if b:
        x = x + 1
    yield x
    if b:
        x = x + 1
|]

[fsm|countEnDelayTailFSM :: (CP.HiddenClockResetEnable dom)
                         => CP.Signal dom Bool -> CP.Signal dom Integer
input b
fun f x:
    yield x
    ret call f (x + if b then 1 else 0)
ret call f (0 :: Integer)
|]

[fsm|countEnDelayContinueFSM :: (CP.HiddenClockResetEnable dom)
                             => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0 :: Integer
forever:
    yield x
    if not b:
        continue
    x = x + 1
|]

[fsm|countEnDelayWhileFSM :: (CP.HiddenClockResetEnable dom)
                          => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0
forever:
    do:
        yield x
    until b
    x = x + 1
|]

[fsm|countEnMooreContinueFSM :: (CP.HiddenClockResetEnable dom)
                             => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0
forever:
    yield x
    if not b':
        continue
    x = x + 1
|]

[fsm|countEnMooreWhileFSM :: (CP.HiddenClockResetEnable dom)
                          => CP.Signal dom Bool -> CP.Signal dom Integer
input b
var x = 0
forever:
    do:
        yield x
    until b'
    x = x + 1
|]

[fsm|countEnMooreTailFSM :: (CP.HiddenClockResetEnable dom)
                         => CP.Signal dom Bool -> CP.Signal dom Integer
input b
fun g (i, j):
    yield i
    ret call f j
fun f i:
    if b:
        ret call g (i, (i+1))
    else:
        ret call g (i, i)
ret call f 0
|]

[fsm|countEnMooreTail2FSM :: (CP.HiddenClockResetEnable dom)
                          => CP.Signal dom Bool -> CP.Signal dom Integer
input b
fun f i:
    let bb = b
    yield i
    if bb:
        ret call f (i+1)
    else:
        ret call f i
ret call f 0
|]

[fsm|countEnMealyTailFSM :: (CP.HiddenClockResetEnable dom)
                         => CP.Signal dom Bool -> CP.Signal dom Integer
input b
fun g i:
    yield i
    ret call f i
fun f i:
    if b:
        ret call g (i+1)
    else:
        ret call g i
ret call f 0
|]

[fsm|countUpDownFSM :: (CP.HiddenClockResetEnable dom)
                    => Integer -> CP.Signal dom Integer
param m
fun f i:
    yield i
    if i == m:
        ret call g (i-1)
    else:
        ret call f (i+1)
fun g i:
    yield i
    if i == 0:
        ret call f (i+1)
    else:
        ret call g (i-1)
ret call f 0
|]

[fsm|countUpDownWhileFSM :: (CP.HiddenClockResetEnable dom)
                         => Integer -> CP.Signal dom Integer
param m
var i = 0
forever:
    do:
        yield i
        i = i + 1
    until i == m
    do:
        yield i
        i = i - 1
    while i /= 0
|]

[fsm|countUpDownWhileCallFSM :: (CP.HiddenClockResetEnable dom)
                             => Integer -> CP.Signal dom Integer
param m
fun e i:
    yield i
var i = 0
forever:
    do:
        call e i
        i = i + 1
    until i == m
    do:
        call e i
        i = i - 1
    while i /= 0
|]

[fsm|countUpDownWhileSlowFSM :: (CP.HiddenClockResetEnable dom)
                             => Integer -> CP.Signal dom Integer
param m
var i = 0
forever:
    do:
        yield i
        yield i
        i = i + 1
    until i == m
    do:
        yield i
        yield i
        i = i - 1
    while i /= 0
|]

[fsm|countUpDownWhileSlowCallFSM :: (CP.HiddenClockResetEnable dom)
                                 => Integer -> CP.Signal dom Integer
param m
fun e i:
    yield i
var i = 0
forever:
    do:
        call e i
        call e i
        i = i + 1
    until i == m
    do:
        call e i
        call e i
        i = i - 1
    while i /= 0
|]

