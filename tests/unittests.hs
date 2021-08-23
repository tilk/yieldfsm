import Prelude
import qualified Clash.Prelude as CP
import Test.Tasty
import qualified Hedgehog as H
import qualified Test.Tasty.Hedgehog as TH
import qualified Test.Tasty.HUnit as TU

import FSM

main :: IO ()
main = defaultMain $ testGroup "."
  [ 
    TU.testCase "upcount" $ CP.simulateN @CP.System 100 seqFSM (repeat ()) TU.@?= [(0 :: Integer)..99]
--    TH.testProperty "upcount" $ H.property $ 1 H.=== 1
  ]

[fsm|seqFSM :: (CP.HiddenClockResetEnable dom) => CP.Signal dom () -> CP.Signal dom Integer
inputs ()
fun f i
    begin
        emit i
        ret call f (i+1)
    end
ret call f 0
|]

