module FSM where

import FSMLangParser
import FSMLang2Desc
import FSMDescGenADT
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import Prelude
import Text.Trifecta

mkFSM :: String -> TH.Q [TH.Dec]
mkFSM str = compileFSM "fsm" $ lang2desc p
    where Success p = parseString parseProg mempty str

fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

