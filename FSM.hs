module FSM where

import FSMLang
import FSMLangParser
import FSMLang2Desc
import FSMDescGenADT
import FSMLangProcess
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import Prelude
import Text.Trifecta

mkFSM :: String -> TH.Q [TH.Dec]
mkFSM str = do
    TH.runIO $ print np
    np' <- cutBlocks np
    TH.runIO $ print np'
    compileFSM "fsm" (nprog2desc np')
    where
    Success p = parseString parseProg mempty str
    Just np = toNProg p

fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

