module FSM where

import FSMLang
import FSMLangParser
import FSMLang2Desc
import FSMDescGenADT
import FSMLangProcess
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import Control.Monad.State
import Prelude
import Text.Trifecta

mkFSM :: String -> TH.Q [TH.Dec]
mkFSM str 
    | Success p <- pr = do
        let Just np = toNProg p
        TH.runIO $ print np
        np0 <- cutBlocks np
        TH.runIO $ print np0
        np' <- deTailCall np0
        TH.runIO $ print np'
        let np'' = removeEpsilon np'
        TH.runIO $ print np''
        compileFSM "fsm" (nprog2desc np'')
    | Failure e <- pr = do
        TH.runIO $ print $ _errDoc e
        fail "FAIL"
    where
    pr = parseString parseProg mempty str

fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

