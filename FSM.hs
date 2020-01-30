module FSM where

import FSMLang
import FSMLangParser
import FSMLangPretty
import FSMLang2Desc
import FSMDescGenADT
import FSMLangProcess
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import Control.Monad.State
import Data.Text.Prettyprint.Doc.Render.Text
import Prelude
import Text.Trifecta

mkFSM :: String -> TH.Q [TH.Dec]
mkFSM str 
    | Success p <- pr = do
        let Just np = toNProg p
        TH.runIO $ putDoc $ prettyNProg np
        np0 <- cutBlocks np
        TH.runIO $ putDoc $ prettyNProg np0
        np' <- deTailCall np0
        TH.runIO $ putDoc $ prettyNProg np'
        let np'' = removeEpsilon np'
        TH.runIO $ putDoc $ prettyNProg np''
        compileFSM "fsm" (nprog2desc np'')
    | Failure e <- pr = do
        TH.runIO $ print $ _errDoc e
        fail "FAIL"
    where
    pr = parseString parseProg mempty str

fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

