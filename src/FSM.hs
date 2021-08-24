module FSM where

import FSM.Lang
import FSM.LangParser
import FSM.LangPretty
import FSM.Lang2Desc
import FSM.DescGenADT
import FSM.LangProcess
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
        np'1 <- makeTailCalls np'
        TH.runIO $ putDoc $ prettyNProg np'1
        let np'' = removeEpsilon np'1
        TH.runIO $ putDoc $ prettyNProg np''
        compileFSM (nprog2desc np'')
    | Failure e <- pr = do
        TH.runIO $ print $ _errDoc e
        fail "FAIL"
    where
    pr = parseString parseProg mempty str

fsm :: THQ.QuasiQuoter
fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

