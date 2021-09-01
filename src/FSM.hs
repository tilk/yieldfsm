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
import Text.Megaparsec
import System.IO

mkFSM :: String -> TH.Q [TH.Dec]
mkFSM str = do
    pr <- runParseProg str
    case pr of
        Right p -> do
            TH.runIO $ hPutStrLn stderr ""
            TH.runIO $ hPutStrLn stderr $ show $ progName p
            p' <- deTailCall p
            TH.runIO $ hPutDoc stderr $ prettyProg p'
            p'' <- makeLocalVars p'
            TH.runIO $ hPutDoc stderr $ prettyProg p''
            np <- lambdaLift p''
            TH.runIO $ hPutDoc stderr $ prettyNProg np
            np0 <- cutBlocks np
            TH.runIO $ hPutDoc stderr $ prettyNProg np0
            np' <- makeTailCalls np0
            TH.runIO $ hPutDoc stderr $ prettyNProg np'
            np'' <- removeEpsilon np'
            TH.runIO $ hPutDoc stderr $ prettyNProg np''
            ret <- compileFSM (nprog2desc np'')
            TH.runIO $ hFlush stderr
            return ret
        Left e -> do
            TH.runIO $ do
                hPutStrLn stderr $ errorBundlePretty e
                hFlush stderr
            fail "FAIL"

fsm :: THQ.QuasiQuoter
fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

