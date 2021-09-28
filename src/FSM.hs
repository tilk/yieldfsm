module FSM where

import FSM.Lang
import FSM.LangParser
import FSM.LangPretty
import FSM.Lang2Desc
import FSM.DescGenADT
import FSM.LangProcess
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import Text.PrettyPrint as HPJ
import Control.Monad.State
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
            p' <- deTailCall . previousInputs =<< refreshVars =<< refreshFunctions p
            TH.runIO $ hPutStrLn stderr $ "deTailCall:"
            TH.runIO $ hPutStrLn stderr $ HPJ.render $ prettyProgHPJ p'
            p'' <- simplifyCase <$> makeLocalVars p'
            TH.runIO $ hPutStrLn stderr $ "makeLocalVars:"
            TH.runIO $ hPutStrLn stderr $ HPJ.render $ prettyProgHPJ p''
            np <- lambdaLift p''
            TH.runIO $ hPutStrLn stderr $ "lambdaLift:"
            TH.runIO $ hPutStrLn stderr $ HPJ.render $ prettyNProgHPJ np
            np0 <- cutBlocks np
            TH.runIO $ hPutStrLn stderr $ "cutBlocks:"
            TH.runIO $ hPutStrLn stderr $ HPJ.render $ prettyNProgHPJ np0
            np' <- foldInit . simplifyCaseN <$> makeTailCalls np0
            TH.runIO $ hPutStrLn stderr $ "makeTailCalls:"
            TH.runIO $ hPutStrLn stderr $ HPJ.render $ prettyNProgHPJ np'
            np'' <- flattenTuples . cleanUnusedConstructors . simplifyCaseN <$> removeEpsilon np'
            TH.runIO $ hPutStrLn stderr $ "removeEpsilon:"
            TH.runIO $ hPutStrLn stderr $ HPJ.render $ prettyNProgHPJ np''
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

