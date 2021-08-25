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
mkFSM str 
    | Right p <- pr = do
        TH.runIO $ hPutStrLn stderr ""
        TH.runIO $ hPutStrLn stderr $ show $ progName p
        let Just np = toNProg p
        TH.runIO $ hPutDoc stderr $ prettyNProg np
        np0 <- cutBlocks np
        TH.runIO $ hPutDoc stderr $ prettyNProg np0
        np' <- deTailCall np0
        TH.runIO $ hPutDoc stderr $ prettyNProg np'
        np'1 <- makeTailCalls np'
        TH.runIO $ hPutDoc stderr $ prettyNProg np'1
        np'' <- removeEpsilon np'1
        TH.runIO $ hPutDoc stderr $ prettyNProg np''
        ret <- compileFSM (nprog2desc np'')
        TH.runIO $ hFlush stderr
        return ret
    | Left e <- pr = do
        TH.runIO $ do
            hPutStrLn stderr $ errorBundlePretty e
            hFlush stderr
        fail "FAIL"
    where
    pr = runParser parseProg "" str

fsm :: THQ.QuasiQuoter
fsm = THQ.QuasiQuoter undefined undefined undefined mkFSM

