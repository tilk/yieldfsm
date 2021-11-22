{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.DesugarOutputs(desugarOutputs) where

import Prelude
import FSM.Lang
import FSM.LangQ
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh
import FSM.Util.Qlift

desugarOutputsStmt :: (Qlift m, MonadRefresh m) => Stmt LvlSugared -> m (Stmt LvlLoops)
desugarOutputsStmt (SLoop lt s) = SLoop lt <$> desugarOutputsStmt s
desugarOutputsStmt (SBreak bt) = return $ SBreak bt
desugarOutputsStmt (SFun fs s) = SFun <$> mapM (\(p, s') -> (p,) <$> desugarOutputsStmt s') fs <*> desugarOutputsStmt s
desugarOutputsStmt (SLet t n vs s) = SLet t n vs <$> desugarOutputsStmt s
desugarOutputsStmt (SAssign n e) = return $ SAssign n e
desugarOutputsStmt (SYieldO _ e) = return $ SYield e
desugarOutputsStmt (SRet vs) = return $ SRet vs
desugarOutputsStmt (SBlock ss) = SBlock <$> mapM desugarOutputsStmt ss
desugarOutputsStmt (SIf e st sf) = SIf e <$> desugarOutputsStmt st <*> desugarOutputsStmt sf
desugarOutputsStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> desugarOutputsStmt s) cs
desugarOutputsStmt (SNop) = return SNop

desugarOutputs :: (Qlift m, MonadRefresh m) => Prog LvlSugared -> m (Prog LvlLoops)
desugarOutputs prog = do
    b <- desugarOutputsStmt $ progBody prog
    return $ prog { progBody = b }


