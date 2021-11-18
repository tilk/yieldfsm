{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.DesugarLoops(desugarLoops) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

data RData = RData {
    _rDataLoop :: Maybe TH.Name
}

$(makeLenses ''RData)

ltName :: LoopType -> String
ltName LoopForever = "forever"
ltName (LoopWhile _ _ _) = "while"
ltName (LoopRepeat _ _) = "repeat"

desugarLoopsStmt :: (MonadRefresh m, MonadReader RData m) => Stmt 'LvlSugared -> m (Stmt 'LvlFull)
desugarLoopsStmt (SLoop lt s) = do
    f <- makeName $ ltName lt
    s' <- locally rDataLoop (const $ Just f) $ desugarLoopsStmt s
    return s'
desugarLoopsStmt (SFun fs s) = SFun <$> mapM (\(p, s) -> (p,) <$> desugarLoopsStmt s) fs <*> desugarLoopsStmt s
desugarLoopsStmt (SLet t n vs s) = SLet t n vs <$> desugarLoopsStmt s
desugarLoopsStmt (SAssign n e) = return $ SAssign n e
desugarLoopsStmt (SYield e) = return $ SYield e
desugarLoopsStmt (SRet vs) = do
    l <- view rDataLoop
    if l == Nothing then return $ SRet vs else error "Return in loops currently unsupported"
desugarLoopsStmt (SBlock ss) = SBlock <$> mapM desugarLoopsStmt ss
desugarLoopsStmt (SIf e st sf) = SIf e <$> desugarLoopsStmt st <*> desugarLoopsStmt sf
desugarLoopsStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> desugarLoopsStmt s) cs
desugarLoopsStmt (SNop) = return SNop

desugarLoops :: MonadRefresh m => Prog 'LvlSugared -> m (Prog 'LvlFull)
desugarLoops prog = do
    b <- flip runReaderT (RData Nothing) $ desugarLoopsStmt $ progBody prog
    return $ prog { progBody = b }

