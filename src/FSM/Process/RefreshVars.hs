module FSM.Process.RefreshVars(refreshVars) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Data.Maybe
import Control.Monad
import Data.Key(forWithKeyM)
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

refreshVarsCase :: MonadRefresh m => M.Map TH.Name TH.Name -> (TH.Pat, Stmt) -> m (TH.Pat, Stmt)
refreshVarsCase m (p, s) = do
    (p', mp) <- refreshPat p
    (p',) <$> refreshVarsStmt (mp `M.union` m) s

refreshVarsStmt :: MonadRefresh m => M.Map TH.Name TH.Name -> Stmt -> m Stmt
refreshVarsStmt m (SLet t n vs s) = do
    n' <- refreshName n
    let m' = M.insert n n' m
    return $ SLet t n' (rename m' vs) (rename m' s)
refreshVarsStmt m (SCase e cs) = SCase (rename m e) <$> mapM (refreshVarsCase m) cs
refreshVarsStmt m (SFun fs s) =  SFun <$> mapM (refreshVarsCase m) fs <*> refreshVarsStmt m s
refreshVarsStmt m s = return $ rename m s

refreshVars :: MonadRefresh m => Prog -> m Prog
refreshVars prog = do
    s <- refreshVarsStmt M.empty $ progBody prog
    return $ prog { progBody = s }

