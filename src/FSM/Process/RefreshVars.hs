{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>
|-}
module FSM.Process.RefreshVars(refreshVars) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

refreshVarsCase :: (IsDesugared l, MonadRefresh m) => M.Map TH.Name TH.Name -> (TH.Pat, Stmt l) -> m (TH.Pat, Stmt l)
refreshVarsCase m (p, s) = do
    (p', mp) <- refreshPat p
    (p',) <$> refreshVarsStmt (mp `M.union` m) s

refreshVarsStmt :: (IsDesugared l, MonadRefresh m) => M.Map TH.Name TH.Name -> Stmt l -> m (Stmt l)
refreshVarsStmt m (SLet t n vs s) = do
    n' <- refreshName n
    let m' = M.insert n n' m
    return $ SLet t n' (rename m' vs) (rename m' s)
refreshVarsStmt m (SCase e cs) = SCase (rename m e) <$> mapM (refreshVarsCase m) cs
refreshVarsStmt m (SFun fs s) =  SFun <$> mapM (refreshVarsCase m) fs <*> refreshVarsStmt m s
refreshVarsStmt m s = return $ rename m s

refreshVars :: (IsDesugared l, MonadRefresh m) => Prog l -> m (Prog l)
refreshVars prog = do
    s <- refreshVarsStmt M.empty $ progBody prog
    return $ prog { progBody = s }

