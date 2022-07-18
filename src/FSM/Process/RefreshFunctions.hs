{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>
-}
module FSM.Process.RefreshFunctions(refreshFunctions) where

import FSM.Lang
import Prelude
import Data.Maybe
import Control.Monad
import Data.Key(forWithKeyM)
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

refreshFunctionsVStmt :: MonadRefresh m => M.Map TH.Name TH.Name -> VStmt -> m VStmt
refreshFunctionsVStmt _ (VExp e) = return $ VExp e
refreshFunctionsVStmt m (VCall f e) = return $ VCall (fromJust $ M.lookup f m) e

refreshFunctionsStmt :: (IsDesugared l, MonadRefresh m) => M.Map TH.Name TH.Name -> Stmt l -> m (Stmt l)
refreshFunctionsStmt _ SNop = return SNop
refreshFunctionsStmt _ (SYield e) = return $ SYield e
refreshFunctionsStmt m (SYieldT e s) = SYieldT e <$> refreshFunctionsStmt m s
refreshFunctionsStmt m (SRet vs) = SRet <$> refreshFunctionsVStmt m vs
refreshFunctionsStmt _ (SAssign n e) = return $ SAssign n e
refreshFunctionsStmt m (SBlock ss) = SBlock <$> mapM (refreshFunctionsStmt m) ss
refreshFunctionsStmt m (SIf e st sf) = SIf e <$> refreshFunctionsStmt m st <*> refreshFunctionsStmt m sf
refreshFunctionsStmt m (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> refreshFunctionsStmt m s) cs
refreshFunctionsStmt m (SLet t ln vs s) = SLet t ln <$> refreshFunctionsVStmt m vs <*> refreshFunctionsStmt m s
refreshFunctionsStmt m (SFun fs s) = do
    m' <- (`M.union` m) <$> (forWithKeyM fs $ \f _ -> refreshName f)
    SFun <$> (M.fromList <$> forM (M.toList fs) (\(f, (p, s')) ->  (fromJust $ M.lookup f m', ) . (p,) <$> refreshFunctionsStmt m' s')) <*> refreshFunctionsStmt m' s

refreshFunctions :: (IsDesugared l, MonadRefresh m) => Prog l -> m (Prog l)
refreshFunctions prog = do
    s <- refreshFunctionsStmt M.empty $ progBody prog
    return $ prog { progBody = s }

