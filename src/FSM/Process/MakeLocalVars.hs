{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

This module defines a transformation which ensures that all references
to mutable variables are local.
-}
{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.MakeLocalVars(makeLocalVars) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import Control.Lens
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import Data.Key(forWithKeyM)
import FSM.Util.MonadRefresh
import FSM.Process.ReturningFuns

data LVData = LVData {
    _lvDataReturning :: Bool,
    _lvDataRetFuns :: S.Set TH.Name,
    _lvDataMutVars :: [TH.Name],
    _lvDataEnvVars :: [TH.Name],
    _lvDataFunVars :: M.Map TH.Name [TH.Name]
}

$(makeLenses ''LVData)

{-|
Makes all references to mutable variables local - i.e. the mutable variable
is defined in the same function where it's used. To achieve that, it creates
local copies of non-local variables. They are initialized from new function
arguments, and their final values are returned in a tuple.

Example:

> var n = 0
> fun loop ():
>     fun f ():
>         n = n + 1
>         ret ()
>     let _ = call f ()
>     yield n
>     ret call loop ()
> ret call loop ()

Is translated to:

> var n = 0
> fun loop ((), n_init):
>     var n = n_init
>     fun f ((), n_init):
>         var n = n_init
>         n = n + 1
>         ret ((), n)
>     let rt = call f ((), n)
>     case rt
>     | (_, n_new):
>         n = n_new
>         yield n
>         ret call loop ((), n)
> ret call loop ((), n)
-}
makeLocalVars :: (IsDesugared l, WithAssign l, MonadRefresh m) => Prog l -> m (Prog l)
makeLocalVars prog = do
    s' <- flip runReaderT (LVData False (returningFuns $ progBody prog) [] [] M.empty) $ makeLocalVarsStmt $ progBody prog
    return $ prog { progBody = s' }

makeLocalVarsStmt :: (IsDesugared l, WithAssign l, MonadRefresh m, MonadReader LVData m) => Stmt l -> m (Stmt l)
makeLocalVarsStmt SNop = return SNop
makeLocalVarsStmt (SYield e) = return $ SYield e
makeLocalVarsStmt (SBlock ss) = SBlock <$> mapM makeLocalVarsStmt ss
makeLocalVarsStmt (SIf e st sf) = SIf e <$> makeLocalVarsStmt st <*> makeLocalVarsStmt sf
makeLocalVarsStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> makeLocalVarsStmt s) cs
makeLocalVarsStmt (SLet VarLet n vs s) = makeLocalVarsVStmt vs $ \e -> SLet VarLet n (VExp e) <$> makeLocalVarsStmt s
makeLocalVarsStmt (SLet VarMut n vs s) = makeLocalVarsVStmt vs $ \e -> do
    n' <- refreshName n
    SLet VarMut n' (VExp e) <$> locally lvDataMutVars (n':) (makeLocalVarsStmt $ renameSingle n n' s)
makeLocalVarsStmt (SAssign n e) = return $ SAssign n e
makeLocalVarsStmt (SRet vs) = do
    mvs <- view lvDataEnvVars
    mvs' <- mvsOf vs
    r <- view lvDataReturning
    if not r || mvs == mvs' then
        SRet <$> makeLocalVarsVStmtRet vs
    else
        makeLocalVarsVStmt vs (return . SRet . VExp . tupE . (:map TH.VarE mvs))
    where
    mvsOf (VExp _) = return []
    mvsOf (VCall f _) = views lvDataFunVars $ fromJust . M.lookup f
makeLocalVarsStmt (SFun fs s) = do
    mvs0 <- view lvDataMutVars -- TODO deduce actual variable usage
    locally lvDataFunVars (M.map (const mvs0) fs `M.union`) $ do
        fs' <- forWithKeyM fs $ \f (p, s') -> do
            r <- views lvDataRetFuns $ S.member f
            mvs <- views lvDataFunVars $ fromJust . M.lookup f
            vns <- replicateM (length mvs) $ makeName "v"
            let lets = foldr (.) id (zipWith (\mv vn -> SLet VarMut mv (VExp $ TH.VarE vn)) mvs vns)
            locally lvDataEnvVars (const mvs) $ locally lvDataReturning (const r) $
                (tupP $ p:map TH.VarP vns,) . lets <$> makeLocalVarsStmt s'
        SFun fs' <$> makeLocalVarsStmt s

makeLocalVarsVStmtRet :: (MonadRefresh m, MonadReader LVData m) => VStmt -> m VStmt
makeLocalVarsVStmtRet (VExp e) = return $ VExp e
makeLocalVarsVStmtRet (VCall f e) = do
    mvs <- views lvDataFunVars $ fromJust . M.lookup f
    return $ VCall f $ tupE $ e:map TH.VarE mvs

makeLocalVarsVStmt :: (IsDesugared l, WithAssign l, MonadRefresh m, MonadReader LVData m) => VStmt -> (TH.Exp -> m (Stmt l)) -> m (Stmt l)
makeLocalVarsVStmt (VExp e) c = c e
makeLocalVarsVStmt (VCall f e) c = do
    n <- makeName "rt"
    rn <- makeName "r"
    mvs <- views lvDataFunVars $ fromJust . M.lookup f
    vns <- replicateM (length mvs) $ makeName "v"
    s <- c $ TH.VarE rn
    return $ SLet VarLet n (VCall f $ tupE $ e:map TH.VarE mvs) $
        SCase (TH.VarE n) [(tupP $ map TH.VarP $ rn:vns, sBlockS $ zipWith SAssign mvs (map TH.VarE vns) ++ [s])]

