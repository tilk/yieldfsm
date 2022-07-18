{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

This module defines the normalization translation.
-}
{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.Normalization(normalization) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh

data CBData = CBData {
    cbDataFreeVars :: S.Set TH.Name,
    cbDataInputVars :: S.Set TH.Name,
    cbDataName :: TH.Name
}

makeCont :: (MonadReader CBData m, MonadRefresh m, MonadState (FunMap LvlLowest) m) => Stmt LvlLowest -> m (Stmt LvlLowest)
makeCont s = do
    CBData fv _ n <- ask
    let vs = S.toList $ freeVars s `S.difference` fv
    n' <- refreshName n
    modify $ M.insert n' (tupP $ map TH.VarP vs, s)
    return $ SRet (VCall n' (tupE $ map TH.VarE vs))

isSimpleRet :: Stmt LvlLowest -> Bool
isSimpleRet (SRet (VExp e))    = isConstantExpr e
isSimpleRet (SRet (VCall _ e)) = isConstantExpr e
isSimpleRet _                  = False

normalizationStmt :: (MonadRefresh m, MonadState (FunMap LvlLowest) m, MonadReader CBData m) => Stmt LvlLifted -> Stmt LvlLowest -> m (Stmt LvlLowest)
normalizationStmt SNop         s' = return s'
normalizationStmt (SRet vs)    _  = return $ SRet vs
normalizationStmt (SBlock [])  s' = return s'
normalizationStmt (SBlock [s]) s' = normalizationStmt s s'
normalizationStmt (SBlock (s:ss)) s' = do
    s'' <- normalizationStmt (SBlock ss) s'
    normalizationStmt s s''
normalizationStmt (SYield e) s' | not (emittingStmt s') = do -- TODO handling of inputs
    ivs <- asks cbDataInputVars
    if S.null $ freeVars s' `S.intersection` ivs
    then return $ SYieldT e s'
    else do
        s'' <- makeCont s'
        normalizationStmt (SYield e) s''
normalizationStmt (SIf e st sf) s' | isSimpleRet s' =
    SIf e <$> normalizationStmt st s' <*> normalizationStmt sf s'
normalizationStmt (SCase e cs) s'  | isSimpleRet s' =
    SCase e <$> mapM cf cs where
        cf (p, s) = do
            (p', su) <- refreshPat p
            (p',) <$> normalizationStmt (rename su s) s'
normalizationStmt (SLet _ ln vs@(VExp _) s) s' = do
    ln' <- refreshName ln
    SLet VarLet ln' vs <$> normalizationStmt (renameSingle ln ln' s) s'
normalizationStmt (SLet _ ln vs@(VCall _ _) s) s' = do
    ln' <- refreshName ln
    s'' <- normalizationStmt (renameSingle ln ln' s) s'
    s''' <- makeCont s''
    return $ SLet VarLet ln' vs s'''
normalizationStmt (SAssign ln e) s' = do
    ln' <- refreshName ln
    return $ SLet VarLet ln' (VExp e) $ renameSingle ln ln' s'
normalizationStmt s s' = do
    s'' <- makeCont s'
    normalizationStmt s s''

{-|
Performs the normalization translation. In the normalized form, YieldFSM
statements cannot be combined with the semicolon operator,
and every function call is immediately followed by a return statement.
This translation also eliminates mutable variables.
Code duplication is avoided by creating additional functions.

Example:

> fun loop n_init:
>     var n = n_init
>     if n == 3:
>         n = 0
>     else:
>         n = n + 1
>     yield n
>     ret call loop n
> ret call loop 0

Is translated to:

> fun loop n_init:
>     let n1 = n_init
>     if n == 3:
>         let n2 = 0
>         ret call loop1 n2
>     else:
>         let n2 = n1 + 1
>         ret call loop1 n2
> fun loop1 n2:
>     yield n2
>     ret call loop n2
> ret call loop 0
-}
normalization :: MonadRefresh m => NProg LvlLifted -> m (NProg LvlLowest)
normalization prog = do
    let fvs = freeVarsFunMap $ nProgFuns prog
    let ivs = boundVars $ nProgInputs prog
    fs' <- flip execStateT M.empty $ forM_ (M.toList $ nProgFuns prog) $ \(n, (p, s)) -> do
        s' <- flip runReaderT (CBData fvs ivs n) $ normalizationStmt s (SRet (VExp $ tupE []))
        modify $ M.insert n (p, s')
    return $ prog { nProgFuns = fs' }

