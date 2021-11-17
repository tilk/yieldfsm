{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.CutBlocks(cutBlocks) where

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

makeCont :: (MonadReader CBData m, MonadRefresh m, MonadState FunMap m) => Stmt -> m Stmt
makeCont s = do
    CBData fv _ n <- ask
    let vs = S.toList $ freeVars s `S.difference` fv
    n' <- refreshName n
    modify $ M.insert n' (tupP $ map TH.VarP vs, s)
    return $ SRet (VCall n' (tupE $ map TH.VarE vs))

cutBlocksStmt :: (MonadRefresh m, MonadState FunMap m, MonadReader CBData m) => Stmt -> Stmt -> m Stmt
cutBlocksStmt SNop         s' = return s'
cutBlocksStmt (SRet vs)    _  = return $ SRet vs
cutBlocksStmt (SBlock [])  s' = return s'
cutBlocksStmt (SBlock [s]) s' = cutBlocksStmt s s'
cutBlocksStmt (SBlock (s:ss)) s' = do
    s'' <- cutBlocksStmt (SBlock ss) s'
    cutBlocksStmt s s''
cutBlocksStmt (SYield e) s' | not (emittingStmt s') = do -- TODO handling of inputs
    ivs <- asks cbDataInputVars
    if S.null $ freeVars s' `S.intersection` ivs
    then return $ SBlock [SYield e, s']
    else do
        s'' <- makeCont s'
        cutBlocksStmt (SYield e) s''
cutBlocksStmt (SIf e st sf) s' =
    SIf e <$> cutBlocksStmt st s' <*> cutBlocksStmt sf s'
cutBlocksStmt (SCase e cs) s' =
    SCase e <$> mapM cf cs where
        cf (p, s) = do
            (p', su) <- refreshPat p
            (p',) <$> cutBlocksStmt (rename su s) s'
cutBlocksStmt (SLet _ ln vs@(VExp _) s) s' = do
    ln' <- refreshName ln
    SLet VarLet ln' vs <$> cutBlocksStmt (renameSingle ln ln' s) s'
cutBlocksStmt (SLet _ ln vs@(VCall _ _) s) s' = do
    ln' <- refreshName ln
    s'' <- cutBlocksStmt (renameSingle ln ln' s) s'
    s''' <- makeCont s''
    return $ SLet VarLet ln' vs s'''
cutBlocksStmt (SAssign ln e) s' = do
    ln' <- refreshName ln
    return $ SLet VarLet ln' (VExp e) $ renameSingle ln ln' s'
cutBlocksStmt s s' = do
    s'' <- makeCont s'
    cutBlocksStmt s s''

cutBlocks :: MonadRefresh m => NProg -> m NProg
cutBlocks prog = do
    let fvs = freeVars $ SFun (nProgFuns prog) SNop
    let ivs = boundVars $ nProgInputs prog
    fs' <- flip execStateT M.empty $ forM_ (M.toList $ nProgFuns prog) $ \(n, (p, s)) -> do
        s' <- flip runReaderT (CBData fvs ivs n) $ cutBlocksStmt s (SRet (VExp $ tupE []))
        modify $ M.insert n (p, s')
    return $ prog { nProgFuns = fs' }

