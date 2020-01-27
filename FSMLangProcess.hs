{-# LANGUAGE TemplateHaskell, TupleSections #-}
module FSMLangProcess where

import FSMLang
import FSMFreeVars
import Prelude
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Lens
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

{-
data LifterState = LifterState {
    lifterFunEnv :: M.Map TH.Name TH.Name,
    lifterFuns :: M.Map TH.Name (TH.Pat, Stmt)
}

$(makeLenses ''LifterState)

lambdaLift (SSeq s1 s2) = SSeq <$> lambdaLift s1 <*> lambdaLift s2
lambdaLift s = return s
-}

refreshName :: (THS.Quasi m, MonadTrans t) => TH.Name -> t m TH.Name
refreshName n = lift $ THS.qNewName $ TH.nameBase n

refreshPat :: (THS.Quasi m, MonadTrans t, Monad (t m)) => TH.Pat -> t m (TH.Pat, M.Map TH.Name TH.Name)
refreshPat p = runWriterT (f p) where
    f p@(TH.LitP _) = return p
    f   (TH.VarP v) = do
        v' <- lift $ refreshName v
        writer (TH.VarP v', M.singleton v v')
    f   (TH.TupP ps) = TH.TupP <$> mapM f ps
    f   (TH.UnboxedTupP ps) = TH.UnboxedTupP <$> mapM f ps
    f   (TH.UnboxedSumP p x y) = TH.UnboxedSumP <$> f p <*> pure x <*> pure y
    f   (TH.ConP n ps) = TH.ConP n <$> mapM f ps
    f   (TH.InfixP p1 n p2) = TH.InfixP <$> f p1 <*> pure n <*> f p2
    f   (TH.UInfixP p1 n p2) = TH.UInfixP <$> f p1 <*> pure n <*> f p2
    f   (TH.ParensP p) = TH.ParensP <$> f p
    f   (TH.TildeP p) = TH.TildeP <$> f p
    f   (TH.BangP p) = TH.BangP <$> f p
    f   (TH.AsP v p) = do
        v' <- lift $ refreshName v
        p' <- f p
        writer (TH.AsP v' p', M.singleton v v')
    f p@(TH.WildP) = return p
    f   (TH.RecP n fps) = TH.RecP n <$> forM fps (\(n, p) -> (n,) <$> f p)
    f   (TH.ListP ps) = TH.ListP <$> mapM f ps
    f   (TH.SigP p t) = TH.SigP <$> f p <*> pure t
    f   (TH.ViewP e p) = TH.ViewP e <$> f p

simpleStmt SNop = True
simpleStmt (SRet (VCall _ _)) = True
simpleStmt _ = False

cutBlocksStmt :: (THS.Quasi m, MonadState FunMap (t m), MonadTrans t) => S.Set TH.Name -> TH.Name -> Stmt -> Stmt -> t m Stmt
cutBlocksStmt fv n SNop s' = return s'
cutBlocksStmt fv n (SRet vs) s' = return $ SRet vs
cutBlocksStmt fv n (SBlock []) s' = return s'
cutBlocksStmt fv n (SBlock [s]) s' = cutBlocksStmt fv n s s'
cutBlocksStmt fv n (SBlock (s:ss)) s' = do
    s'' <- cutBlocksStmt fv n (SBlock ss) s'
    cutBlocksStmt fv n s s''
cutBlocksStmt fv n (SEmit e) s' | simpleStmt s' =
    return $ SBlock [SEmit e, s']
cutBlocksStmt fv n (SIf e st sf) s' | simpleStmt s' =
    SIf e <$> cutBlocksStmt fv n st s' <*> cutBlocksStmt fv n sf s'
cutBlocksStmt fv n (SCase e cs) s' | simpleStmt s' =
    SCase e <$> mapM cf cs where
        cf (p, s) = do
            (p', su) <- refreshPat p
            (p',) <$> cutBlocksStmt fv n (renameStmt su s) s'
cutBlocksStmt fv n (SLet ln vs@(VExp e) s) s' | simpleStmt s' = do
    ln' <- refreshName ln
    SLet ln' vs <$> cutBlocksStmt fv n (renameStmt (M.singleton ln ln') s) s'
cutBlocksStmt fv n s s' = do
    let vs = S.toList $ freeVarsStmt s' `S.difference` fv
    n' <- refreshName n
    modify $ M.insert n' (TH.TupP $ map TH.VarP vs, s')
    cutBlocksStmt fv n s (SRet (VCall n' (TH.TupE $ map TH.VarE vs)))

cutBlocks :: THS.Quasi m => NProg -> m NProg
cutBlocks (NProg is fs f1 e1) = do
    let fvs = freeVarsStmt $ SFun fs SNop
    fs' <- flip execStateT M.empty $ forM_ (M.toList fs) $ \(n, (p, s)) -> do
        s' <- cutBlocksStmt fvs n s SNop
        modify $ M.insert n (p, s')
    return $ NProg is fs' f1 e1

removeEpsilonStmt fs s@SNop = return s
removeEpsilonStmt fs s@(SEmit _) = return s
removeEpsilonStmt fs   (SIf e st sf) = SIf e <$> removeEpsilonStmt fs st <*> removeEpsilonStmt fs sf
removeEpsilonStmt fs   (SLet ln vs s) = SLet ln vs <$> removeEpsilonStmt fs s
removeEpsilonStmt fs   (SCase e cs) = SCase e <$> mapM cf cs where
    cf (p, s) = (p,) <$> removeEpsilonStmt fs s
removeEpsilonStmt fs s@(SBlock [SEmit e,SRet (VCall f e')]) = removeEpsilonFrom fs f >> return s
removeEpsilonStmt fs   (SRet (VCall f e)) = SCase e <$> (return . (p,) <$> removeEpsilonStmt fs s) where
    Just (p, s) = M.lookup f fs

removeEpsilonFrom fs f = do
    b <- gets (M.member f)
    unless b $ do
        modify $ M.insert f (p, SNop)
        s' <- removeEpsilonStmt fs s
        modify $ M.insert f (p, s')
    where Just (p, s) = M.lookup f fs

removeEpsilon :: NProg -> NProg
removeEpsilon (NProg is fs f1 e1) = NProg is fs' f1 e1
    where fs' = flip execState M.empty $ removeEpsilonFrom fs f1

