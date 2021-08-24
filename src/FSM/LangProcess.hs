{-# LANGUAGE TemplateHaskell, TupleSections, GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleContexts #-}
module FSM.LangProcess where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Lens
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import Debug.Trace

{-
data LifterState = LifterState {
    lifterFunEnv :: M.Map TH.Name TH.Name,
    lifterFuns :: M.Map TH.Name (TH.Pat, Stmt)
}

$(makeLenses ''LifterState)

-}

lambdaLiftStmt   (SLet ln vs s) = SLet ln <$> lambdaLiftVStmt vs <*> lambdaLiftStmt s
lambdaLiftStmt s@(SEmit _) = return s
lambdaLiftStmt   (SRet vs) = SRet <$> lambdaLiftVStmt vs
lambdaLiftStmt   (SBlock ss) = SBlock <$> mapM lambdaLiftStmt ss
lambdaLiftStmt   (SIf e s1 s2) = SIf e <$> lambdaLiftStmt s1 <*> lambdaLiftStmt s2
lambdaLiftStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> lambdaLiftStmt s) cs
lambdaLiftStmt s@(SNop) = return s
lambdaLiftStmt   (SFun fm s) = lambdaLiftStmt s -- TODO function env and state

lambdaLiftVStmt vs@(VExp _) = return vs
lambdaLiftVStmt    (VCall n e) = undefined

-- Unique generator

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance Monad m => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

instance (Monoid s, MonadUnique m) => MonadUnique (WriterT s m) where
    fresh = lift fresh

instance MonadUnique m => MonadUnique (StateT s m) where
    fresh = lift fresh

instance MonadUnique m => MonadUnique (ReaderT s m) where
    fresh = lift fresh

evalUniqueT (UniqueT s) = evalStateT s 0

-- Name refreshing

class Monad m => MonadRefresh m where
    makeName :: String -> m TH.Name

instance MonadRefresh TH.Q where
    makeName = TH.newName 

instance (Monoid s, MonadRefresh m) => MonadRefresh (WriterT s m) where
    makeName = lift . makeName

instance MonadRefresh m => MonadRefresh (StateT s m) where
    makeName = lift . makeName

instance MonadRefresh m => MonadRefresh (ReaderT s m) where
    makeName = lift . makeName

instance MonadRefresh m => MonadRefresh (UniqueT m) where
    makeName = lift . makeName

makeSeqName n = do
    a <- fresh
    makeName $ n ++ show a

refreshName n = makeName $ TH.nameBase n
refreshNameWithPrefix p n = makeName $ p ++ TH.nameBase n

refreshPat :: MonadRefresh m => TH.Pat -> m (TH.Pat, M.Map TH.Name TH.Name)
refreshPat p = runWriterT (f p) where
    f p@(TH.LitP _) = return p
    f   (TH.VarP v) = do
        v' <- refreshName v
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
        v' <- refreshName v
        p' <- f p
        writer (TH.AsP v' p', M.singleton v v')
    f p@(TH.WildP) = return p
    f   (TH.RecP n fps) = TH.RecP n <$> forM fps (\(n, p) -> (n,) <$> f p)
    f   (TH.ListP ps) = TH.ListP <$> mapM f ps
    f   (TH.SigP p t) = TH.SigP <$> f p <*> pure t
    f   (TH.ViewP e p) = TH.ViewP e <$> f p

simpleStmt SNop = True
simpleStmt (SRet _) = True
simpleStmt _ = False

-- Sorta-kinda CPS transformation

-- TODO: fix free vars handling

data CBData = CBData {
    cbDataFreeVars :: S.Set TH.Name,
    cbDataName :: TH.Name
}

$(makeLenses ''CBData)

tupE [x] = x
tupE xs = TH.TupE . map Just $ xs

tupP [x] = x
tupP xs = TH.TupP xs

makeCont s = do
    CBData fv n <- ask
    let vs = S.toList $ freeVarsStmt s `S.difference` fv
    n' <- refreshName n
    modify $ M.insert n' (tupP $ map TH.VarP vs, s)
    return $ SRet (VCall n' (tupE $ map TH.VarE vs))

cutBlocksStmt :: (MonadRefresh m, MonadState FunMap m, MonadReader CBData m) => Stmt -> Stmt -> m Stmt
cutBlocksStmt SNop s' = return s'
cutBlocksStmt (SRet vs) s' = return $ SRet vs
cutBlocksStmt (SBlock []) s' = return s'
cutBlocksStmt (SBlock [s]) s' = cutBlocksStmt s s'
cutBlocksStmt (SBlock (s:ss)) s' = do
    s'' <- cutBlocksStmt (SBlock ss) s'
    cutBlocksStmt s s''
cutBlocksStmt (SEmit e) s' | simpleStmt s' = 
    return $ SBlock [SEmit e, s']
cutBlocksStmt (SIf e st sf) s' | simpleStmt s' = 
    SIf e <$> cutBlocksStmt st s' <*> cutBlocksStmt sf s'
cutBlocksStmt (SCase e cs) s' | simpleStmt s' = 
    SCase e <$> mapM cf cs where
        cf (p, s) = do
            (p', su) <- refreshPat p
            (p',) <$> cutBlocksStmt (renameStmt su s) s'
cutBlocksStmt (SLet ln vs@(VExp _) s) s' | simpleStmt s' = do
    ln' <- refreshName ln
    SLet ln' vs <$> cutBlocksStmt (renameStmt (M.singleton ln ln') s) s'
cutBlocksStmt (SLet ln vs@(VCall _ _) s) s' = do
    ln' <- refreshName ln
    s'' <- cutBlocksStmt (renameStmt (M.singleton ln ln') s) s'
    s''' <- makeCont s''
    return $ SLet ln' vs s'''
cutBlocksStmt s s' = do
    s'' <- makeCont s'
    cutBlocksStmt s s''

cutBlocks :: MonadRefresh m => NProg -> m NProg
cutBlocks (NProg n t ps is fs f1 e1 cs) = do
    let fvs = freeVarsStmt $ SFun fs SNop
    fs' <- flip execStateT M.empty $ forM_ (M.toList fs) $ \(n, (p, s)) -> do
        s' <- flip runReaderT (CBData fvs n) $ cutBlocksStmt s (SRet (VExp $ tupE []))
        modify $ M.insert n (p, s')
    return $ NProg n t ps is fs' f1 e1 cs

-- Eliminate epsilon transitions

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
removeEpsilon (NProg n t ps is fs f1 e1 cs) = NProg n t ps is fs' f1 e1 cs
    where fs' = flip execState M.empty $ removeEpsilonFrom fs f1

-- Call graph calculation

data CGEdge = CGEdge {
    cgEdgeSrc :: TH.Name,
    cgEdgeDst :: TH.Name,
    cgEdgeTail :: Bool
}

type CG = [CGEdge]

callGraph :: FunMap -> CG
callGraph fs = M.toList fs >>= \(n, (_, s)) -> callGraphStmt n s

callGraphStmt :: TH.Name -> Stmt -> CG
callGraphStmt n SNop = mzero
callGraphStmt n (SEmit _) = mzero
callGraphStmt n (SVar _ vs s) = callGraphVStmt n False vs `mplus` callGraphStmt n s
callGraphStmt n (SLet _ vs s) = callGraphVStmt n False vs `mplus` callGraphStmt n s
callGraphStmt n (SAssign _ _) = mzero
callGraphStmt n (SRet vs) = callGraphVStmt n True vs
callGraphStmt n (SBlock ss) = callGraphStmt n =<< ss
callGraphStmt n (SIf _ st sf) = callGraphStmt n st `mplus` callGraphStmt n sf
callGraphStmt n (SCase _ cs) = callGraphStmt n =<< map snd cs

callGraphVStmt :: TH.Name -> Bool -> VStmt -> CG
callGraphVStmt n t (VExp _) = mzero
callGraphVStmt n t (VCall n' _) = return $ CGEdge n n' t

-- Returning functions calculation

returningFuns :: FunMap -> S.Set TH.Name
returningFuns fs = saturateSet (flip (M.findWithDefault S.empty) tailCalled) directRet
    where
    directRet = S.fromList [n | (n, (_, s)) <- M.toList fs, isReturningStmt s ]
    tailCalled = M.fromListWith S.union $ map (\e -> (cgEdgeDst e, S.singleton $ cgEdgeSrc e)) $ filter cgEdgeTail $ callGraph fs

isReturningStmt :: Stmt -> Bool
isReturningStmt SNop = False
isReturningStmt (SEmit _) = False
isReturningStmt (SVar _ _ s) = isReturningStmt s
isReturningStmt (SLet _ _ s) = isReturningStmt s
isReturningStmt (SAssign _ _) = False
isReturningStmt (SBlock ss) = or $ isReturningStmt <$> ss
isReturningStmt (SIf _ st sf) = isReturningStmt st || isReturningStmt sf
isReturningStmt (SCase _ cs) = or $ isReturningStmt <$> map snd cs
isReturningStmt (SRet (VExp _)) = True
isReturningStmt (SRet (VCall _ _)) = False

saturateSet :: Ord k => (k -> S.Set k) -> S.Set k -> S.Set k
saturateSet m s = g s S.empty where
    g s = foldr (.) id (map f (S.toList s))
    f n s | n `S.member` s = s
          | otherwise = g (m n) (S.insert n s)

-- Convert calls to returning functions to non-tail calls

deTailCall :: MonadRefresh m => NProg -> m NProg
deTailCall (NProg n t ps is fs f1 e1 cs) = do
    fs' <- mapM (\(p, s) -> (p,) <$> deTailCallStmt rf s) fs
    return $ NProg n t ps is fs' f1 e1 cs
    where
    rf = returningFuns fs

deTailCallStmt :: MonadRefresh m => S.Set TH.Name -> Stmt -> m Stmt
deTailCallStmt rf s@(SNop) = return s
deTailCallStmt rf s@(SEmit _) = return s
deTailCallStmt rf   (SVar n e s) = SVar n e <$> deTailCallStmt rf s
deTailCallStmt rf   (SLet n e s) = SLet n e <$> deTailCallStmt rf s
deTailCallStmt rf s@(SAssign _ _) = return s
deTailCallStmt rf   (SBlock ss) = SBlock <$> mapM (deTailCallStmt rf) ss
deTailCallStmt rf   (SIf e st sf) = SIf e <$> deTailCallStmt rf st <*> deTailCallStmt rf sf
deTailCallStmt rf   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> deTailCallStmt rf s) cs
deTailCallStmt rf s@(SRet (VExp _)) = return s
deTailCallStmt rf s@(SRet (VCall f e))
    | f `S.member` rf = do
        n <- refreshName f
        return $ SLet n (VCall f e) (SRet (VExp $ TH.VarE n))
    | otherwise = return s

-- Converting to tail calls

data TCData = TCData {
    tcDataCont :: TH.Name,
    tcDataType :: TH.Name,
    tcDataApply :: TH.Name,
    tcDataFreeVars :: S.Set TH.Name,
    tcDataName :: TH.Name
}

data ContData = ContData {
    contDataConName :: TH.Name,
    contDataCaller :: TH.Name,
    contDataCalled :: TH.Name,
    contDataResName :: TH.Name,
    contDataVars :: [TH.Name],
    contDataExp :: TH.Exp,
    contDataTgt :: ContTgt
}

data ContTgt = ContTgtFun TH.Name | ContTgtCont TH.Name

makeTailCallsStmt :: (MonadRefresh m, MonadReader TCData m, MonadWriter [ContData] m) => Stmt -> m Stmt
makeTailCallsStmt   (SLet n (VCall f e) (SRet (VExp e'))) = do
    TCData cfn _ an fvs fn <- ask
    let vs = S.toList $ freeVarsExp e' `S.difference` S.insert n fvs
    cn <- refreshNameWithPrefix "C" f
    tell [ContData cn fn f n vs e' (ContTgtCont an)]
    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
makeTailCallsStmt   (SLet n (VCall f e) (SRet (VCall f' e'))) = do
    TCData _ _ _ fvs fn <- ask
    let vs = S.toList $ freeVarsExp e' `S.difference` S.insert n fvs
    cn <- refreshNameWithPrefix "C" f
    tell [ContData cn fn f n vs e' (ContTgtFun f')]
    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE vs)]))
makeTailCallsStmt   (SLet n (VExp e) s) = SLet n (VExp e) <$> makeTailCallsStmt s -- TODO freevars
makeTailCallsStmt   (SIf e st sf) = SIf e <$> makeTailCallsStmt st <*> makeTailCallsStmt sf
makeTailCallsStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> makeTailCallsStmt s) cs
makeTailCallsStmt   (SRet (VExp e)) = SRet <$> (VCall <$> asks tcDataApply <*> ((\cfn -> tupE [e, TH.VarE cfn]) <$> asks tcDataCont))
makeTailCallsStmt s@(SRet (VCall _ _)) = return s
makeTailCallsStmt   (SBlock [SEmit e,s]) = (\s' -> SBlock [SEmit e, s']) <$> makeTailCallsStmt s

makeTailCalls :: MonadRefresh m => NProg -> m NProg
makeTailCalls (NProg n t ps is fs f1 e1 cs) = do
    let fvs = freeVarsStmt $ SFun fs SNop
    (fsd, cds) <- runWriterT $ forM (M.toList fs) $ \(n, (p, s)) -> do
        if n `S.member` rfs
        then do
            cfn <- makeName "c"
            ctn <- refreshNameWithPrefix "CT" n
            an <- refreshNameWithPrefix "ap" n
            s' <- flip runReaderT (TCData cfn ctn an fvs n) $ makeTailCallsStmt s
            return $ (Just (ctn, an), (n, (tupP [p, TH.VarP cfn], s')))
        else do
            s' <- flip runReaderT (TCData (error "cfn") (error "ctn") (error "an") fvs n) $ makeTailCallsStmt s
            return $ (Nothing, (n, (p, s')))
    let cdmap = M.fromListWith (++) $ map (contDataCalled &&& return) cds
    let rfsd = map (fromJust *** id) . filter (isJust . fst) $ fsd
    apfs <- mapM (apf cdmap) rfsd
    cdefs <- mapM (cdef cdmap) rfsd
    let fs' = M.fromList $ apfs ++ map snd fsd
    return $ NProg n t ps is fs' f1 e1 (M.unions cdefs `M.union` cs)
    where
    rfs = returningFuns fs
    apf cdmap ((ctn, an), (n, (p, s))) 
        | Just cds <- M.lookup n cdmap = do
            rn <- makeName "r"
            cfn <- makeName "c"
            cs <- forM cds $ \cd -> case contDataTgt cd of
                ContTgtFun fn ->
                    return (TH.ConP (contDataConName cd) [tupP $ map TH.VarP $ contDataVars cd],
                        SLet (contDataResName cd) (VExp $ TH.VarE rn) $ SRet (VCall fn (contDataExp cd)))
                ContTgtCont rap -> do
                    rcn <- makeName "rc"
                    return (TH.ConP (contDataConName cd) [tupP $ map TH.VarP $ rcn : contDataVars cd],
                        SLet (contDataResName cd) (VExp $ TH.VarE rn) $ SRet (VCall rap (tupE [contDataExp cd, TH.VarE rcn])))
            return (an, (tupP [TH.VarP rn, TH.VarP cfn], SCase (TH.VarE cfn) cs))
        | otherwise = return (an, (tupP [], SNop)) -- will be cleaned up anyway
    cdef cdmap ((ctn, an), (n, _)) 
        | Just cds <- M.lookup n cdmap = do
            cons <- forM cds $ \cd -> case contDataTgt cd of
                ContTgtFun _ ->
                    return (contDataConName cd, contDataVars cd)
                ContTgtCont _ -> do
                    rcn <- makeName "rc"
                    return (contDataConName cd, rcn : contDataVars cd)
            return $ M.singleton ctn $ M.fromList cons
        | otherwise = return $ M.empty
        

