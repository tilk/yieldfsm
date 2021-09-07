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
import Data.Key(mapWithKeyM, forWithKeyM)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
--import qualified Language.Haskell.TH.Syntax as THS

saturateSet :: Ord k => (k -> S.Set k) -> S.Set k -> S.Set k
saturateSet m = flip g S.empty where
    g s = foldr (.) id (map f (S.toList s))
    f n s | n `S.member` s = s
          | otherwise = g (m n) (S.insert n s)

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

evalUniqueT :: Monad m => UniqueT m a -> m a
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

makeSeqName :: (MonadUnique m, MonadRefresh m) => String -> m TH.Name
makeSeqName n = do
    a <- fresh
    makeName $ n ++ show a

refreshName :: MonadRefresh m => TH.Name -> m TH.Name
refreshName n = makeName $ TH.nameBase n

refreshNameWithPrefix :: MonadRefresh m => String -> TH.Name -> m TH.Name
refreshNameWithPrefix p n = makeName $ p ++ TH.nameBase n

refreshPat :: MonadRefresh m => TH.Pat -> m (TH.Pat, M.Map TH.Name TH.Name)
refreshPat = runWriterT . f where
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
    f   (TH.RecP n fps) = TH.RecP n <$> forM fps (\(n', p) -> (n',) <$> f p)
    f   (TH.ListP ps) = TH.ListP <$> mapM f ps
    f   (TH.SigP p t) = TH.SigP <$> f p <*> pure t
    f   (TH.ViewP e p) = TH.ViewP e <$> f p

tupE :: [TH.Exp] -> TH.Exp
tupE [x] = x
tupE xs = TH.TupE . map Just $ xs

tupP :: [TH.Pat] -> TH.Pat
tupP [x] = x
tupP xs = TH.TupP xs

data LLData = LLData {
    _llDataFreeVars :: S.Set TH.Name,
    _llDataEnv :: LLEnv
}

type LLEnv = M.Map TH.Name (TH.Name, [TH.Name])

$(makeLenses ''LLData)

lambdaLiftStmt :: (MonadRefresh m, MonadState FunMap m, MonadReader LLData m) => Stmt -> m Stmt
lambdaLiftStmt   (SLet t ln vs s) = do
    ln' <- refreshName ln
    SLet t ln' <$> lambdaLiftVStmt vs <*> lambdaLiftStmt (renameStmtSingle ln ln' s)
lambdaLiftStmt   (SAssign n vs) = SAssign n <$> lambdaLiftVStmt vs
lambdaLiftStmt s@(SEmit _) = return s
lambdaLiftStmt   (SRet vs) = SRet <$> lambdaLiftVStmt vs
lambdaLiftStmt   (SBlock ss) = SBlock <$> mapM lambdaLiftStmt ss
lambdaLiftStmt   (SIf e s1 s2) = SIf e <$> lambdaLiftStmt s1 <*> lambdaLiftStmt s2
lambdaLiftStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> lambdaLiftStmt s) cs
lambdaLiftStmt s@(SNop) = return s
lambdaLiftStmt   (SFun fm s) = do
    fvs <- view llDataFreeVars
    e <- forWithKeyM fm $ \n (p, s') -> (, S.elems $ freeVars s' `S.difference` fvs `underPat` freeVarsPat p) <$> refreshName n
    locally llDataEnv (M.union e) $ do
        fm' <- mapWithKeyM processFun fm
        modify $ M.union $ M.mapKeys (fst . fromJust . flip M.lookup e) fm'
        lambdaLiftStmt s
    where
    processFun n (p, s') = do
        (p', su) <- refreshPat p
        (,) <$> (tupP . (++ [p']) <$> views llDataEnv (map TH.VarP . snd . fromJust . M.lookup n)) <*> lambdaLiftStmt (renameStmt su s')

lambdaLiftVStmt :: (Monad m, MonadReader LLData m) => VStmt -> m VStmt
lambdaLiftVStmt vs@(VExp _) = return vs
lambdaLiftVStmt    (VCall n e) = do
    (n', vs) <- views llDataEnv (fromJust . M.lookup n)
    return $ VCall n' $ tupE $ map TH.VarE vs ++ [e]

lambdaLift :: MonadRefresh m => Prog -> m NProg
lambdaLift prog = do
    (s, fm) <- flip runStateT M.empty $ flip runReaderT (LLData (freeVars $ progBody prog) M.empty) $ lambdaLiftStmt (progBody prog)
    case s of
        SRet (VCall f e) -> return $ NProg (progName prog) (progType prog) (progParams prog) (progInputs prog) fm f e M.empty
        _ -> do
            f <- refreshName $ TH.mkName "init"
            return $ NProg (progName prog) (progType prog) (progParams prog) (progInputs prog) (M.insert f (tupP [], s) fm) f (tupE []) M.empty

-- Sorta-kinda CPS transformation

-- TODO: fix free vars handling

data CBData = CBData {
    cbDataFreeVars :: S.Set TH.Name,
    cbDataName :: TH.Name
}

emittingStmt :: Stmt -> Bool
emittingStmt (SBlock [SEmit _, _]) = True
emittingStmt SNop = False
emittingStmt (SRet _) = False
emittingStmt (SIf _ st sf) = emittingStmt st || emittingStmt sf
emittingStmt (SCase _ cs) = any (emittingStmt . snd) cs
emittingStmt (SLet _ _ _ s) = emittingStmt s

makeCont :: (MonadReader CBData m, MonadRefresh m, MonadState FunMap m) => Stmt -> m Stmt
makeCont s = do
    CBData fv n <- ask
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
cutBlocksStmt (SEmit e) s' | not (emittingStmt s') = 
    return $ SBlock [SEmit e, s']
cutBlocksStmt (SIf e st sf) s' = 
    SIf e <$> cutBlocksStmt st s' <*> cutBlocksStmt sf s'
cutBlocksStmt (SCase e cs) s' = 
    SCase e <$> mapM cf cs where
        cf (p, s) = do
            (p', su) <- refreshPat p
            (p',) <$> cutBlocksStmt (renameStmt su s) s'
cutBlocksStmt (SLet _ ln vs@(VExp _) s) s' = do
    ln' <- refreshName ln
    SLet VarLet ln' vs <$> cutBlocksStmt (renameStmtSingle ln ln' s) s'
cutBlocksStmt (SLet _ ln vs@(VCall _ _) s) s' = do
    ln' <- refreshName ln
    s'' <- cutBlocksStmt (renameStmtSingle ln ln' s) s'
    s''' <- makeCont s''
    return $ SLet VarLet ln' vs s'''
cutBlocksStmt (SAssign ln vs) s' = do
    ln' <- refreshName ln
    return $ SLet VarLet ln' vs $ renameStmtSingle ln ln' s'
cutBlocksStmt s s' = do
    s'' <- makeCont s'
    cutBlocksStmt s s''

cutBlocks :: MonadRefresh m => NProg -> m NProg
cutBlocks prog = do
    let fvs = freeVars $ SFun (nProgFuns prog) SNop
    fs' <- flip execStateT M.empty $ forM_ (M.toList $ nProgFuns prog) $ \(n, (p, s)) -> do
        s' <- flip runReaderT (CBData fvs n) $ cutBlocksStmt s (SRet (VExp $ tupE []))
        modify $ M.insert n (p, s')
    return $ prog { nProgFuns = fs' }

-- Eliminate epsilon transitions

data REData = REData {
    _reDataFunMap :: FunMap,
    _reDataEmitted :: Bool
}

$(makeLenses ''REData)

removeEpsilonStmt :: (MonadRefresh f, MonadReader REData f, MonadState (M.Map TH.Name (TH.Pat, Stmt)) f) =>
                     Stmt -> f Stmt
removeEpsilonStmt s@SNop = return s
removeEpsilonStmt s@(SEmit _) = return s
removeEpsilonStmt   (SIf e st sf) = SIf e <$> removeEpsilonStmt st <*> removeEpsilonStmt sf
removeEpsilonStmt   (SLet t ln vs s) = SLet t ln vs <$> removeEpsilonStmt s
removeEpsilonStmt   (SCase e cs) = SCase e <$> mapM cf cs where
    cf (p, s) = (p,) <$> removeEpsilonStmt s
--removeEpsilonStmt s@(SBlock [SEmit _, SRet (VCall f _)]) = removeEpsilonFrom f >> return s
removeEpsilonStmt   (SBlock [SEmit e, s]) = (\s' -> SBlock [SEmit e, s']) <$> locally reDataEmitted (const True) (removeEpsilonStmt s)
removeEpsilonStmt s@(SRet (VCall f e)) = do
    em <- view reDataEmitted
    if em then removeEpsilonFrom f >> return s
    else do
        (p, s') <- views reDataFunMap $ fromJust . M.lookup f
        (p', su) <- refreshPat p
        SCase e <$> (return . (p',) <$> removeEpsilonStmt (renameStmt su s'))
removeEpsilonStmt s = error $ "removeEpsilonStmt statement not in tree form: " ++ show s

removeEpsilonFrom :: (MonadRefresh f, MonadReader REData f, MonadState (M.Map TH.Name (TH.Pat, Stmt)) f) =>
                     TH.Name -> f ()
removeEpsilonFrom f = do
    (p, s) <- views reDataFunMap $ fromJust . M.lookup f
    b <- gets (M.member f)
    unless b $ do
        modify $ M.insert f (p, SNop)
        s' <- locally reDataEmitted (const False) $ removeEpsilonStmt s
        modify $ M.insert f (p, s')

removeEpsilon :: MonadRefresh m => NProg -> m NProg
removeEpsilon prog = do
    fs' <- flip execStateT M.empty $ flip runReaderT (REData (nProgFuns prog) False) $ removeEpsilonFrom (nProgInit prog)
    return $ prog { nProgFuns = fs' }

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
callGraphStmt _ SNop = mzero
callGraphStmt _ (SEmit _) = mzero
callGraphStmt n (SLet _ _ vs s) = callGraphVStmt n False vs `mplus` callGraphStmt n s
callGraphStmt _ (SAssign _ _) = mzero
callGraphStmt n (SRet vs) = callGraphVStmt n True vs
callGraphStmt n (SBlock ss) = callGraphStmt n =<< ss
callGraphStmt n (SIf _ st sf) = callGraphStmt n st `mplus` callGraphStmt n sf
callGraphStmt n (SCase _ cs) = callGraphStmt n =<< map snd cs
callGraphStmt _ s@(SFun _ _) = error $ "callGraphStmt statement not in lambda-lifted form: " ++ show s

callGraphVStmt :: TH.Name -> Bool -> VStmt -> CG
callGraphVStmt _ _ (VExp _) = mzero
callGraphVStmt n t (VCall n' _) = return $ CGEdge n n' t

-- Returning functions calculation

returningFuns :: FunMap -> S.Set TH.Name -> S.Set TH.Name
returningFuns fs rf = saturateSet (flip (M.findWithDefault S.empty) tailCalled) directRet
    where
    directRet = S.fromList [n | (n, (_, s)) <- M.toList fs, isReturningStmt s ] `S.union` rf
    tailCalled = M.fromListWith S.union $ map (\e -> (cgEdgeDst e, S.singleton $ cgEdgeSrc e)) $ filter cgEdgeTail $ callGraph fs

updateRetFuns :: FunMap -> S.Set TH.Name -> S.Set TH.Name
updateRetFuns fs rf = returningFuns fs (rf `S.difference` M.keysSet fs)

isReturningStmt :: Stmt -> Bool
isReturningStmt SNop = False
isReturningStmt (SEmit _) = False
isReturningStmt (SLet _ _ _ s) = isReturningStmt s
isReturningStmt (SAssign _ _) = False
isReturningStmt (SBlock ss) = or $ isReturningStmt <$> ss
isReturningStmt (SIf _ st sf) = isReturningStmt st || isReturningStmt sf
isReturningStmt (SCase _ cs) = or $ isReturningStmt <$> map snd cs
isReturningStmt (SRet (VExp _)) = True
isReturningStmt (SRet (VCall _ _)) = False
isReturningStmt (SFun _ s) = isReturningStmt s

-- Convert calls to returning functions to non-tail calls

deTailCall :: MonadRefresh m => Prog -> m Prog
deTailCall prog = do
    s' <- deTailCallStmt S.empty $ progBody prog
    return $ prog { progBody = s' }

deTailCallStmt :: MonadRefresh m => S.Set TH.Name -> Stmt -> m Stmt
deTailCallStmt _  s@(SNop) = return s
deTailCallStmt _  s@(SEmit _) = return s
deTailCallStmt rf   (SLet t n e s) = SLet t n e <$> deTailCallStmt rf s
deTailCallStmt _  s@(SAssign _ _) = return s
deTailCallStmt rf   (SBlock ss) = SBlock <$> mapM (deTailCallStmt rf) ss
deTailCallStmt rf   (SIf e st sf) = SIf e <$> deTailCallStmt rf st <*> deTailCallStmt rf sf
deTailCallStmt rf   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> deTailCallStmt rf s) cs
deTailCallStmt _  s@(SRet (VExp _)) = return s
deTailCallStmt rf s@(SRet (VCall f e))
    | f `S.member` rf = do
        n <- refreshName f
        return $ SLet VarLet  n (VCall f e) (SRet (VExp $ TH.VarE n))
    | otherwise = return s
deTailCallStmt rf   (SFun fs s) = SFun <$> mapM (\(p, s') -> (p,) <$> deTailCallStmt rf' s') fs <*> deTailCallStmt rf' s
    where rf' = updateRetFuns fs rf

-- Make mutable variables local

data LVData = LVData {
    _lvDataReturning :: Bool,
    _lvDataRetFuns :: S.Set TH.Name,
    _lvDataMutVars :: [TH.Name],
    _lvDataEnvVars :: [TH.Name],
    _lvDataFunVars :: M.Map TH.Name [TH.Name]
}

$(makeLenses ''LVData)

makeLocalVars :: MonadRefresh m => Prog -> m Prog
makeLocalVars prog = do
    s' <- flip runReaderT (LVData False S.empty [] [] M.empty) $ makeLocalVarsStmt $ progBody prog
    return $ prog { progBody = s' }

makeLocalVarsStmt :: (MonadRefresh m, MonadReader LVData m) => Stmt -> m Stmt
makeLocalVarsStmt SNop = return SNop
makeLocalVarsStmt (SEmit e) = return $ SEmit e
makeLocalVarsStmt (SBlock ss) = SBlock <$> mapM makeLocalVarsStmt ss
makeLocalVarsStmt (SIf e st sf) = SIf e <$> makeLocalVarsStmt st <*> makeLocalVarsStmt sf
makeLocalVarsStmt (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> makeLocalVarsStmt s) cs
makeLocalVarsStmt (SLet VarLet n vs s) = makeLocalVarsVStmt vs $ \e -> SLet VarLet n (VExp e) <$> makeLocalVarsStmt s
makeLocalVarsStmt (SLet VarMut n vs s) = makeLocalVarsVStmt vs $ \e -> do
    n' <- refreshName n
    SLet VarMut n' (VExp e) <$> locally lvDataMutVars (n':) (makeLocalVarsStmt $ renameStmtSingle n n' s)
makeLocalVarsStmt (SAssign n vs) = makeLocalVarsVStmt vs (return . SAssign n . VExp)
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
    locally lvDataFunVars (M.map (const mvs0) fs `M.union`) $ locally lvDataRetFuns (updateRetFuns fs) $ do
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

makeLocalVarsVStmt :: (MonadRefresh m, MonadReader LVData m) => VStmt -> (TH.Exp -> m Stmt) -> m Stmt
makeLocalVarsVStmt (VExp e) c = c e
makeLocalVarsVStmt (VCall f e) c = do
    n <- makeName "rt"
    rn <- makeName "r"
    mvs <- views lvDataFunVars $ fromJust . M.lookup f
    vns <- replicateM (length mvs) $ makeName "v"
    s <- c $ TH.VarE rn
    return $ SLet VarLet n (VCall f $ tupE $ e:map TH.VarE mvs) $ 
        SCase (TH.VarE n) [(tupP $ map TH.VarP $ rn:vns, sBlock $ zipWith SAssign mvs (map (VExp . TH.VarE) vns) ++ [s])]

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

makeTailCallsStmt :: (MonadRefresh m, MonadUnique m, MonadReader TCData m, MonadWriter [ContData] m) => Stmt -> m Stmt
makeTailCallsStmt   (SLet VarLet n (VCall f e) (SRet (VExp e'))) = do
    TCData cfn _ an fvs fn <- ask
    let vs = S.toList $ freeVars e' `S.difference` S.insert n fvs
    cn <- makeSeqName $ "C" ++ TH.nameBase f
    tell [ContData cn fn f n vs e' (ContTgtCont an)]
    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
makeTailCallsStmt   (SLet VarLet n (VCall f e) (SRet (VCall f' e'))) = do
    TCData _ _ _ fvs fn <- ask
    let vs = S.toList $ freeVars e' `S.difference` S.insert n fvs
    cn <- makeSeqName $ "C" ++ TH.nameBase f
    tell [ContData cn fn f n vs e' (ContTgtFun f')]
    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE vs)]))
makeTailCallsStmt   (SLet VarLet n (VExp e) s) = SLet VarLet n (VExp e) <$> makeTailCallsStmt s -- TODO freevars
makeTailCallsStmt   (SIf e st sf) = SIf e <$> makeTailCallsStmt st <*> makeTailCallsStmt sf
makeTailCallsStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> makeTailCallsStmt s) cs
makeTailCallsStmt   (SRet (VExp e)) = SRet <$> (VCall <$> asks tcDataApply <*> ((\cfn -> tupE [e, TH.VarE cfn]) <$> asks tcDataCont))
makeTailCallsStmt s@(SRet (VCall _ _)) = return s
makeTailCallsStmt   (SBlock [SEmit e,s]) = (\s' -> SBlock [SEmit e, s']) <$> makeTailCallsStmt s
makeTailCallsStmt s = error $ "makeTailCallsStmt statement not in tree form: " ++ show s

makeTailCalls :: MonadRefresh m => NProg -> m NProg
makeTailCalls prog = evalUniqueT $ do
    let fvs = freeVars $ SFun (nProgFuns prog) SNop
    (fsd, cds) <- runWriterT $ forM (M.toList $ nProgFuns prog) $ \(n, (p, s)) -> do
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
    return $ prog { nProgFuns = fs', nProgConts = M.unions cdefs `M.union` nProgConts prog }
    where
    rfs = returningFuns (nProgFuns prog) S.empty
    apf cdmap ((_ctn, an), (n, (_p, _s))) 
        | Just cds <- M.lookup n cdmap = do
            rn <- makeName "r"
            cfn <- makeName "c"
            cs <- forM cds $ \cd -> case contDataTgt cd of
                ContTgtFun fn ->
                    return (TH.ConP (contDataConName cd) [tupP $ map TH.VarP $ contDataVars cd],
                        SLet VarLet (contDataResName cd) (VExp $ TH.VarE rn) $ SRet (VCall fn (contDataExp cd)))
                ContTgtCont rap -> do
                    rcn <- makeName "rc"
                    return (TH.ConP (contDataConName cd) [tupP $ map TH.VarP $ rcn : contDataVars cd],
                        SLet VarLet (contDataResName cd) (VExp $ TH.VarE rn) $ SRet (VCall rap (tupE [contDataExp cd, TH.VarE rcn])))
            return (an, (tupP [TH.VarP rn, TH.VarP cfn], SCase (TH.VarE cfn) cs))
        | otherwise = return (an, (tupP [], SNop)) -- will be cleaned up anyway
    cdef cdmap ((ctn, _an), (n, _)) 
        | Just cds <- M.lookup n cdmap = do
            cs <- forM cds $ \cd -> case contDataTgt cd of
                ContTgtFun _ ->
                    return (contDataConName cd, contDataVars cd)
                ContTgtCont _ -> do
                    rcn <- makeName "rc"
                    return (contDataConName cd, rcn : contDataVars cd)
            return $ M.singleton ctn $ M.fromList cs
        | otherwise = return $ M.empty
        
-- Constant propagation

hasAssigns :: TH.Name -> Stmt -> Bool
hasAssigns n (SIf _ st sf) = hasAssigns n st || hasAssigns n sf
hasAssigns n (SCase _ cs) = any (hasAssigns n . snd) cs
hasAssigns _ (SRet _) = False
hasAssigns n (SBlock ss) = any (hasAssigns n) ss
hasAssigns _ (SEmit _) = False
hasAssigns n (SAssign n' _) = n == n'
hasAssigns n (SFun fs s) = hasAssigns n s || any (\(p, s') -> not (n `S.member` patBound (freeVarsPat p)) && hasAssigns n s') fs
hasAssigns _ SNop = False
hasAssigns n (SLet _ n' _ s) = n /= n' && hasAssigns n s

propagateConstantsStmt :: Stmt -> Stmt
propagateConstantsStmt   (SIf e st sf) = SIf e (propagateConstantsStmt st) (propagateConstantsStmt sf)
propagateConstantsStmt   (SCase e cs) = SCase e (map (id *** propagateConstantsStmt) cs)
propagateConstantsStmt s@(SRet _) = s
propagateConstantsStmt   (SBlock ss) = SBlock $ map propagateConstantsStmt ss
propagateConstantsStmt s@(SEmit _) = s
propagateConstantsStmt s@(SAssign _ _) = s
propagateConstantsStmt   (SFun fs s) = SFun (M.map (id *** propagateConstantsStmt) fs) (propagateConstantsStmt s)
propagateConstantsStmt s@SNop = s
propagateConstantsStmt   (SLet t n vs s) 
    | VExp e <- vs, isConstantExpr e, canSubst t = propagateConstantsStmt $ substStmtSingle n e s
    | otherwise = SLet t n vs $ propagateConstantsStmt s
    where
    canSubst VarLet = True
    canSubst VarMut = not $ hasAssigns n s

propagateConstants :: Prog -> Prog
propagateConstants prog = prog { progBody = propagateConstantsStmt $ progBody prog }

