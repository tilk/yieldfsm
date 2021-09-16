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
import Data.Graph(stronglyConnComp, flattenSCC)
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

refreshSeqNameWithPrefix :: (MonadUnique m, MonadRefresh m) => String -> TH.Name -> m TH.Name
refreshSeqNameWithPrefix p n = makeSeqName $ p ++ TH.nameBase n

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

-- Refreshing function names

refreshFunctionsVStmt :: MonadRefresh m => M.Map TH.Name TH.Name -> VStmt -> m VStmt
refreshFunctionsVStmt _ (VExp e) = return $ VExp e
refreshFunctionsVStmt m (VCall f e) = return $ VCall (fromJust $ M.lookup f m) e

refreshFunctionsStmt :: MonadRefresh m => M.Map TH.Name TH.Name -> Stmt -> m Stmt
refreshFunctionsStmt _ SNop = return SNop
refreshFunctionsStmt _ (SEmit e) = return $ SEmit e
refreshFunctionsStmt m (SRet vs) = SRet <$> refreshFunctionsVStmt m vs
refreshFunctionsStmt m (SAssign n vs) = SAssign n <$> refreshFunctionsVStmt m vs
refreshFunctionsStmt m (SBlock ss) = SBlock <$> mapM (refreshFunctionsStmt m) ss
refreshFunctionsStmt m (SIf e st sf) = SIf e <$> refreshFunctionsStmt m st <*> refreshFunctionsStmt m sf
refreshFunctionsStmt m (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> refreshFunctionsStmt m s) cs
refreshFunctionsStmt m (SLet t ln vs s) = SLet t ln <$> refreshFunctionsVStmt m vs <*> refreshFunctionsStmt m s
refreshFunctionsStmt m (SFun fs s) = do
    m' <- (`M.union` m) <$> (forWithKeyM fs $ \f _ -> refreshName f)
    SFun <$> (M.fromList <$> forM (M.toList fs) (\(f, (p, s')) ->  (fromJust $ M.lookup f m', ) . (p,) <$> refreshFunctionsStmt m' s')) <*> refreshFunctionsStmt m' s

refreshFunctions :: MonadRefresh m => Prog -> m Prog
refreshFunctions prog = do
    s <- refreshFunctionsStmt M.empty $ progBody prog
    return $ prog { progBody = s }

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
    _reDataInputVars :: S.Set TH.Name,
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
removeEpsilonStmt   (SBlock [SEmit e, s]) = (\s' -> SBlock [SEmit e, s']) <$> locally reDataEmitted (const True) (removeEpsilonStmt s)
removeEpsilonStmt s@(SRet (VCall f e)) = do
    (p, s') <- views reDataFunMap $ fromJust . M.lookup f
    em <- view reDataEmitted
    ivs <- view reDataInputVars
    b <- gets (M.member f)
    if em && (b || emittingStmt s' || not (S.null $ S.intersection ivs $ freeVars s')) then removeEpsilonFrom f >> return s
    else do
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
    fs' <- flip execStateT M.empty $ flip runReaderT (REData (nProgFuns prog) (boundVars $ nProgInputs prog) False) $ removeEpsilonFrom (nProgInit prog)
    return $ prog { nProgFuns = fs' }

-- Call graph calculation

data CGEdge = CGEdge {
    cgEdgeSrc :: TH.Name,
    cgEdgeDst :: TH.Name,
    cgEdgeTail :: Bool
}

type CG = [CGEdge]

callGraph :: Stmt -> CG
callGraph s = callGraphStmt (TH.mkName "") s

callGraphFlat :: FunMap -> CG
callGraphFlat fs = callGraphFunMap (TH.mkName "") fs SNop

callGraphFunMap :: TH.Name -> FunMap -> Stmt -> CG
callGraphFunMap n fs s = (M.toList fs >>= \(n', (_, s')) -> callGraphStmt n' s') `mplus` callGraphStmt n s

callGraphStmt :: TH.Name -> Stmt -> CG
callGraphStmt _ SNop = mzero
callGraphStmt _ (SEmit _) = mzero
callGraphStmt n (SLet _ _ vs s) = callGraphVStmt n False vs `mplus` callGraphStmt n s
callGraphStmt _ (SAssign _ _) = mzero
callGraphStmt n (SRet vs) = callGraphVStmt n True vs
callGraphStmt n (SBlock ss) = callGraphStmt n =<< ss
callGraphStmt n (SIf _ st sf) = callGraphStmt n st `mplus` callGraphStmt n sf
callGraphStmt n (SCase _ cs) = callGraphStmt n =<< map snd cs
callGraphStmt n (SFun fs s) = callGraphFunMap n fs s

callGraphVStmt :: TH.Name -> Bool -> VStmt -> CG
callGraphVStmt _ _ (VExp _) = mzero
callGraphVStmt n t (VCall n' _) = return $ CGEdge n n' t

-- Returning functions calculation

directRet :: Stmt -> S.Set TH.Name
directRet SNop = S.empty
directRet (SEmit _) = S.empty
directRet (SLet _ _ _ s) = directRet s
directRet (SAssign _ _) = S.empty
directRet (SRet _) = S.empty
directRet (SBlock ss) = S.unions $ map directRet ss
directRet (SIf _ st sf) = directRet st `S.union` directRet sf
directRet (SCase _ cs) = S.unions $ map (directRet . snd) cs
directRet (SFun fs s) = directRet s `S.union` S.fromList [n | (n, (_, s')) <- M.toList fs, isReturningStmt s']
                                    `S.union` (S.unions $ map (directRet . snd . snd) $ M.toList fs)

returningFuns :: Stmt -> S.Set TH.Name
returningFuns s = saturateSet (flip (M.findWithDefault S.empty) tailCalled) $ directRet s
    where
    tailCalled = M.fromListWith S.union $ map (\e -> (cgEdgeDst e, S.singleton $ cgEdgeSrc e)) $ filter cgEdgeTail $ callGraph s

returningFunsFlat :: FunMap -> S.Set TH.Name
returningFunsFlat = returningFuns . flip SFun SNop

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

data DTData = DTData {
    _dtDataFunction :: TH.Name,
    _dtDataReturning :: S.Set TH.Name
}

$(makeLenses ''DTData)

deTailCall :: MonadRefresh m => Prog -> m Prog
deTailCall prog = do
    s' <- flip runReaderT (DTData (TH.mkName "") (returningFuns $ progBody prog)) $ deTailCallStmt $ progBody prog
    return $ prog { progBody = s' }

deTailCallStmt :: (MonadReader DTData m, MonadRefresh m) => Stmt -> m Stmt
deTailCallStmt s@(SNop) = return s
deTailCallStmt s@(SEmit _) = return s
deTailCallStmt   (SLet t n e s) = SLet t n e <$> deTailCallStmt s
deTailCallStmt s@(SAssign _ _) = return s
deTailCallStmt   (SBlock ss) = SBlock <$> mapM deTailCallStmt ss
deTailCallStmt   (SIf e st sf) = SIf e <$> deTailCallStmt st <*> deTailCallStmt sf
deTailCallStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> deTailCallStmt s) cs
deTailCallStmt s@(SRet (VExp _)) = return s
deTailCallStmt s@(SRet (VCall f e)) = do
    rf <- view dtDataReturning
    f' <- view dtDataFunction
    if f `S.member` rf && f /= f'
    then do
        n <- refreshName f
        return $ SLet VarLet  n (VCall f e) (SRet (VExp $ TH.VarE n))
    else return s
deTailCallStmt   (SFun fs s) =
    SFun <$> mapWithKeyM (\f (p, s') -> locally dtDataFunction (const f) $ (p,) <$> deTailCallStmt s') fs <*> deTailCallStmt s

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
    s' <- flip runReaderT (LVData False (returningFuns $ progBody prog) [] [] M.empty) $ makeLocalVarsStmt $ progBody prog
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

data Partition a = Partition {
    partitionMap :: M.Map a Int,
    partitionSets :: M.Map Int (S.Set a)
} deriving Show

partitionLookup :: Ord a => a -> Partition a -> Int
partitionLookup k = fromJust . M.lookup k . partitionMap

tailCallSCC :: FunMap -> Partition TH.Name
tailCallSCC fs = Partition pMap pSets
    where
    pMap = M.unions $ (\(i, is) -> map (`M.singleton` i) is) =<< sccs
    pSets = M.fromList $ map (id *** S.fromList) sccs
    sccs = zip [0..] $ map flattenSCC $ stronglyConnComp graph
    graph = map toSCC $ M.toList $ M.unionsWith S.union $ (map funToGr (M.keys fs) ++) $ map edgeToGr $ callGraphFlat fs
    edgeToGr e | cgEdgeTail e = M.singleton (cgEdgeSrc e) (S.singleton $ cgEdgeDst e)
               | otherwise = M.empty
    funToGr n = M.singleton n S.empty
    toSCC (n, ns) = (n, n, S.toList ns)

data TCData = TCData {
    _tcDataProgName :: String,
    _tcDataCont :: TH.Name,
    _tcDataType :: TH.Name,
    _tcDataApply :: TH.Name,
    _tcDataFreeVars :: S.Set TH.Name,
    _tcDataReturning :: S.Set TH.Name,
    _tcDataName :: TH.Name,
    _tcDataPartition :: Partition TH.Name
}

$(makeLenses ''TCData)

data ContData = ContData {
    contDataConName :: TH.Name,
    contDataCaller :: TH.Name,
    contDataCalled :: TH.Name,
    contDataResName :: TH.Name,
    contDataVars :: [TH.Name],
    contDataExp :: TH.Exp,
    contDataTgt :: ContTgt
}

data ContTgt = ContTgtFun TH.Name | ContTgtCont TH.Name | ContTgtFunCont TH.Name

makeTailCallsStmt :: (MonadRefresh m, MonadUnique m, MonadReader TCData m, MonadWriter [ContData] m) => Stmt -> m Stmt
makeTailCallsStmt   (SLet VarLet n (VCall f e) (SRet vst)) = do
    TCData name cfn _ an fvs rfs fn _ <- ask
    let vs = S.toList $ freeVars vst `S.difference` S.insert n fvs
    cn <- makeSeqName $ "C" ++ name ++ TH.nameBase f
    part <- view tcDataPartition
    b <- flip partitionLookup part <$> view tcDataName
    case vst of
        VCall f' e' 
            | f' `S.notMember` rfs -> do
                tell [ContData cn fn f n vs e' (ContTgtFun f')]
                return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE vs)]))
            | b == partitionLookup f' part -> do
                tell [ContData cn fn f n vs e' (ContTgtFunCont f')]
                return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
            | otherwise -> error "unsupported tail call"
        VExp e' -> do
            tell [ContData cn fn f n vs e' (ContTgtCont an)]
            return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
makeTailCallsStmt   (SLet VarLet n (VExp e) s) = locally tcDataFreeVars (S.delete n) $ SLet VarLet n (VExp e) <$> makeTailCallsStmt s
makeTailCallsStmt   (SIf e st sf) = SIf e <$> makeTailCallsStmt st <*> makeTailCallsStmt sf
makeTailCallsStmt   (SCase e cs) = SCase e <$> mapM (\(p, s) -> (p,) <$> makeTailCallsStmt s) cs
makeTailCallsStmt   (SRet (VExp e)) = SRet <$> (VCall <$> view tcDataApply <*> ((\cfn -> tupE [e, TH.VarE cfn]) <$> view tcDataCont))
makeTailCallsStmt s@(SRet (VCall f e)) = do
    r <- S.member f <$> view tcDataReturning
    if not r then return s
    else do
        part <- view tcDataPartition
        b <- flip partitionLookup part <$> view tcDataName
        unless (b == partitionLookup f part) $ error "unsupported tail call"
        cfn <- view tcDataCont
        return $ SRet $ VCall f $ tupE [e, TH.VarE cfn]
makeTailCallsStmt   (SBlock [SEmit e,s]) = (\s' -> SBlock [SEmit e, s']) <$> makeTailCallsStmt s
makeTailCallsStmt s = error $ "makeTailCallsStmt statement not in tree form: " ++ show s

makeTailCalls :: MonadRefresh m => NProg -> m NProg
makeTailCalls prog = evalUniqueT $ do
    partInfo <- fmap (M.map fromJust . M.filter isJust) $ forM (partitionSets part) $ \ns -> do
        let n = S.findMin ns
        if n `S.member` rfs
        then do
            ctn <- refreshSeqNameWithPrefix ("CT" ++ name) n
            an <- refreshNameWithPrefix "ap" n
            return $ Just (ctn, an)
        else return Nothing
    (fsd, cds) <- runWriterT $ forM (M.toList $ nProgFuns prog) $ \(n, (p, s)) -> do
        case M.lookup (partitionLookup n part) partInfo of
            Just (ctn, an) -> do
                cfn <- makeName "c"
                s' <- flip runReaderT (TCData name cfn ctn an fvs rfs n part) $ makeTailCallsStmt s
                return (n, (tupP [p, TH.VarP cfn], s'))
            Nothing -> do
                s' <- flip runReaderT (TCData name (error "cfn") (error "ctn") (error "an") fvs rfs n part) $ makeTailCallsStmt s
                return (n, (p, s'))
    let cdmap = M.fromListWith (++) $ map (flip partitionLookup part . contDataCalled &&& return) cds
    apfs <- mapM (apf cdmap) $ M.toList partInfo
    cdefs <- mapM (cdef cdmap) $ M.toList partInfo
    return $ prog { nProgFuns = M.fromList $ apfs ++ fsd, nProgConts = M.unions cdefs `M.union` nProgConts prog }
    where
    fvs = freeVarsFunMap $ nProgFuns prog
    part = tailCallSCC $ nProgFuns prog
    name = TH.nameBase $ nProgName prog
    rfs = returningFunsFlat (nProgFuns prog)
    apf cdmap (pid, (_ctn, an))
        | Just cds <- M.lookup pid cdmap = do
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
                ContTgtFunCont fn -> do
                    rcn <- makeName "rc"
                    return (TH.ConP (contDataConName cd) [tupP $ map TH.VarP $ rcn : contDataVars cd],
                        SLet VarLet (contDataResName cd) (VExp $ TH.VarE rn) $ SRet (VCall fn (tupE [contDataExp cd, TH.VarE rcn])))
            return (an, (tupP [TH.VarP rn, TH.VarP cfn], SCase (TH.VarE cfn) cs))
        | otherwise = return (an, (tupP [], SNop)) -- will be cleaned up anyway
    cdef cdmap (pid, (ctn, _an))
        | Just cds <- M.lookup pid cdmap = do
            cs <- forM cds $ \cd -> case contDataTgt cd of
                ContTgtFun _ ->
                    return (contDataConName cd, contDataVars cd)
                ContTgtCont _ -> do
                    rcn <- makeName "rc"
                    return (contDataConName cd, rcn : contDataVars cd)
                ContTgtFunCont _ -> do
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

