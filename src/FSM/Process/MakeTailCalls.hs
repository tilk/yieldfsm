{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.MakeTailCalls(makeTailCalls) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Data.Maybe
import Control.Arrow
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Util.MonadRefresh
import FSM.Util.MonadUnique
import FSM.Process.ReturningFuns
import FSM.Process.TailCallSCC

data TCData = TCData {
    _tcDataProgName :: String,
    _tcDataCont :: TH.Name,
    _tcDataType :: TH.Name,
    _tcDataApply :: TH.Name,
    _tcDataFreeVars :: S.Set TH.Name,
    _tcDataReturning :: S.Set TH.Name,
    _tcDataName :: TH.Name,
    _tcDataPartition :: Partition TH.Name,
    _tcDataPartInj :: M.Map (Int, Int) TH.Name
}

$(makeLenses ''TCData)

data ContData = ContData {
    contDataConName :: TH.Name,
    contDataCalled :: TH.Name,
    contDataResName :: TH.Name,
    contDataVars :: [TH.Name],
    contDataExp :: TH.Exp,
    contDataTgt :: ContTgt
}

data ContTgt = ContTgtFun TH.Name | ContTgtCont TH.Name | ContTgtFunCont TH.Name | ContTgtFunContInj TH.Name TH.Name

mlookup :: (Ord k, Monoid a) => k -> M.Map k a -> a
mlookup k = maybe mempty id . M.lookup k

makeTailCallsStmt :: (MonadRefresh m, MonadUnique m, MonadReader TCData m, MonadWriter [ContData] m) => Stmt 'LvlLowest -> m (Stmt 'LvlLowest)
makeTailCallsStmt   (SLet VarLet n (VCall f e) (SRet vst)) = do
    TCData name cfn _ an fvs rfs _ part partInj <- ask
    if f `S.member` rfs then do
        let vs = S.toList $ freeVars vst `S.difference` S.insert n fvs
        cn <- makeSeqName $ "C" ++ name ++ TH.nameBase f
        b <- flip partitionLookup part <$> view tcDataName
        case vst of
            VCall f' e'
                | f' `S.notMember` rfs -> do
                    tell [ContData cn f n vs e' (ContTgtFun f')]
                    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE vs)]))
                | b == b' -> do
                    tell [ContData cn f n vs e' (ContTgtFunCont f')]
                    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
                | Just icn <- M.lookup (b, b') partInj -> do
                    tell [ContData cn f n vs e' (ContTgtFunContInj f' icn)]
                    return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
                | otherwise -> error "should not happen"
                    where
                    b' = partitionLookup f' part
            VExp e' -> do
                tell [ContData cn f n vs e' (ContTgtCont an)]
                return $ SRet (VCall f (tupE [e, TH.AppE (TH.ConE cn) (tupE $ map TH.VarE $ cfn : vs)]))
    else return $ SRet (VCall f e)
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
        let b' = partitionLookup f part
        partInj <- view tcDataPartInj
        let inj = case M.lookup (b, b') partInj of
                    Just icn -> TH.AppE (TH.ConE icn)
                    Nothing | b == b' -> id
                            | otherwise -> error "should not happen"
        cfn <- view tcDataCont
        return $ SRet $ VCall f $ tupE [e, inj $ TH.VarE cfn]
makeTailCallsStmt   (SBlock [SYield e,s]) = (\s' -> SBlock [SYield e, s']) <$> makeTailCallsStmt s
makeTailCallsStmt s = error $ "makeTailCallsStmt statement not in tree form: " ++ show s

makeTailCalls :: MonadRefresh m => NProg 'LvlLowest -> m (NProg 'LvlLowest)
makeTailCalls prog = evalUniqueT $ do
    partInj <- forM (M.fromSet (const ()) edges) $ \() -> makeSeqName $ "C" ++ name
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
                s' <- flip runReaderT (TCData name cfn ctn an fvs rfs n part partInj) $ makeTailCallsStmt s
                return (n, (tupP [p, TH.VarP cfn], s'))
            Nothing -> do
                s' <- flip runReaderT (TCData name (error "cfn") (error "ctn") (error "an") fvs rfs n part partInj) $ makeTailCallsStmt s
                return (n, (p, s'))
    let cdmap = M.fromListWith (++) $ map (flip partitionLookup part . contDataCalled &&& return) cds
    let injmap = M.fromListWith (++) $ map (\((b, b'), icn) -> (b', [(icn, snd $ fromJust $ M.lookup b partInfo)])) $ M.toList partInj
    apfs <- mapM (apf cdmap injmap) $ M.toList partInfo
    cdefs <- mapM (cdef cdmap injmap) $ M.toList partInfo
    return $ prog { nProgFuns = M.fromList $ apfs ++ fsd, nProgConts = M.unions cdefs `M.union` nProgConts prog }
    where
    edges = S.filter (\(a, b) -> let retPart x = S.findMin (partitionSet x part) `S.member` rfs in retPart a && retPart b) $ partitionEdges part
    fvs = freeVarsFunMap $ nProgFuns prog
    part = tailCallSCCN prog
    name = TH.nameBase $ nProgName prog
    rfs = returningFunsFlat (nProgFuns prog)
    apf cdmap injmap (pid, (_ctn, an)) = do
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
            ContTgtFunContInj fn icn -> do
                rcn <- makeName "rc"
                return (TH.ConP (contDataConName cd) [tupP $ map TH.VarP $ rcn : contDataVars cd],
                    SLet VarLet (contDataResName cd) (VExp $ TH.VarE rn) $ SRet (VCall fn (tupE [contDataExp cd, TH.AppE (TH.ConE icn) (TH.VarE rcn)])))
        ics <- forM injs $ \(icn, rap) -> do
            rcn <- makeName "rc"
            return (TH.ConP icn [TH.VarP rcn],
                SRet (VCall rap (tupE [TH.VarE rn, TH.VarE rcn])))
        return (an, (tupP [TH.VarP rn, TH.VarP cfn], SCase (TH.VarE cfn) $ cs ++ ics))
        where
        cds = mlookup pid cdmap
        injs = mlookup pid injmap
    cdef cdmap injmap (pid, (ctn, _an)) = do
        cs <- forM cds $ \cd -> case contDataTgt cd of
            ContTgtFun _ ->
                return (contDataConName cd, contDataVars cd)
            ContTgtCont _ -> do
                rcn <- makeName "rc"
                return (contDataConName cd, rcn : contDataVars cd)
            ContTgtFunCont _ -> do
                rcn <- makeName "rc"
                return (contDataConName cd, rcn : contDataVars cd)
            ContTgtFunContInj _ _ -> do
                rcn <- makeName "rc"
                return (contDataConName cd, rcn : contDataVars cd)
        ics <- forM injs $ \(icn, _rap) -> do
            rcn <- makeName "rc"
            return (icn, [rcn])
        return $ M.singleton ctn $ M.fromList $ cs ++ ics
        where
        cds = mlookup pid cdmap
        injs = mlookup pid injmap

