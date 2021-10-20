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
    if f `S.member` rfs then do
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
        unless (b == partitionLookup f part) $ error "unsupported tail call"
        cfn <- view tcDataCont
        return $ SRet $ VCall f $ tupE [e, TH.VarE cfn]
makeTailCallsStmt   (SBlock [SYield e,s]) = (\s' -> SBlock [SYield e, s']) <$> makeTailCallsStmt s
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
    part = tailCallSCCN prog
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

