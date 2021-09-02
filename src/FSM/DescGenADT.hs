{-# LANGUAGE TemplateHaskell, DeriveGeneric, TupleSections #-}
module FSM.DescGenADT where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Control.Monad
import FSM.Desc
import Prelude
import Data.List
import Data.Maybe(fromJust)
import GHC.Generics(Generic)
import Clash.Prelude(NFDataX, BitPack, mealy)

conName cn = fromJust . flip M.lookup cn

b = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

compileDT :: M.Map TH.Name TH.Name -> DecisionTree Transition -> TH.Q TH.Exp
compileDT cn (DTLeaf tr) =
    TH.tupE [TH.conE (conName cn $ transNextState tr) `TH.appE` pure (transNextStateParams tr), pure $ transOutput tr]
compileDT cn (DTIf e dt df) =
    TH.condE (pure e) (compileDT cn dt) (compileDT cn df)
compileDT cn (DTCase e cs) =
    TH.caseE (pure e) (flip map cs $ \(p, d) -> TH.match (pure p) (TH.normalB $ compileDT cn d) [])
compileDT cn (DTLet p e d) =
    TH.letE [TH.valD (pure p) (TH.normalB $ pure e) []] (compileDT cn d)

compilePat :: TH.Pat -> TH.Q ([TH.Name], TH.Type)
compilePat (TH.VarP n) = do
    n' <- TH.newName (TH.nameBase n)
    return ([n'], TH.VarT n')
compilePat (TH.TupP ps) = do
    rs <- mapM compilePat ps
    return (fst =<< rs, foldl TH.AppT (TH.TupleT $ length rs) $ map snd rs)
compilePat (TH.ConP n ps) = do
    rs <- mapM compilePat ps
    return (fst =<< rs, foldl TH.AppT (TH.ConT n) $ map snd rs)

makeConNames :: String -> [TH.Name] -> M.Map TH.Name TH.Name
makeConNames nm = fst . foldl' f (M.empty, M.empty) where
    f (cn, d) n | Just k <- M.lookup s d = (M.insert n (TH.mkName $ "C" ++ s ++ "_" ++ nm ++ show k) cn, M.insert s (k+1) d)
                | otherwise = (M.insert n (TH.mkName $ "C" ++ s ++ "_" ++ nm) cn, M.insert s 0 d)
        where s = TH.nameBase n

derivclause = TH.DerivClause Nothing [TH.ConT ''Show, TH.ConT ''Generic, TH.ConT ''NFDataX]

tupT [] = TH.TupleT 0
tupT [t] = t
tupT ts = foldl TH.AppT (TH.TupleT (length ts)) ts

compileFSM :: FSM -> TH.Q [TH.Dec]
compileFSM fsm = do
    let nm = TH.nameBase $ fsmName fsm
    let cn = makeConNames nm (M.keys $ fsmStates fsm)
    initStateName <- TH.newName ("fsmInitState_" ++ nm)
    stateName <- TH.newName ("FSMState_" ++ nm)
    funcName <- TH.newName ("fsmFunc_" ++ nm)
    stateData <- forM (M.assocs $ fsmStates fsm) $ \(n, s) -> do
        (tps, tp) <- compilePat $ fsmStateParams s 
        return (tps, TH.NormalC (conName cn n) [(b, tp)])
    let stateCons = map snd stateData
    let tvars = map TH.PlainTV $ fst =<< stateData
    funcClauses <- forM (M.assocs $ fsmStates fsm) $ \(n, s) -> do
        TH.clause [TH.conP (conName cn n) [pure $ fsmStateParams s], pure $ fsmInputs fsm] (TH.normalB $ compileDT cn $ fsmStateTrans s) []
    contDecls <- forM (M.assocs $ fsmConts fsm) $ \(n, cs) -> do
        contCons <- forM (M.assocs cs) $ \(n', ns) -> return $ TH.NormalC n' [(b, tupT $ map TH.VarT ns)]
        let contTvars = map TH.PlainTV $ nub $ concat (M.elems cs)
        return $ TH.DataD [] n contTvars Nothing contCons [derivclause]
    return $ TH.DataD [] stateName tvars Nothing stateCons [derivclause] :
                TH.SigD (fsmName fsm) (fsmType fsm) :
                TH.FunD (fsmName fsm) [TH.Clause (fsmParams fsm) (TH.NormalB $ TH.AppE (TH.AppE (TH.VarE 'mealy) (TH.VarE funcName)) (TH.VarE initStateName)) (
                    TH.ValD (TH.VarP initStateName) (TH.NormalB $ TH.AppE (TH.ConE $ conName cn $ fsmInitState fsm) (fsmInitStateParam fsm)) [] :
                    TH.FunD funcName funcClauses : []
                )] :
                contDecls


