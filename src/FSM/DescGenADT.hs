{-# LANGUAGE TemplateHaskell, DeriveGeneric, TupleSections #-}
module FSM.DescGenADT where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Arrow
import FSM.Desc
import FSM.FreeVars
import Prelude
import Data.List(foldl', nub)
import Data.Maybe(fromJust)
import GHC.Generics(Generic)
import Clash.Prelude(NFDataX, mealy)

conName :: M.Map TH.Name TH.Name -> TH.Name -> TH.Name
conName cn = fromJust . flip M.lookup cn

b :: TH.Bang
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
compilePat _ = error "unsupported pattern"

makeConNames :: String -> [TH.Name] -> M.Map TH.Name TH.Name
makeConNames nm = fst . foldl' f (M.empty, M.empty) where
    f :: (M.Map TH.Name TH.Name, M.Map String Integer) -> TH.Name -> (M.Map TH.Name TH.Name, M.Map String Integer)
    f (cn, d) n | Just k <- M.lookup s d = (M.insert n (TH.mkName $ "C" ++ s ++ "_" ++ nm ++ show k) cn, M.insert s (k+1) d)
                | otherwise = (M.insert n (TH.mkName $ "C" ++ s ++ "_" ++ nm) cn, M.insert s 0 d)
        where s = TH.nameBase n

derivclause :: TH.DerivClause
derivclause = TH.DerivClause Nothing [TH.ConT ''Show, TH.ConT ''Generic, TH.ConT ''NFDataX]

tupT :: [TH.Type] -> TH.Type
tupT [] = TH.TupleT 0
tupT [t] = t
tupT ts = foldl TH.AppT (TH.TupleT (length ts)) ts

wildUnused :: S.Set TH.Name -> TH.Pat -> TH.Pat
wildUnused _ p@(TH.LitP _) = p
wildUnused s p@(TH.VarP n) | n `S.member` s = p
                                 | otherwise = TH.WildP
wildUnused s   (TH.TupP ps) = TH.TupP $ wildUnused s <$> ps
wildUnused s   (TH.UnboxedTupP ps) = TH.UnboxedTupP $ wildUnused s <$> ps
wildUnused s   (TH.UnboxedSumP p al ar) = TH.UnboxedSumP (wildUnused s p) al ar
wildUnused s   (TH.ConP n ps) = TH.ConP n $ wildUnused s <$> ps
wildUnused s   (TH.InfixP p1 n p2) = TH.InfixP (wildUnused s p1) n (wildUnused s p2)
wildUnused s   (TH.UInfixP p1 n p2) = TH.UInfixP (wildUnused s p1) n (wildUnused s p2)
wildUnused s   (TH.ParensP p) = TH.ParensP $ wildUnused s p
wildUnused s   (TH.TildeP p) = TH.TildeP $ wildUnused s p
wildUnused s   (TH.BangP p) = TH.BangP $ wildUnused s p
wildUnused s   (TH.AsP n p) = TH.AsP n $ wildUnused s p
wildUnused _   (TH.WildP) = TH.WildP
wildUnused s   (TH.RecP n fps) = TH.RecP n $ (id *** wildUnused s) <$> fps
wildUnused s   (TH.ListP ps) = TH.ListP $ wildUnused s <$> ps
wildUnused s   (TH.SigP p t) = TH.SigP (wildUnused s p) t
wildUnused s   (TH.ViewP e p) = TH.ViewP e $ wildUnused s p

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
        TH.clause [TH.conP (conName cn n) [pure $ fsmStateParams s], pure $ wildUnused (freeVars $ fsmStateTrans s) $ maybe (TH.TupP []) id $ fsmInputs fsm] (TH.normalB $ compileDT cn $ fsmStateTrans s) []
    contDecls <- forM (M.assocs $ fsmConts fsm) $ \(n, cs) -> do
        contCons <- forM (M.assocs cs) $ \(n', ns) -> return $ TH.NormalC n' [(b, tupT $ map TH.VarT ns)]
        let contTvars = map TH.PlainTV $ nub $ concat (M.elems cs)
        return $ TH.DataD [] n contTvars Nothing contCons [derivclause]
    return $ TH.DataD [] stateName tvars Nothing stateCons [derivclause] :
                TH.SigD (fsmName fsm) (fsmType fsm) :
                TH.FunD (fsmName fsm) [TH.Clause (fsmParams fsm) (TH.NormalB $ maybe (`TH.AppE` (TH.AppE (TH.VarE 'pure) (TH.TupE []))) (const id) (fsmInputs fsm) $ TH.AppE (TH.AppE (TH.VarE 'mealy) (TH.VarE funcName)) (TH.VarE initStateName)) (
                    TH.ValD (TH.VarP initStateName) (TH.NormalB $ TH.AppE (TH.ConE $ conName cn $ fsmInitState fsm) (fsmInitStateParam fsm)) [] :
                    TH.FunD funcName funcClauses : []
                )] :
                contDecls


