{-# LANGUAGE TemplateHaskell, DeriveGeneric, TupleSections #-}
module FSMDescGenADT where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Control.Monad
import FSMDesc
import Prelude
import Data.List
import Data.Maybe(fromJust)
import GHC.Generics(Generic)
import Clash.Prelude(NFDataX, BitPack)

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
compilePat (TH.TupP ts) = do
    rs <- mapM compilePat ts
    return (fst =<< rs, foldl TH.AppT (TH.TupleT $ length rs) $ map snd rs)

makeConNames :: [TH.Name] -> M.Map TH.Name TH.Name
makeConNames = fst . foldl' f (M.empty, M.empty) where
    f (cn, d) n | Just k <- M.lookup s d = (M.insert n (TH.mkName $ "C" ++ s ++ show k) cn, M.insert s (k+1) d)
                | otherwise = (M.insert n (TH.mkName $ "C" ++ s) cn, M.insert s 0 d)
        where s = TH.nameBase n

compileFSM :: String -> FSM -> TH.Q [TH.Dec]
compileFSM nm fsm = do
    let cn = makeConNames (M.keys $ fsmStates fsm)
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
    return [TH.DataD [] stateName tvars Nothing stateCons [TH.DerivClause Nothing [TH.ConT ''Show, TH.ConT ''Generic, TH.ConT ''NFDataX]],
            TH.ValD (TH.VarP initStateName) (TH.NormalB $ TH.AppE (TH.ConE $ conName cn $ fsmInitState fsm) (fsmInitStateParam fsm)) [],
            TH.FunD funcName funcClauses]


