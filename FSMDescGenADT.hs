{-# LANGUAGE TemplateHaskell #-}
module FSMDescGenADT where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Control.Monad
import FSMDesc

conName = TH.mkName . ("C" ++) . TH.nameBase

b = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

compileDT :: DecisionTree Transition -> TH.Q TH.Exp
compileDT (DTLeaf tr) =
    TH.tupE [TH.conE (conName $ transNextState tr) `TH.appE` pure (transNextStateParams tr), pure $ transOutput tr]
compileDT (DTIf e dt df) =
    TH.condE (pure e) (compileDT dt) (compileDT df)

compilePat :: TH.Pat -> TH.Q ([TH.Name], TH.Type)
compilePat (TH.VarP n) = do
    n' <- TH.newName (TH.nameBase n)
    return ([n'], TH.VarT n')
compilePat (TH.TupP ts) = do
    rs <- mapM compilePat ts
    return (fst =<< rs, foldl TH.AppT (TH.TupleT $ length rs) $ map snd rs)

compileFSM :: String -> FSM -> TH.Q [TH.Dec]
compileFSM nm fsm = do
    initStateName <- TH.newName ("fsmInitState_" ++ nm)
    stateName <- TH.newName ("FSMState_" ++ nm)
    funcName <- TH.newName ("fsmFunc_" ++ nm)
    stateData <- forM (M.assocs $ fsmStates fsm) $ \(n, s) -> do
        (tps, tp) <- compilePat $ fsmStateParams s 
        return (tps, TH.NormalC (conName n) [(b, tp)])
    let stateCons = map snd stateData
    let tvars = map TH.PlainTV $ fst =<< stateData
    funcClauses <- forM (M.assocs $ fsmStates fsm) $ \(n, s) -> do
        TH.clause [TH.conP (conName n) [pure $ fsmStateParams s], pure $ fsmInputs fsm] (TH.normalB $ compileDT $ fsmStateTrans s) []
    return [TH.DataD [] stateName tvars Nothing stateCons [TH.DerivClause Nothing [TH.ConT ''Show]],
            TH.ValD (TH.VarP initStateName) (TH.NormalB $ TH.AppE (TH.ConE $ conName $ fsmInitState fsm) (fsmInitStateParam fsm)) [],
            TH.FunD funcName funcClauses]


