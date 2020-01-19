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
    TH.tupE [TH.appsE $ TH.conE (conName $ transNextState tr) : map pure (transNextStateParams tr), pure $ transOutput tr]
compileDT (DTIf e dt df) =
    TH.condE (pure e) (compileDT dt) (compileDT df)

compileFSM :: String -> FSM -> TH.Q [TH.Dec]
compileFSM nm fsm = do
    initStateName <- TH.newName ("fsmInitState_" ++ nm)
    stateName <- TH.newName ("FSMState_" ++ nm)
    funcName <- TH.newName ("fsmFunc_" ++ nm)
    stateData <- forM (M.assocs $ fsmStates fsm) $ \(n, s) -> do
        tps <- forM (fsmStateParams s) $ \n -> TH.newName (TH.nameBase n)
        return (tps, TH.NormalC (conName n) (map (\t -> (b, TH.VarT t)) tps))
    let stateCons = map snd stateData
    let tvars = map TH.PlainTV $ fst =<< stateData
    funcClauses <- forM (M.assocs $ fsmStates fsm) $ \(n, s) -> do
        TH.clause [TH.conP (conName n) (map TH.varP (fsmStateParams s)), TH.tupP (map TH.varP (fsmInputs fsm))] (TH.normalB $ compileDT $ fsmStateTrans s) []
    return [TH.DataD [] stateName tvars Nothing stateCons [TH.DerivClause Nothing [TH.ConT ''Show]],
            TH.ValD (TH.VarP initStateName) (TH.NormalB $ TH.ConE $ conName $ fsmInitState fsm) [],
            TH.FunD funcName funcClauses]


