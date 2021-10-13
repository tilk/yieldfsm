{-# LANGUAGE TemplateHaskell #-}
module FSM.Desc(Transition(..), DecisionTree(..), FSMState(..), MemInfo(..), FSM(..)) where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude
import FSM.Lang(MemInfo(..))

data Transition = Transition {
    transOutput :: TH.Exp,
    transNextState :: TH.Name,
    transNextStateParams :: TH.Exp
}

data DecisionTree a = DTIf TH.Exp (DecisionTree a) (DecisionTree a)
                    | DTLet TH.Pat TH.Exp (DecisionTree a)
                    | DTCase TH.Exp [(TH.Pat, DecisionTree a)]
                    | DTLeaf a

data FSMState = FSMState {
    fsmStateParams :: TH.Pat,
    fsmStateTrans :: DecisionTree Transition
}

data FSM = FSM {
    fsmName :: TH.Name,
    fsmType :: TH.Type,
    fsmParams :: [TH.Pat],
    fsmStates :: M.Map TH.Name FSMState,
    fsmInputs :: Maybe TH.Pat,
    fsmMemories :: M.Map TH.Name MemInfo,
    fsmInitState :: TH.Name,
    fsmInitStateParam :: TH.Exp,
    fsmConts :: M.Map TH.Name (M.Map TH.Name [TH.Name])
}

