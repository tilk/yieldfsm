{-# LANGUAGE TemplateHaskell #-}
module FSMDesc where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M

data Transition = Transition {
    transOutput :: TH.Exp,
    transNextState :: TH.Name,
    transNextStateParams :: [TH.Exp]
}

data DecisionTree a = DTIf TH.Exp (DecisionTree a) (DecisionTree a)
                    | DTLeaf a

data FSMState = FSMState {
    fsmStateParams :: [TH.Name],
    fsmStateTrans :: DecisionTree Transition
}

data FSM = FSM {
    fsmStates :: M.Map TH.Name FSMState,
    fsmInputs :: [TH.Name],
    fsmInitState :: TH.Name
}

