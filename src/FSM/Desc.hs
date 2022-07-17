{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Target language of the YieldFSM compiler. The 'FSM' data type is compiled
to Mealy machines in the module 'FSM.DescGenADT'.
|-}
module FSM.Desc where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

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
    fsmInitState :: TH.Name,
    fsmInitStateParam :: TH.Exp,
    fsmConts :: M.Map TH.Name (M.Map TH.Name [TH.Name])
}

