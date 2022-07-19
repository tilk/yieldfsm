{-# LANGUAGE TemplateHaskell #-}
{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Target language of the YieldFSM compiler. The 'FSM' data type is compiled
to Mealy machines in the module @FSM.DescGenADT@.
-}
module FSM.Desc where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

{-|
Encodes a Mealy machine transition.
-}
data Transition = Transition {
    transOutput :: TH.Exp,         -- ^ Output value on this transition.
    transNextState :: TH.Name,     -- ^ The name of the next state.
    transNextStateParams :: TH.Exp -- ^ Parameters of the next state.
}

{-|
Decision trees. Used to represent conditions on which the next
transition is selected.
-}
data DecisionTree a = DTIf TH.Exp (DecisionTree a) (DecisionTree a) -- ^ Boolean condition.
                    | DTLet TH.Pat TH.Exp (DecisionTree a)          -- ^ Let definition.
                    | DTCase TH.Exp [(TH.Pat, DecisionTree a)]      -- ^ Case (pattern-matching) condition.
                    | DTLeaf a                                      -- ^ Leaf of the decision tree.

{-|
Encodes a Mealy machine state. A state can be parametrized, and it has
a number of transitions, encoded using a decision tree.
-}
data FSMState = FSMState {
    fsmStateParams :: TH.Pat,                -- ^ State parameters.
    fsmStateTrans :: DecisionTree Transition -- ^ State transitions.
}

{-|
Encodes a Mealy machine.
-}
data FSM = FSM {
    fsmName :: TH.Name,                  -- ^ Machine name.
    fsmType :: TH.Type,                  -- ^ Clash type of the machine.
    fsmParams :: [TH.Pat],               -- ^ Machine parameters.
    fsmStates :: M.Map TH.Name FSMState, -- ^ Machine states.
    fsmInputs :: Maybe TH.Pat,           -- ^ Machine inputs.
    fsmInitState :: TH.Name,             -- ^ Machine initial state.
    fsmInitStateParam :: TH.Exp,         -- ^ Parameters of the initial state.
    fsmConts :: M.Map TH.Name (M.Map TH.Name [TH.Name]) -- ^ Continuation data types.
}

