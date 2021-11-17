{-# LANGUAGE ViewPatterns #-}
module FSM.Lang2Desc(nprog2desc) where

import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Lang
import FSM.Desc
import Prelude
import Control.Arrow

stmt2dtree :: Maybe TH.Exp -> Stmt LvlLowest -> DecisionTree Transition
stmt2dtree me       (SIf e s1 s2) = DTIf e (stmt2dtree me s1) (stmt2dtree me s2)
stmt2dtree Nothing  (SBlock [SYield e, s]) = stmt2dtree (Just e) s
stmt2dtree me       (SLet VarLet n (VExp e) s) = DTLet (TH.VarP n) e (stmt2dtree me s)
stmt2dtree me       (SCase e cs) = DTCase e (map (id *** stmt2dtree me) cs)
stmt2dtree (Just e) (SRet (VCall n ec)) = DTLeaf $ Transition e n ec

fun2state :: (TH.Pat, Stmt LvlLowest) -> FSMState
fun2state (p, s) = FSMState p (stmt2dtree Nothing s)

nprog2desc :: NProg LvlLowest -> FSM
nprog2desc prog = FSM { 
    fsmName = nProgName prog,
    fsmType = nProgType prog,
    fsmParams = nProgParams prog,
    fsmStates = M.map fun2state $ nProgFuns prog,
    fsmInputs = nProgInputs prog,
    fsmInitState = nProgInit prog,
    fsmInitStateParam = nProgInitParam prog,
    fsmConts = nProgConts prog
}

