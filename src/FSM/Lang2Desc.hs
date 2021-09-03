{-# LANGUAGE ViewPatterns #-}
module FSM.Lang2Desc(nprog2desc) where

import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Lang
import FSM.Desc
import Prelude
import Control.Arrow

stmt2dtree :: Maybe TH.Exp -> Stmt -> DecisionTree Transition
stmt2dtree me       (SIf e s1 s2) = DTIf e (stmt2dtree me s1) (stmt2dtree me s2)
stmt2dtree Nothing  (SBlock [SEmit e, s]) = stmt2dtree (Just e) s
stmt2dtree me       (SLet VarLet n (VExp e) s) = DTLet (TH.VarP n) e (stmt2dtree me s)
stmt2dtree me       (SCase e cs) = DTCase e (map (id *** stmt2dtree me) cs)
stmt2dtree (Just e) (SRet (VCall n ec)) = DTLeaf $ Transition e n ec

fun2state :: (TH.Pat, Stmt) -> FSMState
fun2state (p, s) = FSMState p (stmt2dtree Nothing s)

nprog2desc (NProg n t ps is fs f1 e1 cs) = FSM { 
    fsmName = n,
    fsmType = t,
    fsmParams = ps,
    fsmStates = M.map fun2state fs,
    fsmInputs = is,
    fsmInitState = f1,
    fsmInitStateParam = e1,
    fsmConts = cs
}

