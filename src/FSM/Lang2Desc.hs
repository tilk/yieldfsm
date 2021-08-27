{-# LANGUAGE ViewPatterns #-}
module FSM.Lang2Desc(nprog2desc) where

import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Lang
import FSM.Desc
import Prelude
import Control.Arrow

stmt2dtree :: Stmt -> DecisionTree Transition
stmt2dtree (SIf e s1 s2) = DTIf e (stmt2dtree s1) (stmt2dtree s2)
stmt2dtree (SBlock ss) = stmts2dtree ss
stmt2dtree (SLet VarLet n (VExp e) s) = DTLet (TH.VarP n) e (stmt2dtree s)
stmt2dtree (SCase e cs) = DTCase e (map (id *** stmt2dtree) cs)

stmts2dtree :: [Stmt] -> DecisionTree Transition
stmts2dtree [SEmit e, SRet (VCall n ec)] = DTLeaf $ Transition e n ec

fun2state :: (TH.Pat, Stmt) -> FSMState
fun2state (p, s) = FSMState p (stmt2dtree s)

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

