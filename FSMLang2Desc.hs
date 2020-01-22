module FSMLang2Desc where

import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSMLang
import FSMDesc
import Prelude

stmt2dtree :: Stmt -> DecisionTree Transition
stmt2dtree (SIf e s1 s2) = DTIf e (stmt2dtree s1) (stmt2dtree s2)
stmt2dtree (SBlock ss) = stmts2dtree ss

stmts2dtree :: [Stmt] -> DecisionTree Transition
stmts2dtree [SEmit e, SRet (VCall n ec)] = DTLeaf $ Transition e n ec

fun2state :: (TH.Pat, Stmt) -> FSMState
fun2state (p, s) = FSMState p (stmt2dtree s)

lang2desc :: Prog -> FSM
lang2desc (Prog is [SFun fs, SRet (VCall f1 e1)]) = FSM {
    fsmStates = M.map fun2state fs,
    fsmInputs = is,
    fsmInitState = f1,
    fsmInitStateParam = e1
}

