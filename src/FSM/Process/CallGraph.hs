module FSM.Process.CallGraph(CGEdge(..), CG, callGraph, callGraphFlat, callGraphProg, callGraphNProg) where

import FSM.Lang
import Prelude
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

data CGEdge = CGEdge {
    cgEdgeSrc :: TH.Name,
    cgEdgeDst :: TH.Name,
    cgEdgeArg :: TH.Exp,
    cgEdgeTail :: Bool
}

type CG = [CGEdge]

callGraph :: Stmt -> CG
callGraph s = callGraphStmt (TH.mkName "") s

callGraphFlat :: FunMap -> CG
callGraphFlat fs = callGraphFunMap (TH.mkName "") fs SNop

callGraphFunMap :: TH.Name -> FunMap -> Stmt -> CG
callGraphFunMap n fs s = (M.toList fs >>= \(n', (_, s')) -> callGraphStmt n' s') `mplus` callGraphStmt n s

callGraphStmt :: TH.Name -> Stmt -> CG
callGraphStmt _ SNop = mzero
callGraphStmt _ (SYield _) = mzero
callGraphStmt n (SLet _ _ vs s) = callGraphVStmt n False vs `mplus` callGraphStmt n s
callGraphStmt _ (SAssign _ _) = mzero
callGraphStmt n (SRet vs) = callGraphVStmt n True vs
callGraphStmt n (SBlock ss) = callGraphStmt n =<< ss
callGraphStmt n (SIf _ st sf) = callGraphStmt n st `mplus` callGraphStmt n sf
callGraphStmt n (SCase _ cs) = callGraphStmt n =<< map snd cs
callGraphStmt n (SFun fs s) = callGraphFunMap n fs s

callGraphVStmt :: TH.Name -> Bool -> VStmt -> CG
callGraphVStmt _ _ (VExp _) = mzero
callGraphVStmt n t (VCall n' e) = return $ CGEdge n n' e t

callGraphProg :: Prog -> CG
callGraphProg prog = callGraph $ progBody prog

callGraphNProg :: NProg -> CG
callGraphNProg prog = CGEdge (TH.mkName "INIT") (nProgInit prog) (nProgInitParam prog) True : callGraphFlat (nProgFuns prog)

