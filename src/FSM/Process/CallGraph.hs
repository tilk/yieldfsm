{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>
|-}
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

callGraph :: IsDesugared l => Stmt l -> CG
callGraph s = callGraphStmt (TH.mkName "") s

callGraphFlat :: IsDesugared l => FunMap l -> CG
callGraphFlat fs = callGraphFunMap fs

callGraphFunMap :: IsDesugared l => FunMap l -> CG
callGraphFunMap fs = (M.toList fs >>= \(n', (_, s')) -> callGraphStmt n' s')

callGraphStmt :: IsDesugared l => TH.Name -> Stmt l -> CG
callGraphStmt _ SNop = mzero
callGraphStmt _ (SYield _) = mzero
callGraphStmt n (SYieldT _ s) = callGraphStmt n s
callGraphStmt n (SLet _ _ vs s) = callGraphVStmt n False vs `mplus` callGraphStmt n s
callGraphStmt _ (SAssign _ _) = mzero
callGraphStmt n (SRet vs) = callGraphVStmt n True vs
callGraphStmt n (SBlock ss) = callGraphStmt n =<< ss
callGraphStmt n (SIf _ st sf) = callGraphStmt n st `mplus` callGraphStmt n sf
callGraphStmt n (SCase _ cs) = callGraphStmt n =<< map snd cs
callGraphStmt n (SFun fs s) = callGraphFunMap fs `mplus` callGraphStmt n s

callGraphVStmt :: TH.Name -> Bool -> VStmt -> CG
callGraphVStmt _ _ (VExp _) = mzero
callGraphVStmt n t (VCall n' e) = return $ CGEdge n n' e t

callGraphProg :: IsDesugared l => Prog l -> CG
callGraphProg prog = callGraph $ progBody prog

callGraphNProg :: IsDesugared l => NProg l -> CG
callGraphNProg prog = CGEdge (TH.mkName "INIT") (nProgInit prog) (nProgInitParam prog) True : callGraphFlat (nProgFuns prog)

