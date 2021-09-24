module FSM.Process.SimplifyCase(simplifyCase) where

import FSM.Lang
import Prelude
import Control.Arrow
import Control.Monad
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

-- TODO: we need to distinguish between two cases
-- case 1: we know for certain the pattern doesn't match
-- case 2: it's unknown whether the pattern matches
-- a new monad with two Nothings is needed

simplifyCase1 :: TH.Exp -> TH.Pat -> Stmt -> Maybe Stmt
simplifyCase1 e (TH.VarP n) s = Just $ SLet VarLet n (VExp e) s
simplifyCase1 (TH.TupE mes) (TH.TupP ps) s
        | length mes == length ps, all isJust mes = foldM (flip $ uncurry simplifyCase1) s $ zip (map fromJust mes) ps
simplifyCase1 _ (TH.TupP _) _ = Nothing
simplifyCase1 _ _ _ = Nothing

simplifyCaseDo :: TH.Exp -> [(TH.Pat, Stmt)] -> Stmt
simplifyCaseDo e cs = maybe (SCase e cs) id $ msum (map (uncurry $ simplifyCase1 e) cs)

simplifyCaseStmt :: Stmt -> Stmt
simplifyCaseStmt   (SCase e cs) = simplifyCaseDo e (map (id *** simplifyCaseStmt) cs)
simplifyCaseStmt e@(SRet _) = e
simplifyCaseStmt e@(SAssign _ _) = e
simplifyCaseStmt e@(SYield _) = e
simplifyCaseStmt   (SFun fs s) = SFun (simplifyCaseFunMap fs) (simplifyCaseStmt s)
simplifyCaseStmt   (SLet t n vs s) = SLet t n vs (simplifyCaseStmt s)
simplifyCaseStmt   (SBlock ss) = SBlock $ map simplifyCaseStmt ss
simplifyCaseStmt   (SIf e st sf) = SIf e (simplifyCaseStmt st) (simplifyCaseStmt sf)
simplifyCaseStmt e@SNop = e

simplifyCaseFunMap :: FunMap -> FunMap
simplifyCaseFunMap = M.map (id *** simplifyCaseStmt)

simplifyCase :: NProg -> NProg
simplifyCase prog = prog { nProgFuns = simplifyCaseFunMap $ nProgFuns prog }

