module FSM.Process.ReturningFuns(returningFuns, returningFunsFlat) where

import FSM.Lang
import Prelude
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Process.CallGraph

saturateSet :: Ord k => (k -> S.Set k) -> S.Set k -> S.Set k
saturateSet m = flip g S.empty where
    g s = foldr (.) id (map f (S.toList s))
    f n s | n `S.member` s = s
          | otherwise = g (m n) (S.insert n s)

directRetFunMap :: FunMap l -> S.Set TH.Name
directRetFunMap fs = S.fromList [n | (n, (_, s')) <- M.toList fs, isReturningStmt s']
                     `S.union` (S.unions $ map (directRet . snd . snd) $ M.toList fs)

directRet :: Stmt l -> S.Set TH.Name
directRet SNop = S.empty
directRet (SYield _) = S.empty
directRet (SLet _ _ _ s) = directRet s
directRet (SAssign _ _) = S.empty
directRet (SRet _) = S.empty
directRet (SBlock ss) = S.unions $ map directRet ss
directRet (SIf _ st sf) = directRet st `S.union` directRet sf
directRet (SCase _ cs) = S.unions $ map (directRet . snd) cs
directRet (SFun fs s) = directRet s `S.union` directRetFunMap fs

returningFunsH :: CG -> S.Set TH.Name -> S.Set TH.Name
returningFunsH cg ns = saturateSet (flip (M.findWithDefault S.empty) tailCalled) ns
    where
    tailCalled = M.fromListWith S.union $ map (\e -> (cgEdgeDst e, S.singleton $ cgEdgeSrc e)) $ filter cgEdgeTail cg

returningFuns :: Stmt l -> S.Set TH.Name
returningFuns s = returningFunsH (callGraph s) (directRet s)

returningFunsFlat :: FunMap l -> S.Set TH.Name
returningFunsFlat fs = returningFunsH (callGraphFlat fs) (directRetFunMap fs)

isReturningStmt :: Stmt l -> Bool
isReturningStmt SNop = False
isReturningStmt (SYield _) = False
isReturningStmt (SLet _ _ _ s) = isReturningStmt s
isReturningStmt (SAssign _ _) = False
isReturningStmt (SBlock ss) = or $ isReturningStmt <$> ss
isReturningStmt (SIf _ st sf) = isReturningStmt st || isReturningStmt sf
isReturningStmt (SCase _ cs) = or $ isReturningStmt <$> map snd cs
isReturningStmt (SRet (VExp _)) = True
isReturningStmt (SRet (VCall _ _)) = False
isReturningStmt (SFun _ s) = isReturningStmt s

