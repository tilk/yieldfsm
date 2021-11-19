module FSM.Process.PreviousInputs(previousInputs) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Arrow
import qualified Clash.Prelude as CP
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

updateYieldsStmt :: (IsDesugared l, WithAssign l) => [Stmt l] -> Stmt l -> Stmt l
updateYieldsStmt ss s@(SYield _) = SBlock $ ss ++ [s]
updateYieldsStmt ss   (SBlock ss1) = SBlock $ map (updateYieldsStmt ss) ss1
updateYieldsStmt _  s@SNop = s
updateYieldsStmt _  s@(SAssign _ _) = s
updateYieldsStmt _  s@(SRet _) = s
updateYieldsStmt ss   (SIf e st sf) = SIf e (updateYieldsStmt ss st) (updateYieldsStmt ss sf)
updateYieldsStmt ss   (SCase e cs) = SCase e (map (id *** updateYieldsStmt ss) cs)
updateYieldsStmt ss   (SFun fs s) = SFun (M.map (id *** updateYieldsStmt ss) fs) (updateYieldsStmt ss s)
updateYieldsStmt ss   (SLet t n vs s) = SLet t n vs (updateYieldsStmt ss s)

newVars :: (String, Int) -> [(TH.Name, TH.Name)]
newVars (_, 0) = []
newVars (n, k) = (primName n k, primName n (k-1)):newVars (n, k-1)

primName :: String -> Int -> TH.Name
primName n k = TH.mkName $ n ++ replicate k '\''

addVar :: (IsDesugared l, WithAssign l) => TH.Name -> Stmt l -> Stmt l
addVar n = SLet VarMut n (VExp $ TH.VarE 'CP.undefined)

previousInputs :: (IsDesugared l, WithAssign l) => Prog l -> Prog l
previousInputs prog 
    | length pvars > 0 = prog { progBody = flip (foldr addVar) (map fst pvars) $ updateYieldsStmt (map (\(n, n') -> SAssign n (TH.VarE n')) pvars) $ progBody prog }
    | otherwise = prog
    where
    ivars = S.map TH.nameBase $ boundVars $ progInputs prog
    pvars = concatMap newVars $ M.toList $ M.fromListWith max $ filter ((`S.member` ivars) . fst) $ map countPrims $ S.toList $ S.map TH.nameBase $ freeVars prog

countPrims :: String -> (String, Int)
countPrims = f 0 . reverse where
    f k ('\'':s) = f (k+1) s
    f k s = (reverse s, k)

