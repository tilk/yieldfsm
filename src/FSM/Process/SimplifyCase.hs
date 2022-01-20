module FSM.Process.SimplifyCase(
    simplifyCase, simplifyCaseN, simplifyCaseNFull, simplifyCaseGen, mkLetGen
) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import qualified FSM.Util.MultiSet as MS
import FSM.Util.MMaybe

conExp :: [TH.Exp] -> TH.Exp -> Maybe (TH.Name, [TH.Exp])
conExp es (TH.ConE n) = Just (n, reverse es)
conExp es (TH.AppE e e') = conExp (e':es) e
conExp _ _ = Nothing

matchableExp :: TH.Exp -> Bool
matchableExp e | Just _ <- conExp [] e = True
matchableExp (TH.LitE _) = True
matchableExp (TH.TupE mes) | all isJust mes = True
matchableExp (TH.UnboxedTupE mes) | all isJust mes = True
matchableExp (TH.UnboxedSumE _ _ _) = True
matchableExp (TH.RecConE _ _) = True
matchableExp _ = False

simplifyCaseGen :: (TH.Name -> TH.Exp -> a -> a) -> TH.Exp -> TH.Pat -> a -> MMaybe a
simplifyCaseGen m e (TH.VarP n) s = MJust $ m n e s
simplifyCaseGen _ (TH.LitE l) (TH.LitP l') s | l == l' = MJust s
simplifyCaseGen _ e (TH.LitP _) _ | matchableExp e = MNo
simplifyCaseGen m (TH.TupE mes) (TH.TupP ps) s
    | length mes == length ps, all isJust mes = foldM (flip $ uncurry $ simplifyCaseGen m) s $ zip (map fromJust mes) ps
simplifyCaseGen _ e (TH.TupP _) _ | matchableExp e = MNo
simplifyCaseGen m e (TH.ConP n ps) s 
    | Just (n', es) <- conExp [] e, n == n', length es == length ps = foldM (flip $ uncurry $ simplifyCaseGen m) s $ zip es ps
simplifyCaseGen _ e (TH.ConP _ _) _ | matchableExp e = MNo
simplifyCaseGen _ _ _ _ = MNoNo

type KindMap = M.Map TH.Name VarKind

simplifyCase1 :: IsDesugared l => KindMap -> TH.Exp -> TH.Pat -> Stmt l -> MMaybe (Stmt l)
simplifyCase1 m = simplifyCaseGen (mkLet m VarLet)

simplifyCaseDo :: IsDesugared l => KindMap -> TH.Exp -> [(TH.Pat, Stmt l)] -> Stmt l
simplifyCaseDo m e cs = mmaybe (SCase e (map (simplifyCaseCase m) cs)) id $ msum (map (uncurry $ simplifyCase1 m e) cs)

simplifyCaseCase :: IsDesugared l => KindMap -> (TH.Pat, Stmt l) -> (TH.Pat, Stmt l)
simplifyCaseCase m (p, s) = (p, simplifyCaseStmt (setVars VarLet (boundVars p) m) s)

mkLetGen :: (FreeVars a, Subst a) => (KindMap -> a -> a) -> (TH.Name -> TH.Exp -> a -> a) -> KindMap -> TH.Name -> TH.Exp -> a -> a
mkLetGen f g m n e s
    | (TH.VarE n') <- e, Just VarLet <- M.lookup n' m = f m $ substSingle n e s
    | S.null (freeVars e `S.difference` M.keysSet (M.filter (== VarLet) m)) && (isConstantExpr e || MS.lookup n (freeVars s) <= 1) = f m $ substSingle n e s
    | otherwise = g n e $ f (M.insert n VarLet m) s

mkLet :: IsDesugared l => KindMap -> VarKind -> TH.Name -> TH.Exp -> Stmt l -> Stmt l
mkLet m t n e s
    | not (canSubst t) = SLet t n (VExp e) $ simplifyCaseStmt (M.insert n t m) s
    | otherwise = mkLetGen simplifyCaseStmt (\n' e' -> SLet VarLet n' (VExp e')) m n e s
    where
    canSubst VarLet = True
    canSubst VarMut = not $ hasAssigns n s

simplifyCaseStmt :: IsDesugared l => KindMap -> Stmt l -> Stmt l
simplifyCaseStmt m   (SCase e cs) = simplifyCaseDo m e cs
simplifyCaseStmt _ e@(SRet _) = e
simplifyCaseStmt _ e@(SAssign _ _) = e
simplifyCaseStmt _ e@(SYield _) = e
simplifyCaseStmt m   (SYieldT e s) = SYieldT e (simplifyCaseStmt m s)
simplifyCaseStmt m   (SFun fs s) = SFun (simplifyCaseFunMap m fs) (simplifyCaseStmt m s)
simplifyCaseStmt m   (SBlock ss) = SBlock $ map (simplifyCaseStmt m) ss
simplifyCaseStmt m   (SIf e st sf) = SIf e (simplifyCaseStmt m st) (simplifyCaseStmt m sf)
simplifyCaseStmt _ e@SNop = e
simplifyCaseStmt m   (SLet t n (VExp e) s) = mkLet m t n e s
simplifyCaseStmt m   (SLet t n vs s) = SLet t n vs (simplifyCaseStmt m s)

simplifyCaseFunMap :: IsDesugared l => KindMap -> FunMap l -> FunMap l
simplifyCaseFunMap m = M.map (simplifyCaseCase m)

simplifyCase :: IsDesugared l => Prog l -> Prog l
simplifyCase prog = prog { progBody = simplifyCaseStmt m $ progBody prog }
    where
    m = setVars VarMut (boundVars $ progInputs prog) $ setVars VarLet (freeVars $ progBody prog) M.empty

simplifyCaseN :: IsDesugared l => NProg l -> NProg l
simplifyCaseN prog = prog { nProgFuns = simplifyCaseFunMap m $ nProgFuns prog }
    where
    m = setVars VarMut (boundVars $ nProgInputs prog) $ setVars VarLet (freeVarsFunMap $ nProgFuns prog) M.empty

simplifyCaseNFull :: IsDesugared l => NProg l -> NProg l
simplifyCaseNFull prog = prog { nProgFuns = simplifyCaseFunMap m $ nProgFuns prog }
    where
    m = setVars VarLet (boundVars $ nProgInputs prog) $ setVars VarLet (freeVarsFunMap $ nProgFuns prog) M.empty

setVars :: VarKind -> S.Set TH.Name -> KindMap -> KindMap
setVars t s m = M.fromList (map (, t) $ S.toList s) `M.union` m

hasAssigns :: IsDesugared l => TH.Name -> Stmt l -> Bool
hasAssigns n (SIf _ st sf) = hasAssigns n st || hasAssigns n sf
hasAssigns n (SCase _ cs) = any (hasAssigns n . snd) cs
hasAssigns _ (SRet _) = False
hasAssigns n (SBlock ss) = any (hasAssigns n) ss
hasAssigns _ (SYield _) = False
hasAssigns n (SYieldT _ s) = hasAssigns n s
hasAssigns n (SAssign n' _) = n == n'
hasAssigns n (SFun fs s) = hasAssigns n s || any (\(p, s') -> not (n `S.member` patBound (freeVarsPat p)) && hasAssigns n s') fs
hasAssigns _ SNop = False
hasAssigns n (SLet _ n' _ s) = n /= n' && hasAssigns n s

