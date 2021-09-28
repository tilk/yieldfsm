module FSM.Process.SimplifyCase(simplifyCase, simplifyCaseN) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

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

data MMaybe a = MJust a | MNo | MNoNo

instance Functor MMaybe where
    fmap f (MJust a) = MJust (f a)
    fmap _ MNo = MNo
    fmap _ MNoNo = MNoNo

instance Applicative MMaybe where
    pure = MJust
    MJust f <*> MJust a = MJust (f a)
    MNoNo <*> _ = MNoNo
    _ <*> MNoNo = MNoNo
    MNo <*> _ = MNo
    _ <*> MNo = MNo

instance Monad MMaybe where
    MJust a >>= f = f a
    MNo >>= _ = MNo
    MNoNo >>= _ = MNoNo

instance Alternative MMaybe where
    empty = MNo
    MNoNo <|> _ = MNoNo
    _ <|> MNoNo = MNoNo
    (MJust a) <|> _ = MJust a
    MNo <|> m = m

instance MonadPlus MMaybe

mmaybe :: b -> (a -> b) -> MMaybe a -> b
mmaybe _ f (MJust a) = f a
mmaybe a _ MNo = a
mmaybe a _ MNoNo = a

simplifyCase1 :: M.Map TH.Name VarKind -> TH.Exp -> TH.Pat -> Stmt -> MMaybe Stmt
simplifyCase1 m e (TH.VarP n) s = MJust $ mkLet m VarLet n (VExp e) s
simplifyCase1 _ (TH.LitE l) (TH.LitP l') s | l == l' = MJust s
simplifyCase1 _ e (TH.LitP _) _ | matchableExp e = MNo
simplifyCase1 m (TH.TupE mes) (TH.TupP ps) s
    | length mes == length ps, all isJust mes = foldM (flip $ uncurry $ simplifyCase1 m) s $ zip (map fromJust mes) ps
simplifyCase1 _ e (TH.TupP _) _ | matchableExp e = MNo
simplifyCase1 m e (TH.ConP n ps) s 
    | Just (n', es) <- conExp [] e, n == n', length es == length ps = foldM (flip $ uncurry $ simplifyCase1 m) s $ zip es ps
simplifyCase1 _ e (TH.ConP _ _) _ | matchableExp e = MNo
simplifyCase1 _ _ _ _ = MNoNo

simplifyCaseDo :: M.Map TH.Name VarKind -> TH.Exp -> [(TH.Pat, Stmt)] -> Stmt
simplifyCaseDo m e cs = mmaybe (SCase e (map (simplifyCaseCase m) cs)) id $ msum (map (uncurry $ simplifyCase1 m e) cs)

simplifyCaseCase :: M.Map TH.Name VarKind -> (TH.Pat, Stmt) -> (TH.Pat, Stmt)
simplifyCaseCase m (p, s) = (p, simplifyCaseStmt (setVars VarLet (boundVars p) m) s)

mkLet :: M.Map TH.Name VarKind -> VarKind -> TH.Name -> VStmt -> Stmt -> Stmt
mkLet m t n vs s
    | VExp e@(TH.VarE n') <- vs, Just VarLet <- M.lookup n' m, canSubst t = simplifyCaseStmt m $ substSingle n e s
    | VExp e <- vs, isConstantExpr e, canSubst t = simplifyCaseStmt m $ substSingle n e s
    | otherwise = SLet t n vs $ simplifyCaseStmt (M.insert n t m) s
    where
    canSubst VarLet = True
    canSubst VarMut = not $ hasAssigns n s

simplifyCaseStmt :: M.Map TH.Name VarKind -> Stmt -> Stmt
simplifyCaseStmt m   (SCase e cs) = simplifyCaseDo m e cs
simplifyCaseStmt _ e@(SRet _) = e
simplifyCaseStmt _ e@(SAssign _ _) = e
simplifyCaseStmt _ e@(SYield _) = e
simplifyCaseStmt m   (SFun fs s) = SFun (simplifyCaseFunMap m fs) (simplifyCaseStmt m s)
simplifyCaseStmt m   (SBlock ss) = SBlock $ map (simplifyCaseStmt m) ss
simplifyCaseStmt m   (SIf e st sf) = SIf e (simplifyCaseStmt m st) (simplifyCaseStmt m sf)
simplifyCaseStmt _ e@SNop = e
simplifyCaseStmt m   (SLet t n vs s) = mkLet m t n vs s

simplifyCaseFunMap :: M.Map TH.Name VarKind -> FunMap -> FunMap
simplifyCaseFunMap m = M.map (simplifyCaseCase m)

simplifyCase :: Prog -> Prog
simplifyCase prog = prog { progBody = simplifyCaseStmt m $ progBody prog }
    where
    m = setVars VarMut (boundVars $ progInputs prog) $ setVars VarLet (freeVars $ progBody prog) M.empty

simplifyCaseN :: NProg -> NProg
simplifyCaseN prog = prog { nProgFuns = simplifyCaseFunMap m $ nProgFuns prog }
    where
    m = setVars VarMut (boundVars $ nProgInputs prog) $ setVars VarLet (freeVarsFunMap $ nProgFuns prog) M.empty

setVars :: VarKind -> S.Set TH.Name -> M.Map TH.Name VarKind -> M.Map TH.Name VarKind
setVars t s m = M.fromList (map (, t) $ S.toList s) `M.union` m

hasAssigns :: TH.Name -> Stmt -> Bool
hasAssigns n (SIf _ st sf) = hasAssigns n st || hasAssigns n sf
hasAssigns n (SCase _ cs) = any (hasAssigns n . snd) cs
hasAssigns _ (SRet _) = False
hasAssigns n (SBlock ss) = any (hasAssigns n) ss
hasAssigns _ (SYield _) = False
hasAssigns n (SAssign n' _) = n == n'
hasAssigns n (SFun fs s) = hasAssigns n s || any (\(p, s') -> not (n `S.member` patBound (freeVarsPat p)) && hasAssigns n s') fs
hasAssigns _ SNop = False
hasAssigns n (SLet _ n' _ s) = n /= n' && hasAssigns n s

