module FSM.Process.SimplifyCase(simplifyCase) where

import FSM.Lang
import Prelude
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Maybe
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

simplifyCase1 :: TH.Exp -> TH.Pat -> Stmt -> MMaybe Stmt
simplifyCase1 e (TH.VarP n) s = MJust $ SLet VarLet n (VExp e) s
simplifyCase1 (TH.LitE l) (TH.LitP l') s | l == l' = MJust s
simplifyCase1 e (TH.LitP _) _ | matchableExp e = MNo
simplifyCase1 (TH.TupE mes) (TH.TupP ps) s
    | length mes == length ps, all isJust mes = foldM (flip $ uncurry simplifyCase1) s $ zip (map fromJust mes) ps
simplifyCase1 e (TH.TupP _) _ | matchableExp e = MNo
simplifyCase1 e (TH.ConP n ps) s 
    | Just (n', es) <- conExp [] e, n == n', length es == length ps = foldM (flip $ uncurry simplifyCase1) s $ zip es ps
simplifyCase1 e (TH.ConP _ _) _ | matchableExp e = MNo
simplifyCase1 _ _ _ = MNoNo

simplifyCaseDo :: TH.Exp -> [(TH.Pat, Stmt)] -> Stmt
simplifyCaseDo e cs = mmaybe (SCase e cs) id $ msum (map (uncurry $ simplifyCase1 e) cs)

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

