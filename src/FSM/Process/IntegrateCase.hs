module FSM.Process.IntegrateCase(integrateCase) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Arrow
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

unionCFunc :: Maybe TH.Pat -> Maybe TH.Pat -> Maybe TH.Pat
unionCFunc _ _ = Nothing

unionC :: M.Map TH.Name (Maybe TH.Pat) -> M.Map TH.Name (Maybe TH.Pat) -> M.Map TH.Name (Maybe TH.Pat)
unionC = M.unionWith unionCFunc

unionsC :: Foldable f => f (M.Map TH.Name (Maybe TH.Pat)) -> M.Map TH.Name (Maybe TH.Pat)
unionsC = M.unionsWith unionCFunc

cantFromSetC :: S.Set TH.Name -> M.Map TH.Name (Maybe TH.Pat)
cantFromSetC = M.fromSet (const Nothing)

canIntegrateCaseStmt :: S.Set TH.Name -> Stmt -> M.Map TH.Name (Maybe TH.Pat)
canIntegrateCaseStmt ns (SLet _ _ vs s) = canIntegrateCaseStmt ns s `unionC` cantFromSetC (freeVars vs `S.intersection` ns)
canIntegrateCaseStmt ns (SAssign _ vs) = cantFromSetC (freeVars vs `S.intersection` ns)
canIntegrateCaseStmt ns (SYield e) = cantFromSetC (freeVars e `S.intersection` ns)
canIntegrateCaseStmt ns (SRet vs) = cantFromSetC (freeVars vs `S.intersection` ns)
canIntegrateCaseStmt ns (SBlock ss) = unionsC $ map (canIntegrateCaseStmt ns) ss
canIntegrateCaseStmt ns (SIf e st sf) = canIntegrateCaseStmt ns st `unionC` canIntegrateCaseStmt ns sf `unionC` cantFromSetC (freeVars e `S.intersection` ns)
canIntegrateCaseStmt ns (SCase e ps)
    | TH.VarE n <- e, [(p, s)] <- ps, n `S.member` ns, S.null (freeVars p) = f (p, s) `unionC` M.singleton n (Just p)
    | otherwise = unionsC (map f ps) `unionC` cantFromSetC (freeVars e `S.intersection` ns)
        where
        f (p, s) = canIntegrateCaseStmt ns s `unionC` cantFromSetC (freeVars p `S.intersection` ns)
canIntegrateCaseStmt _  (SFun _ _) = error "Not in lambda-lifted form"
canIntegrateCaseStmt _   SNop = M.empty

integrateCaseStmt :: S.Set TH.Name -> Stmt -> Stmt
integrateCaseStmt ns   (SLet t n vs s) = SLet t n vs $ integrateCaseStmt (S.delete n ns) s
integrateCaseStmt _  s@(SAssign _ _) = s
integrateCaseStmt _  s@(SYield _) = s
integrateCaseStmt _  s@(SRet _) = s
integrateCaseStmt ns   (SBlock ss) = SBlock $ map (integrateCaseStmt ns) ss
integrateCaseStmt ns   (SIf e st sf) = SIf e (integrateCaseStmt ns st) (integrateCaseStmt ns sf)
integrateCaseStmt ns   (SCase e ps)
    | TH.VarE n <- e, [(_, s)] <- ps, n `S.member` ns = integrateCaseStmt ns s
    | otherwise = SCase e $ map (id *** integrateCaseStmt ns) ps
integrateCaseStmt _    (SFun _ _) = error "Not in lambda-lifted form"
integrateCaseStmt _  s@(SNop) = s

integrateCaseFunMap :: FunMap -> FunMap
integrateCaseFunMap fs = M.map f fs
    where
    f (p, s) = (substPat cm p, integrateCaseStmt (M.keysSet cm) s)
        where
        cm = M.map fromJust $ M.filter isJust $ canIntegrateCaseStmt (boundVars p `S.difference` boundAsVars p) s

integrateCase :: NProg -> NProg
integrateCase prog = prog { nProgFuns = integrateCaseFunMap $ nProgFuns prog }

