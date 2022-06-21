{-# LANGUAGE FlexibleContexts #-}
module FSM.Process.HoistFromConstructors(hoistFromConstructors) where

import FSM.Lang
import FSM.FreeVars
import FSM.Util.MonadRefresh
import Prelude
import Data.Maybe
import Data.Monoid
import Control.Monad.Writer
import qualified Language.Haskell.TH as TH

matchApp :: TH.Exp -> (TH.Exp, [TH.Exp])
matchApp e = m e [] where
    m (TH.AppE e1 e2) es = m e1 (e2:es)
    m e0 es = (e0, es)

buildApp :: TH.Exp -> [TH.Exp] -> TH.Exp
buildApp e = foldl TH.AppE e

hoistExp :: (MonadRefresh m, IsLowered l, MonadWriter (Endo (Stmt l)) m)
         => Bool -> TH.Exp -> m TH.Exp
hoistExp _ (matchApp -> (e@(TH.ConE _), es)) = buildApp e <$> mapM (hoistExp True) es
hoistExp _ (TH.TupE mes) | all isJust mes, es <- map fromJust mes = TH.TupE . map Just <$> mapM (hoistExp True) es
hoistExp _ (TH.InfixE (Just e1) e@(TH.ConE _) (Just e2)) = TH.InfixE <$> (Just <$> hoistExp True e1) <*> pure e <*> (Just <$> hoistExp True e2)
hoistExp _ (TH.UInfixE e1 e@(TH.ConE _) e2) = TH.UInfixE <$> hoistExp True e1 <*> pure e <*> hoistExp True e2
hoistExp b (TH.ParensE e) = TH.ParensE <$> hoistExp b e
hoistExp True e = do
    v <- makeName "v"
    tell $ Endo $ SLet VarLet v (VExp e)
    return $ TH.VarE v
hoistExp False e = return e

hoistStmt :: (MonadRefresh m, IsLowered l) => Stmt l -> m (Stmt l)
hoistStmt (SLet k n (VExp e) s) = do
    (e', Endo sf) <- runWriterT $ hoistExp False e
    if isConstantExpr e'
    then sf <$> hoistStmt (substSingle n e' s)
    else sf . SLet k n (VExp e') <$> hoistStmt s
hoistStmt (SLet _ _ (VCall _ _) _) = error "No let calls in lowered language"
hoistStmt (SYieldT e s) = SYieldT e <$> hoistStmt s
hoistStmt (SRet vs) = return $ SRet vs
hoistStmt (SIf e st sf) = SIf e <$> hoistStmt st <*> hoistStmt sf
hoistStmt (SCase e cs) = SCase e <$> mapM hoistCase cs

hoistCase :: (MonadRefresh m, IsLowered l) => (TH.Pat, Stmt l) -> m (TH.Pat, Stmt l)
hoistCase (p, s) = (p,) <$> hoistStmt s

hoistFunMap :: (MonadRefresh m, IsLowered l) => FunMap l -> m (FunMap l)
hoistFunMap = mapM hoistCase

hoistFromConstructors :: (MonadRefresh m, IsLowered l) => NProg l -> m (NProg l)
hoistFromConstructors prog = do
    prog' <- hoistFunMap $ nProgFuns prog
    return $ prog { nProgFuns = prog' }

