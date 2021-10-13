{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module FSM.Process.CleanUnusedConts(cleanUnusedConts) where

import FSM.Lang
import Prelude
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

decodeApp :: [TH.Exp] -> TH.Exp -> Maybe (TH.Name, [TH.Exp])
decodeApp es (TH.ConE n) = Just (n, es)
decodeApp es (TH.AppE e1 e2) = decodeApp (e2:es) e1
decodeApp _  _ = Nothing

pattern AConE :: TH.Name -> [TH.Exp] -> TH.Exp
pattern AConE n es <- (decodeApp [] -> Just (n, es)) where
    AConE n es = foldl TH.AppE (TH.ConE n) es

class CleanConts a where
    cleanConts :: S.Set TH.Name -> a -> a

instance CleanConts TH.Exp where
    cleanConts _  (TH.VarE n) = TH.VarE n
    cleanConts ns (TH.ConE n) | n `S.member` ns = tupE []
                              | otherwise = TH.ConE n
    cleanConts _  (TH.LitE l) = TH.LitE l
    cleanConts ns (AConE n es) | n `S.member` ns = tupE (cleanConts ns es)
    cleanConts ns (TH.AppE e1 e2) = TH.AppE (cleanConts ns e1) (cleanConts ns e2)
    cleanConts ns (TH.AppTypeE e t) = TH.AppTypeE (cleanConts ns e) t
    cleanConts ns (TH.InfixE me1 e me2) = TH.InfixE (cleanConts ns me1) e (cleanConts ns me2)
    cleanConts ns (TH.UInfixE e1 e e2) = TH.UInfixE (cleanConts ns e1) e (cleanConts ns e2)
    cleanConts ns (TH.ParensE e) = TH.ParensE (cleanConts ns e)
    cleanConts ns (TH.LamE ps e) = TH.LamE (cleanConts ns ps) (cleanConts ns e)
    cleanConts ns (TH.LamCaseE ms) = TH.LamCaseE (cleanConts ns ms)
    cleanConts ns (TH.TupE es) = TH.TupE (cleanConts ns es)
    cleanConts ns (TH.UnboxedTupE es) = TH.UnboxedTupE (cleanConts ns es)
    cleanConts ns (TH.UnboxedSumE e al ar) = TH.UnboxedSumE (cleanConts ns e) al ar
    cleanConts ns (TH.CondE e e1 e2) = TH.CondE (cleanConts ns e) (cleanConts ns e1) (cleanConts ns e2)
    cleanConts ns (TH.MultiIfE ges) = TH.MultiIfE (cleanConts ns ges)
    cleanConts ns (TH.LetE ds e) = TH.LetE (cleanConts ns ds) (cleanConts ns e)
    cleanConts ns (TH.CaseE e ms) = TH.CaseE (cleanConts ns e) (cleanConts ns ms)
    cleanConts ns (TH.DoE ss) = TH.DoE (cleanConts ns ss)
    cleanConts ns (TH.MDoE ss) = TH.MDoE (cleanConts ns ss)
    cleanConts ns (TH.CompE ss) = TH.CompE (cleanConts ns ss)
    cleanConts ns (TH.ArithSeqE r) = TH.ArithSeqE (cleanConts ns r)
    cleanConts ns (TH.ListE es) = TH.ListE (cleanConts ns es)
    cleanConts ns (TH.SigE e t) = TH.SigE (cleanConts ns e) t
    cleanConts ns (TH.RecConE n fes) = TH.RecConE n (map (id *** cleanConts ns) fes)
    cleanConts ns (TH.RecUpdE e fes) = TH.RecUpdE (cleanConts ns e) (map (id *** cleanConts ns) fes)
    cleanConts ns (TH.StaticE e) = TH.StaticE (cleanConts ns e)
    cleanConts _  (TH.UnboundVarE n) = TH.UnboundVarE n
    cleanConts _  (TH.LabelE n) = TH.LabelE n
    cleanConts _  (TH.ImplicitParamVarE n) = TH.ImplicitParamVarE n

instance CleanConts TH.Range where
    cleanConts ns (TH.FromR e) = TH.FromR (cleanConts ns e)
    cleanConts ns (TH.FromThenR e1 e2) = TH.FromThenR (cleanConts ns e1) (cleanConts ns e2)
    cleanConts ns (TH.FromToR e1 e2) = TH.FromToR (cleanConts ns e1) (cleanConts ns e2)
    cleanConts ns (TH.FromThenToR e1 e2 e3) = TH.FromThenToR (cleanConts ns e1) (cleanConts ns e2) (cleanConts ns e3)

instance CleanConts TH.Clause where
    cleanConts ns (TH.Clause ps b ds) = TH.Clause (cleanConts ns ps) (cleanConts ns b) (cleanConts ns ds)

instance CleanConts TH.Pat where
    cleanConts _  (TH.LitP l) = TH.LitP l
    cleanConts _  (TH.VarP n) = TH.VarP n
    cleanConts ns (TH.TupP ps) = TH.TupP (cleanConts ns ps)
    cleanConts ns (TH.UnboxedTupP ps) = TH.UnboxedTupP (cleanConts ns ps)
    cleanConts ns (TH.UnboxedSumP p al ar) = TH.UnboxedSumP (cleanConts ns p) al ar
    cleanConts ns (TH.ConP n ps) | n `S.member` ns = tupP ps
                                 | otherwise = TH.ConP n (cleanConts ns ps)
    cleanConts ns (TH.InfixP p1 n p2) = TH.InfixP (cleanConts ns p1) n (cleanConts ns p2)
    cleanConts ns (TH.UInfixP p1 n p2) = TH.UInfixP (cleanConts ns p1) n (cleanConts ns p2)
    cleanConts ns (TH.ParensP p) = TH.ParensP (cleanConts ns p)
    cleanConts ns (TH.TildeP p) = TH.TildeP (cleanConts ns p)
    cleanConts ns (TH.BangP p) = TH.BangP (cleanConts ns p)
    cleanConts ns (TH.AsP n p) = TH.AsP n (cleanConts ns p)
    cleanConts _  (TH.WildP) = TH.WildP
    cleanConts ns (TH.RecP n fps) = TH.RecP n (map (id *** cleanConts ns) fps)
    cleanConts ns (TH.ListP ps) = TH.ListP (cleanConts ns ps)
    cleanConts ns (TH.SigP p t) = TH.SigP (cleanConts ns p) t
    cleanConts ns (TH.ViewP e p) = TH.ViewP (cleanConts ns e) (cleanConts ns p)

instance CleanConts TH.Stmt where
    cleanConts ns (TH.BindS p e) = TH.BindS (cleanConts ns p) (cleanConts ns e)
    cleanConts ns (TH.LetS ds) = TH.LetS (cleanConts ns ds)
    cleanConts ns (TH.NoBindS e) = TH.NoBindS (cleanConts ns e)
    cleanConts ns (TH.ParS sss) = TH.ParS (cleanConts ns sss)
    cleanConts ns (TH.RecS ss) = TH.RecS (cleanConts ns ss)

instance CleanConts TH.Dec where
    cleanConts ns (TH.ValD p b ds) = TH.ValD (cleanConts ns p) (cleanConts ns b) (cleanConts ns ds)
    cleanConts ns (TH.FunD n cs) = TH.FunD n (cleanConts ns cs)

instance CleanConts TH.Body where
    cleanConts ns (TH.NormalB e) = TH.NormalB (cleanConts ns e)
    cleanConts ns (TH.GuardedB ges) = TH.GuardedB (cleanConts ns ges)

instance CleanConts TH.Guard where
    cleanConts ns (TH.NormalG e) = TH.NormalG (cleanConts ns e)
    cleanConts ns (TH.PatG ss) = TH.PatG (cleanConts ns ss)

instance CleanConts TH.Match where
    cleanConts ns (TH.Match p b ds) = TH.Match (cleanConts ns p) (cleanConts ns b) (cleanConts ns ds)

instance CleanConts VStmt where
    cleanConts ns (VExp e) = VExp $ cleanConts ns e
    cleanConts ns (VCall n e) = VCall n $ cleanConts ns e

instance CleanConts Stmt where
    cleanConts ns (SLet t n vs s) = SLet t n (cleanConts ns vs) (cleanConts ns s)
    cleanConts ns (SAssign n vs) = SAssign n (cleanConts ns vs)
    cleanConts ns (SYield e) = SYield (cleanConts ns e)
    cleanConts ns (SRet vs) = SRet (cleanConts ns vs)
    cleanConts ns (SFun fs s) = SFun (cleanContsFunMap ns fs) (cleanConts ns s)
    cleanConts ns (SBlock ss) = SBlock (cleanConts ns ss)
    cleanConts ns (SIf e st sf) = SIf (cleanConts ns e) (cleanConts ns st) (cleanConts ns sf)
    cleanConts ns (SCase e cs) = SCase (cleanConts ns e) (cleanConts ns cs)
    cleanConts _  SNop = SNop

instance CleanConts a => CleanConts (Maybe a) where
    cleanConts ns = fmap (cleanConts ns)

instance CleanConts a => CleanConts [a] where
    cleanConts ns = fmap (cleanConts ns)

instance (CleanConts a, CleanConts b) => CleanConts (a, b) where
    cleanConts ns = cleanConts ns *** cleanConts ns

cleanContsFunMap :: S.Set TH.Name -> FunMap -> FunMap
cleanContsFunMap ns = M.map (cleanConts ns *** cleanConts ns)

cleanUnusedConts :: NProg -> NProg
cleanUnusedConts prog = prog {
        nProgFuns = cleanContsFunMap ns $ nProgFuns prog,
        nProgInitParam = cleanConts ns $ nProgInitParam prog,
        nProgConts = M.filter ((> 1) . M.size) $ nProgConts prog
    }
    where
    ns = S.unions $ map (M.keysSet) $ M.elems $ nProgConts prog

