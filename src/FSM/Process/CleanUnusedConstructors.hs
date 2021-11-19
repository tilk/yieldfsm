module FSM.Process.CleanUnusedConstructors(cleanUnusedConstructors) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Arrow
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

class FreeConstructors a where
    freeConstructorsPat :: a -> PatFV S.Set

instance FreeConstructors TH.Exp where
    freeConstructorsPat (TH.VarE _) = mempty
    freeConstructorsPat (TH.ConE n) = patFreeSingleton n
    freeConstructorsPat (TH.LitE _) = mempty
    freeConstructorsPat (TH.AppE e1 e2) = freeConstructorsPat e1 <> freeConstructorsPat e2
    freeConstructorsPat (TH.AppTypeE e _ ) = freeConstructorsPat e
    freeConstructorsPat (TH.InfixE me1 e me2) = freeConstructorsPat e <> freeConstructorsPat me1 <> freeConstructorsPat me2
    freeConstructorsPat (TH.UInfixE e1 e e2) = freeConstructorsPat e <> freeConstructorsPat e1 <> freeConstructorsPat e2
    freeConstructorsPat (TH.ParensE e) = freeConstructorsPat e
    freeConstructorsPat (TH.LamE ps e) = freeConstructorsPat e <> mconcat (map freeConstructorsPat ps)
    freeConstructorsPat (TH.LamCaseE ms) = freeConstructorsPat ms
    freeConstructorsPat (TH.TupE es) = freeConstructorsPat es
    freeConstructorsPat (TH.UnboxedTupE es) = freeConstructorsPat es
    freeConstructorsPat (TH.UnboxedSumE e _ _) = freeConstructorsPat e
    freeConstructorsPat (TH.CondE e e1 e2) = freeConstructorsPat e <> freeConstructorsPat e1 <> freeConstructorsPat e2
    freeConstructorsPat (TH.MultiIfE ges) = mconcat $ map freeConstructorsPat ges
    freeConstructorsPat (TH.LetE ds e) = freeConstructorsPat e <> freeConstructorsPat ds
    freeConstructorsPat (TH.CaseE e ms) = freeConstructorsPat e <> freeConstructorsPat ms
    freeConstructorsPat (TH.DoE ss) = freeConstructorsPat ss
    freeConstructorsPat (TH.MDoE ss) = freeConstructorsPat ss
    freeConstructorsPat (TH.CompE ss) = freeConstructorsPat ss
    freeConstructorsPat (TH.ArithSeqE r) = freeConstructorsPat r
    freeConstructorsPat (TH.ListE es) = freeConstructorsPat es
    freeConstructorsPat (TH.SigE e _) = freeConstructorsPat e
    freeConstructorsPat (TH.RecConE _ fes) = mconcat $ map (freeConstructorsPat . snd) fes
    freeConstructorsPat (TH.RecUpdE e fes) = freeConstructorsPat e <> (mconcat $ map (freeConstructorsPat . snd) fes)
    freeConstructorsPat (TH.StaticE e) = freeConstructorsPat e
    freeConstructorsPat (TH.UnboundVarE _) = mempty
    freeConstructorsPat (TH.LabelE _) = mempty
    freeConstructorsPat (TH.ImplicitParamVarE _) = mempty

instance FreeConstructors TH.Range where
    freeConstructorsPat (TH.FromR e) = freeConstructorsPat e
    freeConstructorsPat (TH.FromThenR e1 e2) = freeConstructorsPat e1 <> freeConstructorsPat e2
    freeConstructorsPat (TH.FromToR e1 e2) = freeConstructorsPat e1 <> freeConstructorsPat e2
    freeConstructorsPat (TH.FromThenToR e1 e2 e3) = freeConstructorsPat e1 <> freeConstructorsPat e2 <> freeConstructorsPat e3

instance FreeConstructors TH.Clause where
    freeConstructorsPat (TH.Clause ps b ds) = freeConstructorsPat b <> freeConstructorsPat ds <> freeConstructorsPat ps

instance FreeConstructors TH.Pat where
    freeConstructorsPat (TH.LitP _) = mempty
    freeConstructorsPat (TH.VarP _) = mempty
    freeConstructorsPat (TH.TupP ps) = freeConstructorsPat ps
    freeConstructorsPat (TH.UnboxedTupP ps) = freeConstructorsPat ps
    freeConstructorsPat (TH.UnboxedSumP p _ _) = freeConstructorsPat p
    freeConstructorsPat (TH.ConP n ps) = patSingleton n <> freeConstructorsPat ps
    freeConstructorsPat (TH.InfixP p1 _ p2) = freeConstructorsPat p1 <> freeConstructorsPat p2
    freeConstructorsPat (TH.UInfixP p1 _ p2) = freeConstructorsPat p1 <> freeConstructorsPat p2
    freeConstructorsPat (TH.ParensP p) = freeConstructorsPat p
    freeConstructorsPat (TH.TildeP p) = freeConstructorsPat p
    freeConstructorsPat (TH.BangP p) = freeConstructorsPat p
    freeConstructorsPat (TH.AsP _ p) = freeConstructorsPat p
    freeConstructorsPat (TH.WildP) = mempty
    freeConstructorsPat (TH.RecP _ fps) = mconcat $ map (freeConstructorsPat . snd) fps
    freeConstructorsPat (TH.ListP ps) = freeConstructorsPat ps
    freeConstructorsPat (TH.SigP p _) = freeConstructorsPat p
    freeConstructorsPat (TH.ViewP e p) = freeConstructorsPat p <> freeConstructorsPat e

instance FreeConstructors TH.Stmt where
    freeConstructorsPat (TH.BindS p e) = freeConstructorsPat p <> freeConstructorsPat e
    freeConstructorsPat (TH.LetS ds) = mconcat $ map freeConstructorsPat ds
    freeConstructorsPat (TH.NoBindS e) = freeConstructorsPat e
    freeConstructorsPat (TH.ParS sss) = mconcat $ map freeConstructorsPat sss
    freeConstructorsPat (TH.RecS ss) = freeConstructorsPat ss

instance FreeConstructors TH.Dec where
    freeConstructorsPat (TH.ValD p b ds) = freeConstructorsPat b <> freeConstructorsPat ds <> freeConstructorsPat p
    freeConstructorsPat (TH.FunD _ cs) = freeConstructorsPat cs

instance FreeConstructors TH.Body where
    freeConstructorsPat (TH.NormalB e) = freeConstructorsPat e
    freeConstructorsPat (TH.GuardedB ges) = mconcat $ map freeConstructorsPat ges

instance FreeConstructors TH.Guard where
    freeConstructorsPat (TH.NormalG e) = freeConstructorsPat e
    freeConstructorsPat (TH.PatG ss) = freeConstructorsPat ss

instance FreeConstructors TH.Match where
    freeConstructorsPat (TH.Match p b ds) = freeConstructorsPat b <> freeConstructorsPat ds <> freeConstructorsPat p

instance FreeConstructors VStmt where
    freeConstructorsPat (VExp e) = freeConstructorsPat e
    freeConstructorsPat (VCall _ e) = freeConstructorsPat e

instance IsDesugared l => FreeConstructors (Stmt l) where
    freeConstructorsPat (SLet _ _ vs s) = freeConstructorsPat vs <> freeConstructorsPat s
    freeConstructorsPat (SAssign _ vs) = freeConstructorsPat vs
    freeConstructorsPat (SYield e) = freeConstructorsPat e
    freeConstructorsPat (SYieldT e s) = freeConstructorsPat e <> freeConstructorsPat s
    freeConstructorsPat (SRet vs) = freeConstructorsPat vs
    freeConstructorsPat (SFun fs s) = freeConstructorsPat s <> freeConstructorsPatFunMap fs
    freeConstructorsPat (SBlock ss) = freeConstructorsPat ss
    freeConstructorsPat (SIf e st sf) = freeConstructorsPat e <> freeConstructorsPat st <> freeConstructorsPat sf
    freeConstructorsPat (SCase e cs) = freeConstructorsPat e <> freeConstructorsPat cs
    freeConstructorsPat SNop = mempty

instance FreeConstructors a => FreeConstructors (Maybe a) where
    freeConstructorsPat = maybe mempty id . fmap freeConstructorsPat

instance FreeConstructors a => FreeConstructors [a] where
    freeConstructorsPat = mconcat . fmap freeConstructorsPat

instance (FreeConstructors a, FreeConstructors b) => FreeConstructors (a, b) where
    freeConstructorsPat = uncurry (<>) . (freeConstructorsPat *** freeConstructorsPat)

freeConstructors :: FreeConstructors a => a -> S.Set TH.Name
freeConstructors = patFree . freeConstructorsPat

patConstructors :: FreeConstructors a => a -> S.Set TH.Name
patConstructors = patBound . freeConstructorsPat

freeConstructorsPatFunMap :: IsDesugared l => FunMap l -> PatFV S.Set
freeConstructorsPatFunMap = freeConstructorsPat . map snd . M.toList

freeConstructorsFunMap :: IsDesugared l => FunMap l -> S.Set TH.Name
freeConstructorsFunMap = patFree . freeConstructorsPatFunMap

-- todo: removing constructors from exps

removeConstructorsStmt :: IsDesugared l => S.Set TH.Name -> Stmt l -> Stmt l
removeConstructorsStmt con   (SLet t n vs s) = SLet t n vs (removeConstructorsStmt con s)
removeConstructorsStmt _   s@(SAssign _ _) = s
removeConstructorsStmt _   s@(SYield _) = s
removeConstructorsStmt con   (SYieldT e s) = SYieldT e (removeConstructorsStmt con s)
removeConstructorsStmt _   s@(SRet _) = s
removeConstructorsStmt con   (SFun fs s) = SFun (removeConstructorsFunMap con fs) (removeConstructorsStmt con s)
removeConstructorsStmt con   (SBlock ss) = SBlock $ map (removeConstructorsStmt con) ss
removeConstructorsStmt con   (SIf e st sf) = SIf e (removeConstructorsStmt con st) (removeConstructorsStmt con sf)
removeConstructorsStmt con   (SCase e cs) = SCase e $ map (id *** removeConstructorsStmt con) . filter (S.null . (`S.difference` con) . patConstructors . fst) $ cs
removeConstructorsStmt _   s@(SNop) = s

removeConstructorsFunMap :: IsDesugared l => S.Set TH.Name -> FunMap l -> FunMap l
removeConstructorsFunMap con = M.map (id *** removeConstructorsStmt con) . M.filter (S.null . (`S.difference` con) . patConstructors . fst)

cleanUnusedConstructors :: IsDesugared l => NProg l -> NProg l
cleanUnusedConstructors prog = prog { 
        nProgFuns = removeConstructorsFunMap con $ nProgFuns prog,
        nProgConts = M.map (M.filterWithKey (\n _ -> n `S.member` con)) $ nProgConts prog
    }
    where
    con = freeConstructorsFunMap (nProgFuns prog) <> freeConstructors (nProgInitParam prog)

