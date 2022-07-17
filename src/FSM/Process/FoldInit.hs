{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>
|-}
module FSM.Process.FoldInit(foldInit) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import FSM.Process.SimplifyCase
import FSM.Util.MMaybe

myLet :: TH.Pat -> TH.Exp -> TH.Exp -> TH.Exp
myLet p e e' = TH.LetE [TH.ValD p (TH.NormalB e) []] e'

maybeMkLet :: M.Map TH.Name VarKind -> TH.Pat -> TH.Exp -> TH.Exp -> TH.Exp
maybeMkLet m p e e' = mmaybe (myLet p e e') id $ simplifyCaseGen (mkLetGen (const id) (\n -> myLet (TH.VarP n)) m) e p e'

doFoldInit :: IsDesugared l => S.Set TH.Name -> NProg l -> NProg l
doFoldInit is prog
    | Just (p, SRet (VCall n e)) <- M.lookup (nProgInit prog) (nProgFuns prog),
        S.null (freeVars e `S.intersection` is) =
            doFoldInit is $ prog { 
                nProgInit = n, 
                nProgInitParam = maybeMkLet vmap p (nProgInitParam prog) e
            }
    | otherwise = prog
    where
    vmap = M.fromList (map (,VarMut) $ S.toList $ boundVars $ nProgInputs prog) `M.union` M.fromList (map (,VarLet) $ S.toList $ freeVarsFunMap (nProgFuns prog) `S.union` freeVars (nProgInitParam prog))

foldInit :: IsDesugared l => NProg l -> NProg l
foldInit prog = doFoldInit (boundVars $ nProgInputs prog) prog

