module FSM.Process.FoldInit(foldInit) where

import Prelude
import FSM.Lang
import FSM.FreeVars
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

doFoldInit :: S.Set TH.Name -> NProg -> NProg
doFoldInit is prog | Just (p, SRet (VCall n e)) <- M.lookup (nProgInit prog) (nProgFuns prog),
                     S.null (freeVars e `S.intersection` is) =
                        doFoldInit is $ prog { 
                            nProgInit = n, 
                            nProgInitParam = TH.LetE [TH.ValD p (TH.NormalB $ nProgInitParam prog) []] e
                        }
                  | otherwise = prog

foldInit :: NProg -> NProg
foldInit prog = doFoldInit (boundVars $ nProgInputs prog) prog

