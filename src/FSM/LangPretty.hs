{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

The pretty-printer.
-}
module FSM.LangPretty(prettyProgHPJ, prettyNProgHPJ) where

import FSM.Lang
import Prelude hiding ((<>))
import Language.Haskell.TH.PprLib
import qualified Text.PrettyPrint as HPJ
import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M

prettyKeyword :: String -> Doc
prettyKeyword s = text s

prettyFun :: FunMap l -> Stmt l -> Doc
prettyFun fs s = vcat $ map f (M.toList fs) ++ [prettyStmt s]
    where f (n, (p, s')) = vcat [prettyKeyword "fun" <+> TH.ppr n <+> TH.ppr p <> prettyKeyword ":", nest 4 $ prettyStmt s']

prettyStmt :: Stmt l -> Doc
prettyStmt SNop = prettyKeyword "skip"
prettyStmt (SLet t n vs s) = vcat [prettyKeyword (kw t) <+> TH.ppr n <+> prettyKeyword "=" <+> prettyVStmt vs, prettyStmt s]
    where
    kw VarLet = "let"
    kw VarMut = "var"
prettyStmt (SAssign n e) = TH.ppr n <+> prettyKeyword "=" <+> TH.ppr e
prettyStmt (SYield e) = prettyKeyword "yield" <+> TH.ppr e
prettyStmt (SYieldT e s) = vcat [prettyKeyword "yield" <+> TH.ppr e, prettyStmt s]
prettyStmt (SRet vs) = prettyKeyword "ret" <+> prettyVStmt vs
prettyStmt (SFun fs s) = prettyFun fs s
prettyStmt (SIf e st sf) = vcat [prettyKeyword "if" <+> TH.ppr e <> prettyKeyword ":", nest 4 (prettyStmt st), prettyElse sf]
    where
    prettyElse (SIf e' st' sf') = vcat [prettyKeyword "elif" <+> TH.ppr e' <> prettyKeyword ":", nest 4 (prettyStmt st'), prettyElse sf']
    prettyElse s = vcat [prettyKeyword "else:", nest 4 (prettyStmt s)]
prettyStmt (SBlock ss) = vcat (map prettyStmt ss)
prettyStmt (SCase e cs) = vcat [prettyKeyword "case" <+> TH.ppr e, vcat (map f cs)]
    where f (p, s) = vcat [prettyKeyword "|" <+> TH.ppr p <> prettyKeyword ":", nest 4 $ prettyStmt s]
prettyStmt _ = error "unsupported statement for pretty-printing"

prettyVStmt :: VStmt -> Doc
prettyVStmt (VExp e) = TH.ppr e
prettyVStmt (VCall n e) = prettyKeyword "call" <+> TH.ppr n <+> TH.ppr e

prettyInputs :: Maybe TH.Pat -> Doc
prettyInputs Nothing = empty
prettyInputs (Just p) = prettyKeyword "input" <+> TH.ppr p

prettyNProg :: NProg l -> Doc
prettyNProg np = vcat [prettyInputs (nProgInputs np), prettyFun (nProgFuns np) (SRet (VCall (nProgInit np) (nProgInitParam np)))]

prettyProg :: Prog l -> Doc
prettyProg np = vcat [prettyInputs (progInputs np), prettyStmt (progBody np)]

-- | Pretty-printer for 'NProg'.
prettyNProgHPJ :: NProg l -> HPJ.Doc
prettyNProgHPJ = to_HPJ_Doc . prettyNProg

-- | Pretty-printer for 'Prog'.
prettyProgHPJ :: Prog l -> HPJ.Doc
prettyProgHPJ = to_HPJ_Doc . prettyProg

