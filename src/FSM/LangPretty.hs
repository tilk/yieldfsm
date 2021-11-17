module FSM.LangPretty(prettyProgHPJ, prettyNProgHPJ) where

import FSM.Lang
import Prelude hiding ((<>))
import Language.Haskell.TH.PprLib
import qualified Text.PrettyPrint as HPJ
import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M

prettyKeyword :: String -> Doc
prettyKeyword s = text s

prettyStmt :: Stmt -> Doc
prettyStmt SNop = prettyKeyword "nop"
prettyStmt (SLet t n vs s) = vcat [prettyKeyword (kw t) <+> TH.ppr n <+> prettyKeyword "=" <+> prettyVStmt vs, prettyStmt s]
    where
    kw VarLet = "let"
    kw VarMut = "var"
prettyStmt (SAssign n e) = TH.ppr n <+> prettyKeyword "=" <+> TH.ppr e
prettyStmt (SYield e) = prettyKeyword "yield" <+> TH.ppr e
prettyStmt (SRet vs) = prettyKeyword "ret" <+> prettyVStmt vs
prettyStmt (SFun fs s) = vcat $ map f (M.toList fs) ++ [prettyStmt s]
    where f (n, (p, s')) = vcat [prettyKeyword "fun" <+> TH.ppr n <+> TH.ppr p <> prettyKeyword ":", nest 4 $ prettyStmt s']
prettyStmt (SIf e st sf) = vcat [prettyKeyword "if" <+> TH.ppr e <> prettyKeyword ":", nest 4 (prettyStmt st), prettyKeyword "else:", nest 4 (prettyStmt sf)]
prettyStmt (SBlock ss) = vcat (map prettyStmt ss)
prettyStmt (SCase e cs) = vcat [prettyKeyword "case" <+> TH.ppr e, vcat (map f cs)]
    where f (p, s) = vcat [prettyKeyword "|" <+> TH.ppr p <> prettyKeyword ":", nest 4 $ prettyStmt s]

prettyVStmt :: VStmt -> Doc
prettyVStmt (VExp e) = TH.ppr e
prettyVStmt (VCall n e) = prettyKeyword "call" <+> TH.ppr n <+> TH.ppr e

prettyInputs :: Maybe TH.Pat -> Doc
prettyInputs Nothing = empty
prettyInputs (Just p) = prettyKeyword "input" <+> TH.ppr p

prettyNProg :: NProg -> Doc
prettyNProg np = vcat [prettyInputs (nProgInputs np), prettyStmt (SFun (nProgFuns np) (SRet (VCall (nProgInit np) (nProgInitParam np))))]

prettyProg :: Prog -> Doc
prettyProg np = vcat [prettyInputs (progInputs np), prettyStmt (progBody np)]

prettyNProgHPJ :: NProg -> HPJ.Doc
prettyNProgHPJ = to_HPJ_Doc . prettyNProg

prettyProgHPJ :: Prog -> HPJ.Doc
prettyProgHPJ = to_HPJ_Doc . prettyProg

