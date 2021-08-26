module FSM.LangPretty(prettyNProg) where

import FSM.Lang
import Prelude
import Data.Text.Prettyprint.Doc
import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M

prettyKeyword :: String -> Doc ann
prettyKeyword s = pretty s

prettyTH :: TH.Ppr a => a -> Doc ann
prettyTH n = pretty $ TH.pprint n

prettyStmt :: Stmt -> Doc ann
prettyStmt SNop = prettyKeyword "nop"
prettyStmt (SLet t n e s) = vcat [prettyKeyword (kw t) <+> prettyTH n <+> prettyKeyword "=" <+> prettyVStmt e, prettyStmt s]
    where
    kw VarLet = "let"
    kw VarMut = "var"
prettyStmt (SAssign n e) = prettyTH n <+> prettyKeyword "=" <+> prettyTH e
prettyStmt (SEmit e) = prettyKeyword "emit" <+> prettyTH e
prettyStmt (SRet e) = prettyKeyword "ret" <+> prettyVStmt e
prettyStmt (SFun fs s) = vcat $ map f (M.toList fs) ++ [prettyStmt s]
    where f (n, (p, s')) = nest 4 $ prettyKeyword "fun" <+> prettyTH n <+> prettyTH p <> prettyKeyword ":" <> line <> prettyStmt s'
prettyStmt (SIf e st sf) = vcat [prettyKeyword "if" <+> prettyTH e <> prettyKeyword ":", indent 4 (prettyStmt st), prettyKeyword "else", indent 4 (prettyStmt sf)]
prettyStmt (SBlock ss) = vcat (map prettyStmt ss)
prettyStmt (SCase e cs) = vcat [prettyKeyword "case" <+> prettyTH e, vcat (map f cs)]
    where f (p, s) = vcat [prettyKeyword "|" <+> prettyTH p <> prettyKeyword ":", indent 4 $ prettyStmt s]

prettyVStmt :: VStmt -> Doc ann
prettyVStmt (VExp e) = prettyTH e
prettyVStmt (VCall n e) = prettyKeyword "call" <+> prettyTH n <+> prettyTH e

prettyNProg :: NProg -> Doc ann
prettyNProg np = prettyKeyword "inputs" <+> prettyTH (nProgInputs np) <> line
    <> prettyStmt (SFun (nProgFuns np) (SRet (VCall (nProgInit np) (nProgInitParam np)))) <> line

