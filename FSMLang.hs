module FSMLang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

data Stmt = SVar TH.Name TH.Exp
          | SAssign TH.Name TH.Exp
          | SLet TH.Name TH.Exp
          | SEmit TH.Exp
          | SRet TH.Exp
          | SCall TH.Name TH.Name TH.Exp
          | STailcall TH.Name TH.Exp
          | SFun TH.Name TH.Pat Stmt
          | SSeq Stmt Stmt
          | SIf TH.Exp Stmt Stmt
          | SNop
    deriving Show

data Prog = Prog {
    progInputs :: TH.Pat,
    progBody :: Stmt
}

