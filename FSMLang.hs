module FSMLang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

data VStmt = VExp TH.Exp
           | VCall TH.Name TH.Exp
    deriving Show

data Stmt = SVar TH.Name VStmt
          | SLet TH.Name VStmt
          | SAssign TH.Name TH.Exp
          | SEmit TH.Exp
          | SRet VStmt
          | SFun TH.Name TH.Pat Stmt
          | SSeq Stmt Stmt
          | SIf TH.Exp Stmt Stmt
          | SNop
    deriving Show

data Prog = Prog {
    progInputs :: TH.Pat,
    progBody :: Stmt
}

