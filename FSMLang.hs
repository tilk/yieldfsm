module FSMLang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

type FunMap = M.Map TH.Name (TH.Pat, Stmt)

data VStmt = VExp TH.Exp
           | VCall TH.Name TH.Exp
    deriving Show

data Stmt = SVar TH.Name VStmt
          | SLet TH.Name VStmt
          | SAssign TH.Name TH.Exp
          | SEmit TH.Exp
          | SRet VStmt
          | SFun FunMap
          | SBlock [Stmt]
          | SIf TH.Exp Stmt Stmt
          | SNop
    deriving Show

data Prog = Prog {
    progInputs :: TH.Pat,
    progBody :: [Stmt]
}

data NProg = NProg {
    nProgInputs :: TH.Pat,
    nProgFuns :: FunMap,
    nProgInit :: TH.Name,
    nProgInitParam :: TH.Exp
}

toNProg :: Prog -> Maybe NProg
toNProg (Prog is [SFun fs, SRet (VCall f1 e1)]) = Just (NProg is fs f1 e1)
toNProg _ = Nothing

