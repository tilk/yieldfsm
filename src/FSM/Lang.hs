module FSM.Lang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

type FunMap = M.Map TH.Name (TH.Pat, Stmt)

data VStmt = VExp TH.Exp
           | VCall TH.Name TH.Exp
    deriving Show

data VarKind = VarLet | VarMut deriving (Show, Eq)

data Stmt = SLet VarKind TH.Name VStmt Stmt
          | SAssign TH.Name VStmt
          | SEmit TH.Exp
          | SRet VStmt
          | SFun FunMap Stmt
          | SBlock [Stmt]
          | SIf TH.Exp Stmt Stmt
          | SCase TH.Exp [(TH.Pat, Stmt)]
          | SNop
    deriving Show

data Prog = Prog {
    progName :: TH.Name,
    progType :: TH.Type,
    progParams :: [TH.Pat],
    progInputs :: TH.Pat,
    progBody :: Stmt
} deriving Show

data NProg = NProg {
    nProgName :: TH.Name,
    nProgType :: TH.Type,
    nProgParams :: [TH.Pat],
    nProgInputs :: TH.Pat,
    nProgFuns :: FunMap,
    nProgInit :: TH.Name,
    nProgInitParam :: TH.Exp,
    nProgConts :: M.Map TH.Name (M.Map TH.Name [TH.Name])
} deriving Show

sBlock :: [Stmt] -> Stmt
sBlock [] = SNop
sBlock [s] = s
sBlock ss = SBlock ss

