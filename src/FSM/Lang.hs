module FSM.Lang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Prelude

type FunMap = M.Map TH.Name (TH.Pat, Stmt)

data VStmt = VExp TH.Exp
           | VCall TH.Name TH.Exp
    deriving (Show, Eq)

data VarKind = VarLet | VarMut deriving (Show, Eq)

data Stmt = SLet VarKind TH.Name VStmt Stmt
          | SAssign TH.Name TH.Exp
          | SYield TH.Exp
          | SRet VStmt
          | SFun FunMap Stmt
          | SBlock [Stmt]
          | SIf TH.Exp Stmt Stmt
          | SCase TH.Exp [(TH.Pat, Stmt)]
          | SNop
    deriving (Show, Eq)

data Prog = Prog {
    progName :: TH.Name,
    progType :: TH.Type,
    progParams :: [TH.Pat],
    progInputs :: Maybe TH.Pat,
    progBody :: Stmt
} deriving (Show, Eq)

data NProg = NProg {
    nProgName :: TH.Name,
    nProgType :: TH.Type,
    nProgParams :: [TH.Pat],
    nProgInputs :: Maybe TH.Pat,
    nProgFuns :: FunMap,
    nProgInit :: TH.Name,
    nProgInitParam :: TH.Exp,
    nProgConts :: M.Map TH.Name (M.Map TH.Name [TH.Name])
} deriving (Show, Eq)

sBlockS :: [Stmt] -> Stmt
sBlockS [] = SNop
sBlockS [s] = s
sBlockS ss = SBlock ss

tupE :: [TH.Exp] -> TH.Exp
tupE [x] = x
tupE xs = TH.TupE . map Just $ xs

tupP :: [TH.Pat] -> TH.Pat
tupP [x] = x
tupP xs = TH.TupP xs

emittingStmt :: Stmt -> Bool
emittingStmt (SBlock [SYield _, _]) = True
emittingStmt SNop = False
emittingStmt (SRet _) = False
emittingStmt (SIf _ st sf) = emittingStmt st || emittingStmt sf
emittingStmt (SCase _ cs) = any (emittingStmt . snd) cs
emittingStmt (SLet _ _ _ s) = emittingStmt s



