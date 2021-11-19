module FSM.Lang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Data.Kind(Type)
import Prelude
import GHC.TypeLits

type Lvl = Nat
type LvlSugared = 3
type LvlFull = 2
type LvlLifted = 1
type LvlLowest = 0

type WithLoops l = LvlSugared <= l
type NoLoops l = (LvlSugared <=? l) ~ 'False

type WithFun l = LvlFull <= l
type NoFun l = (LvlFull <=? l) ~ 'False

type WithAssign l = LvlLifted <= l
type NoAssign l = (LvlLifted <=? l) ~ 'False

type WithBlock l = LvlLifted <= l
type NoBlock l = (LvlLifted <=? l) ~ 'False

type IsDesugared l = NoLoops l
type IsLifted l = (NoFun l, IsDesugared l)
type IsLowered l = (NoBlock l, IsLifted l)

type FunMap l = M.Map TH.Name (TH.Pat, Stmt l)

data VStmt = VExp TH.Exp
           | VCall TH.Name TH.Exp
    deriving (Show, Eq)

data VarKind = VarLet | VarMut deriving (Show, Eq)

data BrkType = BrkCont | BrkBrk deriving (Show, Eq)

data WhileType = WhileWhile | WhileUntil deriving (Show, Eq)

data IterType = IterWhile | IterDoWhile deriving (Show, Eq)

data LoopType = LoopForever
              | LoopWhile IterType WhileType TH.Exp
              | LoopRepeat IterType TH.Exp
    deriving (Show, Eq)

data Stmt :: Lvl -> Type where
    SLoop   :: WithLoops l => LoopType -> Stmt l -> Stmt l
    SBreak  :: WithLoops l => BrkType -> Stmt l
    SFun    :: WithFun l => FunMap l -> Stmt l -> Stmt l
    SLet    :: VarKind -> TH.Name -> VStmt -> Stmt l -> Stmt l
    SAssign :: WithAssign l => TH.Name -> TH.Exp -> Stmt l
    SYield  :: WithBlock l => TH.Exp -> Stmt l
    SYieldT :: NoBlock l => TH.Exp -> Stmt l -> Stmt l
    SRet    :: VStmt -> Stmt l
    SBlock  :: WithBlock l => [Stmt l] -> Stmt l
    SIf     :: TH.Exp -> Stmt l -> Stmt l -> Stmt l
    SCase   :: TH.Exp -> [(TH.Pat, Stmt l)] -> Stmt l
    SNop    :: WithBlock l => Stmt l

deriving instance Show (Stmt l)
deriving instance Eq (Stmt l)

data Prog l = Prog {
    progName :: TH.Name,
    progType :: TH.Type,
    progParams :: [TH.Pat],
    progInputs :: Maybe TH.Pat,
    progBody :: Stmt l
} deriving (Show, Eq)

data NProg l = NProg {
    nProgName :: TH.Name,
    nProgType :: TH.Type,
    nProgParams :: [TH.Pat],
    nProgInputs :: Maybe TH.Pat,
    nProgFuns :: FunMap l,
    nProgInit :: TH.Name,
    nProgInitParam :: TH.Exp,
    nProgConts :: M.Map TH.Name (M.Map TH.Name [TH.Name])
} deriving (Show, Eq)

sBlockS :: WithBlock l => [Stmt l] -> Stmt l
sBlockS [] = SNop
sBlockS [s] = s
sBlockS ss = SBlock ss

tupE :: [TH.Exp] -> TH.Exp
tupE [x] = x
tupE xs = TH.TupE . map Just $ xs

tupP :: [TH.Pat] -> TH.Pat
tupP [x] = x
tupP xs = TH.TupP xs

emittingStmt :: IsLowered l => Stmt l -> Bool
emittingStmt (SYieldT _ _) = True
emittingStmt (SRet _) = False
emittingStmt (SIf _ st sf) = emittingStmt st || emittingStmt sf
emittingStmt (SCase _ cs) = any (emittingStmt . snd) cs
emittingStmt (SLet _ _ _ s) = emittingStmt s



