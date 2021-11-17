module FSM.Lang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Data.Kind(Type)
import Prelude

data Lvl = LvlFull
         | LvlLowest

type family HasFun (l :: Lvl) :: Bool
type WithFun l = HasFun l ~ 'True
type NoFun l = HasFun l ~ 'False

type instance HasFun 'LvlFull = 'True
type instance HasFun 'LvlLowest = 'False

type FunMap l = M.Map TH.Name (TH.Pat, Stmt l)

data VStmt = VExp TH.Exp
           | VCall TH.Name TH.Exp
    deriving (Show, Eq)

data VarKind = VarLet | VarMut deriving (Show, Eq)

data Stmt :: Lvl -> Type where
    SLet    :: VarKind -> TH.Name -> VStmt -> Stmt l -> Stmt l
    SAssign :: TH.Name -> TH.Exp -> Stmt l
    SYield  :: TH.Exp -> Stmt l
    SRet    :: VStmt -> Stmt l
    SFun    :: WithFun l => FunMap l -> Stmt l -> Stmt l
    SBlock  :: [Stmt l] -> Stmt l
    SIf     :: TH.Exp -> Stmt l -> Stmt l -> Stmt l
    SCase   :: TH.Exp -> [(TH.Pat, Stmt l)] -> Stmt l
    SNop    :: Stmt l

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

sBlockS :: [Stmt l] -> Stmt l
sBlockS [] = SNop
sBlockS [s] = s
sBlockS ss = SBlock ss

tupE :: [TH.Exp] -> TH.Exp
tupE [x] = x
tupE xs = TH.TupE . map Just $ xs

tupP :: [TH.Pat] -> TH.Pat
tupP [x] = x
tupP xs = TH.TupP xs

emittingStmt :: Stmt l -> Bool
emittingStmt (SBlock [SYield _, _]) = True
emittingStmt SNop = False
emittingStmt (SRet _) = False
emittingStmt (SIf _ st sf) = emittingStmt st || emittingStmt sf
emittingStmt (SCase _ cs) = any (emittingStmt . snd) cs
emittingStmt (SLet _ _ _ s) = emittingStmt s



