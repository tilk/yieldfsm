{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Defines the YieldFSM language.
-}
module FSM.Lang where

import qualified Language.Haskell.TH as TH
import qualified Data.Map.Strict as M
import Data.Kind(Type)
import Prelude
import GHC.TypeLits

{-|
Represents the intermediate language used.
Type-level naturals are used in order to leverage type-level inequalities
for testing for language features.
-}
type Lvl = Nat
type LvlSugared = 4 -- ^ Source language.
type LvlLoops   = 3 -- ^ Language with loops, but without outputs.
type LvlFull    = 2 -- ^ Desugared language.
type LvlLifted  = 1 -- ^ Lambda-lifted language.
type LvlLowest  = 0 -- ^ Normalized language.

-- | Type-level negated inequality.
type (a /<= b) = (a <=? b) ~ 'False

type WithOutputs l = LvlSugared <= l  -- ^ Has named outputs.
type NoOutputs   l = LvlSugared /<= l -- ^ Does not have named outputs.

type WithLoops l = LvlLoops <= l      -- ^ Has loops.
type NoLoops   l = LvlLoops /<= l     -- ^ Does not have loops.

type WithFun l = LvlFull <= l         -- ^ Has local function definitions.
type NoFun   l = LvlFull /<= l        -- ^ Does not have local function definitions.

type WithAssign l = LvlLifted <= l    -- ^ Has mutable variable assignment.
type NoAssign   l = LvlLifted /<= l   -- ^ Does not have mutable variable assignment.

type WithBlock l = LvlLifted <= l     -- ^ Has statement composition.
type NoBlock   l = LvlLifted /<= l    -- ^ Does not have statement composition.

type IsOneOutput l = NoOutputs l                -- ^ 'LvlLoops' or lower.
type IsDesugared l = (NoLoops l, IsOneOutput l) -- ^ 'LvlFull' or lower.
type IsLifted l = (NoFun l, IsDesugared l)      -- ^ 'LvlLifted' or lower.
type IsLowered l = (NoBlock l, IsLifted l)      -- ^ 'LvlLowest'.

-- | Set of functions. A function has a pattern for its arguments and a body.
type FunMap l = M.Map TH.Name (TH.Pat, Stmt l)

-- | Evaluables - RHS of @let@ and @ret@ statements.
data VStmt = VExp TH.Exp          -- ^ Expression evaluable.
           | VCall TH.Name TH.Exp -- ^ Function call evaluable.
    deriving (Show, Eq)

-- | Variable kind.
data VarKind = VarLet -- ^ Immutable variable (@let@-defined).
             | VarMut -- ^ Mutable variable (@var@).
    deriving (Show, Eq)

-- | Break statement type.
data BrkType = BrkCont -- ^ Continue statement.
             | BrkBrk  -- ^ Break statement.
    deriving (Show, Eq)

-- | While loop type.
data WhileType = WhileWhile -- ^ While loop.
               | WhileUntil -- ^ Until loop (negated condition).
    deriving (Show, Eq)

-- | Iteration type in a loop statement.
data IterType = IterWhile   -- ^ While iteration - condition checked before entering the loop.
              | IterDoWhile -- ^ Do-while iteration - the body executes at least once.
    deriving (Show, Eq)

-- | Type of loop statement.
data LoopType = LoopForever                         -- ^ @forever@ loops.
              | LoopWhile IterType WhileType TH.Exp -- ^ While-type loops: @while@, @do@-@while@, @until@ and @do@-@until@.
              | LoopRepeat IterType TH.Exp          -- ^ Repeat loops: @repeat@ and @repeat1@.
    deriving (Show, Eq)

{-|
YieldFSM statement type. The available statements are dependent on
the value of the parameter 'Lvl'. Thanks to this, the 'Stmt' type
encodes the various intermediate languages in the YieldFSM compilation
process.
-}
data Stmt :: Lvl -> Type where
    SLoop   :: WithLoops l => LoopType -> Stmt l -> Stmt l     -- ^ Loop statement: @forever@, @while@, etc.
    SBreak  :: WithLoops l => BrkType -> Stmt l                -- ^ Break statement.
    SFun    :: WithFun l => FunMap l -> Stmt l -> Stmt l       -- ^ Function definition.
    SLet    :: VarKind -> TH.Name -> VStmt -> Stmt l -> Stmt l -- ^ Let definition: @let@ and @var@.
    SAssign :: WithAssign l => TH.Name -> TH.Exp -> Stmt l     -- ^ Mutable variable assignment statement.
    SOutput :: WithOutputs l => [TH.Name] -> TH.Exp -> Stmt l  -- ^ Output statement.
    SYieldO :: WithOutputs l => [TH.Name] -> TH.Exp -> Stmt l  -- ^ Yield statement with named outputs.
    SYield  :: (WithBlock l, NoOutputs l) => TH.Exp -> Stmt l  -- ^ Yield statement.
    SYieldT :: NoBlock l => TH.Exp -> Stmt l -> Stmt l         -- ^ Yield statement, immediately followed by another statement.
    SRet    :: VStmt -> Stmt l                                 -- ^ Return statement.
    SBlock  :: WithBlock l => [Stmt l] -> Stmt l               -- ^ Block statement (statement composition).
    SIf     :: TH.Exp -> Stmt l -> Stmt l -> Stmt l            -- ^ If statement.
    SCase   :: TH.Exp -> [(TH.Pat, Stmt l)] -> Stmt l          -- ^ Case statement.
    SNop    :: WithBlock l => Stmt l                           -- ^ Skip statement.

deriving instance Show (Stmt l)
deriving instance Eq (Stmt l)

data Output = Output {
    outputDefault :: TH.Exp
} deriving (Show, Eq)

{-|
YieldFSM programs (automata descriptions).
Program definitions are written in YieldFSM syntax as follows:

> progName :: progType
> param progParam         -- zero or more
> input progInputs        -- zero or one
> output progOutput = exp -- zero or more
> progBody
-}
data Prog l = Prog {
    progName    :: TH.Name,             -- ^ Program name.
    progType    :: TH.Type,             -- ^ Program type.
    progParams  :: [TH.Pat],            -- ^ Program parameters.
    progInputs  :: Maybe TH.Pat,        -- ^ Program inputs.
    progOutputs :: [(TH.Name, Output)], -- ^ Program outputs.
    progBody    :: Stmt l               -- ^ Program body.
} deriving (Show, Eq)

{-|
YieldFSM programs after lambda-lifting.
-}
data NProg l = NProg {
    nProgName      :: TH.Name,      -- ^ Program name.
    nProgType      :: TH.Type,      -- ^ Program type.
    nProgParams    :: [TH.Pat],     -- ^ Program parameters.
    nProgInputs    :: Maybe TH.Pat, -- ^ Program inputs.
    nProgFuns      :: FunMap l,     -- ^ Function definitions.
    nProgInit      :: TH.Name,      -- ^ Initial function name.
    nProgInitParam :: TH.Exp,       -- ^ Initial function parameter.
    nProgConts :: M.Map TH.Name (M.Map TH.Name [TH.Name]) -- ^ Continuation data types.
} deriving (Show, Eq)

{-|
Constructs a block from a list of statements.
Size 0 is represented using 'SNop', and for size 1, the block statement is omitted.
-}
sBlockS :: WithBlock l => [Stmt l] -> Stmt l
sBlockS [] = SNop
sBlockS [s] = s
sBlockS ss = SBlock ss

{-|
Constructs a TH tuple expression from a list of TH expressions.
Size 1 is avoided because of Template Haskell weirdness.
-}
tupE :: [TH.Exp] -> TH.Exp
tupE [x] = x
tupE xs = TH.TupE . map Just $ xs

{-|
Constructs a TH tuple pattern from a list of TH patterns.
Size 1 is avoided because of Template Haskell weirdness.
-}
tupP :: [TH.Pat] -> TH.Pat
tupP [x] = x
tupP xs = TH.TupP xs

{-|
Checks if a statement contains a @yield@.
-}
emittingStmt :: IsLowered l => Stmt l -> Bool
emittingStmt (SYieldT _ _) = True
emittingStmt (SRet _) = False
emittingStmt (SIf _ st sf) = emittingStmt st || emittingStmt sf
emittingStmt (SCase _ cs) = any (emittingStmt . snd) cs
emittingStmt (SLet _ _ _ s) = emittingStmt s



