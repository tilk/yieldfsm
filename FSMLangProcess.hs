{-# LANGUAGE TemplateHaskell #-}
module FSMLangProcess where

import FSMLang
import THFreeVars
import Prelude
import Control.Monad
import Control.Monad.State
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH

{-
data LifterState = LifterState {
    lifterFunEnv :: M.Map TH.Name TH.Name,
    lifterFuns :: M.Map TH.Name (TH.Pat, Stmt)
}

$(makeLenses ''LifterState)

lambdaLift (SSeq s1 s2) = SSeq <$> lambdaLift s1 <*> lambdaLift s2
lambdaLift s = return s
-}


