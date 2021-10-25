module FSM.Process.IntegrateCase(integrateCase) where

import FSM.Lang
import FSM.FreeVars
import Prelude
import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH as TH
import qualified FSM.Util.MultiSet as MS
import FSM.Util.MMaybe

integrateCase :: NProg -> NProg
integrateCase prog = prog { nProgBody = nProgBody prog }

