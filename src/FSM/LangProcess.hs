{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Program transformations.
-}
module FSM.LangProcess(
    lambdaLift, refreshFunctions, normalization, removeEpsilon, makeLocalVars,
    stackReify, deTailCall, previousInputs, flattenTuples,
    foldInit, refreshVars, simplifyCase, simplifyCaseN, simplifyCaseNFull,
    cleanUnusedConstructors, cleanUnusedArgs, cleanUnusedConts,
    argumentPropagation, integrateCase, testFreshness, desugarLoops,
    desugarOutputs, hoistFromConstructors, deduplicateArgs
) where

import FSM.Process.StackReify
import FSM.Process.RefreshFunctions
import FSM.Process.RefreshVars
import FSM.Process.Normalization
import FSM.Process.MakeLocalVars
import FSM.Process.DeTailCall
import FSM.Process.RemoveEpsilon
import FSM.Process.LambdaLift
import FSM.Process.FoldInit
import FSM.Process.SimplifyCase
import FSM.Process.PreviousInputs
import FSM.Process.FlattenTuples
import FSM.Process.CleanUnusedConstructors
import FSM.Process.CleanUnusedArgs
import FSM.Process.CleanUnusedConts
import FSM.Process.ArgumentPropagation
import FSM.Process.IntegrateCase
import FSM.Process.TestFreshness
import FSM.Process.DesugarLoops
import FSM.Process.DesugarOutputs
import FSM.Process.HoistFromConstructors
import FSM.Process.DeduplicateArgs

