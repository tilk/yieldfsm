{-# LANGUAGE TemplateHaskell, TupleSections, GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleContexts #-}
module FSM.LangProcess(
    lambdaLift, refreshFunctions, cutBlocks, removeEpsilon, makeLocalVars,
    makeTailCalls, deTailCall, previousInputs, flattenTuples,
    foldInit, refreshVars, simplifyCase, simplifyCaseN,
    cleanUnusedConstructors, cleanUnusedArgs, cleanUnusedConts
) where

import FSM.Process.MakeTailCalls
import FSM.Process.RefreshFunctions
import FSM.Process.RefreshVars
import FSM.Process.CutBlocks
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

