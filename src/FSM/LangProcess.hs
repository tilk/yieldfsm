{-# LANGUAGE TemplateHaskell, TupleSections, GeneralizedNewtypeDeriving, DerivingStrategies, FlexibleContexts #-}
module FSM.LangProcess(
    lambdaLift, refreshFunctions, cutBlocks, removeEpsilon, makeLocalVars,
    makeTailCalls, deTailCall, propagateConstants, propagateConstantsN,
    foldInit, refreshVars
) where

import FSM.Process.PropagateConstants
import FSM.Process.MakeTailCalls
import FSM.Process.RefreshFunctions
import FSM.Process.RefreshVars
import FSM.Process.CutBlocks
import FSM.Process.MakeLocalVars
import FSM.Process.DeTailCall
import FSM.Process.RemoveEpsilon
import FSM.Process.LambdaLift
import FSM.Process.FoldInit

