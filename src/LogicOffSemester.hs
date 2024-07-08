{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LogicOffSemester where

import TaskSet

import Control.Monad ((>=>))

import LogicTasks.Config (FillConfig(..),PickConfig(..),DecideConfig(..),StepConfig(..),ResolutionConfig(..))
import  Tasks.TreeToFormula.Config

import Syntax.InvalidFormulas.Config
import Syntax.RemoveBrackets.Config
import Syntax.TreeToFormula.Config
import Semantics.TruthTables.FillGaps.Config
import Syntax.InvalidCnfs.Config
import Semantics.TruthTables.ChooseForFormula.Config
import Semantics.TruthTables.FindMistakes.Config
import Semantics.Resolution.Step.Config
import Semantics.Resolution.Complete.Config
import Syntax.Subformulas.Config as Subformulas
import Syntax.ComposeFormula.Config as Compose
import Syntax.DecomposeFormula.Config as Decompose

exerciseTasks :: IO TaskSet
exerciseTasks = from "20.05.2024 12:00" >=> to "07.08.2024 23:55" $ taskSet
  [ demoTask "Task01 A" task17
  , demoTask "Task02 A" task02
  , demoTask "Task03 A" task03
  , demoTask "Task04 A" task04
  , demoTask "Task05 A" task05
  , demoTask "Task06 A" task20
  , demoTask "Task07 A" task18
  , demoTask "Task08 A" task19
  , demoTask "Task09 A" task09
  , demoTask "Task10 A" task10
  , demoTask "Task11 A" task11
  , demoTask "Task12 A" task12
  , demoTask "Task13 A" task13
  , demoTask "Task14 A" task14
  , demoTask "Task15 A" task15
  , demoTask "Task16 A" task16
  , demoTask "Task17 A" Subformulas.medium
  , demoTask "Task18 A" Compose.medium
  , demoTask "Task19 A" Decompose.medium
  ] <> taskSet
  [ mandatoryTask "Task01 B" task17 0.34
  , mandatoryTask "Task02 B" task02 0.34
  , mandatoryTask "Task03 B" task03 0.34
  , mandatoryTask "Task04 B" task04 0.34
  , mandatoryTask "Task05 B" task05 0.34
  , mandatoryTask "Task06 B" task20 0.34
  , mandatoryTask "Task07 B" task18 0.34
  , mandatoryTask "Task08 B" task19 0.34
  , mandatoryTask "Task09 B" task09 0.34
  , mandatoryTask "Task10 B" task10 0.34
  , mandatoryTask "Task11 B" task11 0.34
  , mandatoryTask "Task12 B" task12 0.25
  , mandatoryTask "Task13 B" task13 0.25
  , mandatoryTask "Task14 B" task14 0.25
  , mandatoryTask "Task15 B" task15{printFeedbackImmediately=False} 0.5
  , mandatoryTask "Task16 B" task16{printFeedbackImmediately=False} 0.5
  , mandatoryTask "Task17 B" Subformulas.medium 0.34
  , mandatoryTask "Task18 B" Compose.medium 0.34
  , mandatoryTask "Task19 B" Decompose.medium 0.34
  ]

deriving instance Show TreeToFormulaConfig
deriving instance Show FillConfig
deriving instance Show PickConfig
deriving instance Show StepConfig
deriving instance Show ResolutionConfig
deriving instance Show DecideConfig
