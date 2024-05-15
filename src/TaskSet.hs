{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module TaskSet where

import Task
import Data.Time
import Type.Reflection
import Data.Maybe (fromMaybe)
import Type.Match
import Config (FillConfig, PickConfig, DecideConfig, StepConfig, ResolutionConfig)
import Tasks.LegalCNF.Config (LegalCNFConfig)
import Tasks.LegalProposition.Config (LegalPropositionConfig)
import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig)
import Tasks.SubTree.Config (SubTreeConfig)
import Tasks.TreeToFormula.Config (TreeToFormulaConfig)
import Tasks.ComposeFormula.Config (ComposeFormulaConfig)
import Tasks.DecomposeFormula.Config (DecomposeFormulaConfig)
import Control.Monad (forM, forM_)
import qualified Data.ByteString.Lazy as BS
import Codec.Archive.Zip
import System.Directory (removeFile)
import System.FilePath ((</>), (<.>))

newtype TaskSet = TaskSet [Task]

instance Semigroup TaskSet where
  TaskSet xs <> TaskSet ys = TaskSet $ xs <> ys

type Name = String
type Weight = Double

taskSet :: [Task] -> TaskSet
taskSet = TaskSet

task :: (Typeable config, Show config) => Name -> config -> TaskStatus -> Restrictions -> Weight -> Bewertung -> Task
task n c = Task n undefined undefined cfg (taskTypeOf cfg)
  where
    cfg = TaskConfig c

demoTask :: (Typeable config, Show config) => Name -> config -> Task
demoTask n c = task n c Demo NoRestrictions 0 Best

mandatoryTask :: (Typeable config, Show config) => Name -> config -> Weight -> Task
mandatoryTask n c w = task n c Pflicht badFullGoodSilent w RecentSyntacticallyCorrect

from :: String -> TaskSet -> IO TaskSet
from str (TaskSet ts) = do
  time <- readTime str
  pure $ TaskSet $ map (\t -> t{taskFrom=time}) ts

to :: String -> TaskSet -> IO TaskSet
to str (TaskSet ts) = do
  time <- readTime str
  pure $ TaskSet $ map (\t -> t{taskTo=time}) ts

readTime :: String -> IO UTCTime
readTime x = do
  tz <- getCurrentTimeZone
  pure $ fromMaybe (error $ "invalid date: "++ x) $
    parseTimeM True defaultTimeLocale{knownTimeZones=[tz]} "%-d.%-m.%Y %-H:%M%Z" $ x ++ timeZoneName tz

taskTypeOf :: TaskConfig -> AufgabenTyp
taskTypeOf (TaskConfig cfg) = matchTypeOf cfg
  [ inCaseOf' @FillConfig $ Logik FillQuiz
  , inCaseOf' @PickConfig $ Logik PickQuiz
  , inCaseOf' @DecideConfig $ Logik DecideQuiz
  , inCaseOf' @StepConfig $ Logik StepQuiz
  , inCaseOf' @ResolutionConfig $ Logik ResolutionQuiz
  , inCaseOf' @LegalCNFConfig $ Logik InvalidCnfsQuiz
  , inCaseOf' @LegalPropositionConfig $ Logik InvalidFormulasQuiz
  , inCaseOf' @SuperfluousBracketsConfig $ Logik RemoveBracketsQuiz
  , inCaseOf' @SubTreeConfig $ Logik SubformulasQuiz
  , inCaseOf' @TreeToFormulaConfig $ Logik TreeToFormulaQuiz
  , inCaseOf' @ComposeFormulaConfig $ Logik ComposeFormulaQuiz
  , inCaseOf' @DecomposeFormulaConfig $ Logik DecomposeFormulaQuiz
  , fallbackCase $ \x -> error $ "unknown config type: " ++ show (typeOf x)
  ]

countPoints :: TaskSet -> Double
countPoints (TaskSet ts) = sum $ map weight ts

createTaskFiles :: TaskSet -> FilePath -> IO ()
createTaskFiles (TaskSet xs) path = do
  newFiles <- forM (xs`zip`[1..]) $ \(t,i) -> do
    let fname = path </> padString 3 '0' (show i) <.> "text"
    writeFile fname $ printTask t
    pure fname
  archive <- addFilesToArchive [] emptyArchive newFiles
  BS.writeFile (path </> "tasks.zip") $ fromArchive archive
  forM_ newFiles removeFile

padString :: Int -> Char -> String -> String
padString n x str = replicate (n - length str) x ++ str
