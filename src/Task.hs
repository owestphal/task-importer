{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Task where
import Data.Time
import Data.Map (Map)
import Data.List (intercalate)
import Type.Reflection
import qualified Data.Map as Map


printTask :: Task -> String
printTask Task{..} =
  "Aufgabe {" ++ intercalate ","
  ["aufgabeName = "++ show name
  , "aufgabeVorlesungId = VorlesungKey {unVorlesungKey = SqlBackendKey {unSqlBackendKey = 0}}"
  , "aufgabeHighscore = keine"
  , "aufgabeVon = "++ formatUTCTime taskFrom
  , "aufgabeBis = "++ formatUTCTime taskTo
  , "aufgabeMaxPoints = Just 1.0"
  , "aufgabeKonfiguration = " ++ taskCOnfigStr config
  , "aufgabeHinweis = Nothing"
  , "aufgabeTyp = " ++ show (aufgabenTypString taskType)
  , "aufgabeStatus = " ++ printStatus status
  , "aufgabeServer = \"https://autotool.fmi.uni-due.de:8888\""
  , "aufgabeSignatur = \"missing\""
  , "aufgabeRestriction = " ++ printRestricitons restriction
  , "aufgabeGewicht = Just " ++ show weight
  , "aufgabeBewertung = " ++ show bewertung
  ] ++ "}"

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale{knownTimeZones=[]} "%F %R:00 UTC"

data Task = Task
  { name :: String
  , taskFrom :: UTCTime
  , taskTo :: UTCTime
  , config :: TaskConfig
  , taskType :: AufgabenTyp
  , status :: TaskStatus
  , restriction :: Restrictions
  , weight :: Double
  , bewertung :: Bewertung
  }

data TaskStatus = Demo | Optional | Pflicht

printStatus :: TaskStatus -> String
printStatus Demo = "demo"
printStatus Optional = "optional"
printStatus Pflicht = "mandatory"


data Bewertung = Best | Recent | RecentSyntacticallyCorrect
  deriving Show

data TaskConfig where
  TaskConfig :: (Typeable a, Show a) => a -> TaskConfig

taskCOnfigStr :: TaskConfig -> String
taskCOnfigStr (TaskConfig cfg) = show $ show cfg

data AufgabenTyp = ProPa ProPaAufgabenTyp | Logik LogikAufgabenTyp
data ProPaAufgabenTyp = Haskell | Prolog
data LogikAufgabenTyp
  = FillDirect
  | FillQuiz
  | PickDirect
  | PickQuiz
  | DecideDirect
  | DecideQuiz
  | MaxIst
  | MaxQuiz
  | MinIst
  | MinQuiz
  | StepDirect
  | StepQuiz
  | ResolutionDirect
  | ResolutionQuiz
  | PrologDirect
  | PrologQuiz
  | InvalidCnfsQuiz
  | InvalidFormulasQuiz
  | RemoveBracketsQuiz
  | SubformulasQuiz
  | TreeToFormulaQuiz
  | ComposeFormulaQuiz
  | DecomposeFormulaQuiz

aufgabenTypString :: AufgabenTyp -> String
aufgabenTypString (ProPa Haskell) = "Haskell_Template-Direct"
aufgabenTypString (ProPa Prolog) = "PrologProgramming-Direct"
aufgabenTypString (Logik FillDirect) = "TruthTableFillGaps-Direct"
aufgabenTypString (Logik FillQuiz) = "TruthTableFillGaps-Quiz"
aufgabenTypString (Logik PickDirect) = "TruthTableChooseForFormula-Direct"
aufgabenTypString (Logik PickQuiz) = "TruthTableChooseForFormula-Quiz"
aufgabenTypString (Logik DecideDirect) = "TruthTableFindMistakes-Direct"
aufgabenTypString (Logik DecideQuiz) = "TruthTableFindMistakes-Quiz"
aufgabenTypString (Logik MaxIst) = "TruthTableMaxterm-Direct"
aufgabenTypString (Logik MaxQuiz) = "TruthTableMaxterm-Quiz"
aufgabenTypString (Logik MinIst) = "TruthTableMinTerm-Direct"
aufgabenTypString (Logik MinQuiz) = "TruthTableMinTerm-Quiz"
aufgabenTypString (Logik StepDirect) = "LogicResolutionStep-Direct"
aufgabenTypString (Logik StepQuiz) = "LogicResolutionStep-Quiz"
aufgabenTypString (Logik ResolutionDirect) = "LogicResolutionComplete-Direct"
aufgabenTypString (Logik ResolutionQuiz) = "LogicResolutionComplete-Quiz"
aufgabenTypString (Logik PrologDirect) = "PrologResolutionStep-Direct"
aufgabenTypString (Logik PrologQuiz) = "PrologResolutionStep-Quiz"
aufgabenTypString (Logik InvalidCnfsQuiz) = "LogicInvalidCnfs-Quiz"
aufgabenTypString (Logik InvalidFormulasQuiz) = "LogicInvalidFormulas-Quiz"
aufgabenTypString (Logik RemoveBracketsQuiz) = "LogicRemoveBrackets-Quiz"
aufgabenTypString (Logik SubformulasQuiz) = "LogicSubformulas-Quiz"
aufgabenTypString (Logik TreeToFormulaQuiz) = "LogicTreeToFormula-Quiz"
aufgabenTypString (Logik ComposeFormulaQuiz) = "LogicComposeFormula-Quiz"
aufgabenTypString (Logik DecomposeFormulaQuiz) = "LogicDecomposeFormula-Quiz"

data Restrictions = NoRestrictions |  Restrictions (Map SyntaxType Verbosity)

printRestricitons :: Restrictions -> String
printRestricitons = \case
  NoRestrictions -> printWithMap Map.empty
  (Restrictions x) -> printWithMap x
  where
    printWithMap :: Map SyntaxType Verbosity -> String
    printWithMap m = "Restrictions { max_submissions = fromList [], answers = " ++ show m ++ ", discount = fromList []}"

data SyntaxType = GoodSyntax | BadSyntax
  deriving (Eq,Ord, Show)

data Verbosity = Verbosity {for_status :: VerbosityLevel, for_explanation :: VerbosityLevel }
  deriving Show

data VerbosityLevel = Full | Silent
  deriving Show

badFullGoodSilent :: Restrictions
badFullGoodSilent = Restrictions (Map.fromList [(BadSyntax,Verbosity Full Full),(GoodSyntax, Verbosity Silent Silent)])
