{-# LANGUAGE TupleSections #-}
module Main (main) where

import Data.Time
import Data.Maybe
import Data.List (partition, isPrefixOf)
import Data.Char
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Codec.Archive.Zip

import System.Environment
import System.FilePath
import System.Directory

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path, fromDate, untilDate] -> do
      tF <- readTime fromDate
      tU <- readTime untilDate
      runMain path tF tU
    _ -> putStrLn "unknown arguments"

runMain :: FilePath -> UTCTime -> UTCTime -> IO ()
runMain taskPath fromDate untilDate = do
  files <- listDirectory taskPath
  newFiles <- fmap catMaybes $ forM files $ \file -> do
    mTyp <- determineType taskPath file
    case mTyp of
      Nothing -> pure Nothing
      Just (typ, config) -> do
        let newFile = dropExtension file ++ ".text"
        writeFile newFile $ template typ (takeBaseName file) config fromDate untilDate
        pure $ Just newFile
  archive <- addFilesToArchive [] emptyArchive newFiles
  BS.writeFile "tasks.zip" $ fromArchive archive
  forM_ newFiles removeFile

readTime :: String -> IO UTCTime
readTime x = do
  tz <- getCurrentTimeZone
  pure $ fromMaybe (error $ "invalid date: "++ x) $
    parseTimeM True defaultTimeLocale{knownTimeZones=[tz]} "%-d.%-m.%Y %-H:%M%Z" $ x ++ timeZoneName tz

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

determineType :: FilePath -> FilePath -> IO (Maybe (AufgabenTyp, Config))
determineType taskPath file = do
  config <- readFile $ taskPath </> file
  pure $ case takeExtension file of
    ".hs" -> Just (ProPa Haskell, config)
    ".prolog" -> Just (ProPa Prolog, config)
    _ -> (,config) . Logik <$> determineLogicType config

determineLogicType :: Config -> Maybe LogikAufgabenTyp
determineLogicType config
  | null realConfig = Nothing
  | otherwise = case head realConfig of
    "FillInst" -> Just FillDirect
    "FillConfig" -> Just FillQuiz
    "PickInst" -> Just PickDirect
    "PickConfig" -> Just PickQuiz
    "DecideInst" -> Just DecideDirect
    "DecideConfig" -> Just DecideQuiz
    "MaxIst" -> Just MaxIst
    "MinIst" -> Just MinIst
    "MinMaxConfig" | "Aufgabentyp: Max" `elem` comments -> Just MaxQuiz
    "MinMaxConfig" | "Aufgabentyp: Min" `elem` comments -> Just MinQuiz
    "StepInst" -> Just StepDirect
    "StepConfig" -> Just StepQuiz
    "ResolutionInst" -> Just ResolutionDirect
    "ResolutionConfig" -> Just ResolutionQuiz
    "PrologInst" -> Just PrologDirect
    "PrologConfig" -> Just PrologQuiz
    "LegalCNFConfig" -> Just InvalidCnfsQuiz
    "LegalPropositionConfig" -> Just InvalidFormulasQuiz
    "SuperfluousBracketsConfig" -> Just RemoveBracketsQuiz
    "SubTreeConfig" -> Just SubformulasQuiz
    "SynTreeConfig" -> Just TreeToFormulaQuiz
    _ -> Nothing
  where
    (comments, realConfig) = first (map $ dropWhile (\c -> c == '-' || isSpace c)) $ partition isComment $ lines config
    isComment = ("--" `isPrefixOf`)

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


type Config = String
type Name = String

formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale{knownTimeZones=[]} "%F %R:00 UTC"

template :: AufgabenTyp -> Name -> Config -> UTCTime -> UTCTime -> String
template typ name config von bis = "Aufgabe {" ++ mconcat
  [ "aufgabeName = "++ show name ++ ","
  , "aufgabeVorlesungId = VorlesungKey {unVorlesungKey = SqlBackendKey {unSqlBackendKey = 0}},"
  , "aufgabeHighscore = keine,"
  , "aufgabeVon = "++ formatUTCTime von ++","
  , "aufgabeBis = "++ formatUTCTime bis ++","
  , "aufgabeMaxPoints = Just 1.0,"
  , "aufgabeKonfiguration = " ++ show config ++  ","
  , "aufgabeHinweis = Nothing,"
  , "aufgabeTyp = " ++ show (aufgabenTypString typ) ++ ","
  , "aufgabeStatus = optional,"
  , "aufgabeServer = \"https://autotool.fmi.uni-due.de:8888\","
  , "aufgabeSignatur = \"missing\","
  , "aufgabeRestriction = Nothing,"
  , "aufgabeGewicht = Just 0.0,"
  , "aufgabeBewertung = Best"
  ]
  ++ "}"
