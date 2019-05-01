-- Tip - quick notes for the terminal.
--
-- Written by Peter Duerr.

module Tip
    ( showTip
    , getTipDir
    , editTip
    , searchTips
    , listTips
    , tipDirEnvVarName
    , insufficientArgumentFailure
    , insufficientArgumentString
    , usageFailure
    , usageString
    , tipDoesNotExistFailure
    , tipDoesNotExistString
    ) where

import System.Environment(getEnv)
import System.IO.Error(catchIOError)
import System.IO(hPutStrLn
                ,stderr)
import System.Exit(exitWith
                  ,ExitCode(ExitFailure, ExitSuccess))
import System.Process(readProcess
                     ,readProcessWithExitCode
                     ,system)
import System.Directory(doesFileExist, getDirectoryContents)
import Data.List(isSuffixOf)
import System.FilePath((</>)
                      ,(<.>)
                      )
import System.FilePath.Posix(takeBaseName)
import Data.Maybe(catMaybes)
import Text.Regex.Posix((=~))
import Control.Concurrent.Async(mapConcurrently)
import Text.Printf(printf)
import Control.Monad(liftM)
import System.Console.ANSI(ColorIntensity(Dull)
                          ,setSGR
                          ,Color(Blue, Red)
                          ,SGR(SetColor)
                          ,ConsoleLayer(Foreground))

insufficientArgumentFailure :: ExitCode
insufficientArgumentFailure = ExitFailure 1

insufficientArgumentString :: String
insufficientArgumentString = "Requires at least 1 arguments, got 0\n"

usageFailure :: ExitCode
usageFailure = ExitFailure 2

usageString :: String
usageString = "Usage: tip [-h|-l] [-e|-f] [OPTIONS] TIP"

tipDoesNotExistFailure :: ExitCode
tipDoesNotExistFailure = ExitFailure 3

tipDoesNotExistString :: String
tipDoesNotExistString = "Tip does not exist (create with -e)."

-- The environment variable with the directory for the tips
tipDirEnvVarName :: String
tipDirEnvVarName = "TIP_DIRECTORY"

-- The default directory for tips
defaultTipDir :: String
defaultTipDir = "~/.tips"

-- The default editor command
defaultEditor :: String
defaultEditor = "emacsclient -c -a \"\""

-- The filename extension for tip files
tipExtension :: String
tipExtension = "gpg"

-- Returns the directory with the tips
-- If possible from environment variable, otherwise falling back to default
getTipDir :: IO String
getTipDir = catchIOError (getEnv tipDirEnvVarName) $ \_ -> return defaultTipDir

-- Get the editor command
-- If possible from environment variable, otherwise falling back to default
getEditorCommand :: IO String
getEditorCommand =
  catchIOError (getEnv "EDITOR") $
  \_ -> do
    hPutStrLn stderr "EDITOR environment variable not set, trying to use emacs."
    return defaultEditor

-- Construct the name of the tip file from directory, name and extension
tipName :: FilePath -> String -> FilePath
tipName dir tip = dir </> tip <.> tipExtension

-- Show a tip
showTip :: String -> Bool -> String -> String -> IO ()
showTip dir noColor password tip = do
  let fileName = tipName dir tip
  contents <- readTip password fileName
  printTip (not noColor) contents

readExistingTip :: String -> String -> IO (Maybe String)
readExistingTip password fileName = do
  let gpgCommandArgs = if null password then
                         ["-q"
                         , "--batch"
                         , "--no-tty"
                         , "-d", fileName]
                       else
                         ["-q"
                         , "--batch"
                         , "--no-tty"
                         , "--passphrase-fd", "0"
                         , "-d", fileName]
  (exitCode, out, err) <- readProcessWithExitCode "gpg" gpgCommandArgs password
  -- return (if exitCode == ExitSuccess then Just out else Nothing)
  case exitCode of
    ExitSuccess -> return $ Just out
    _           -> do
      hPutStrLn stderr $ "An error occured while decrypting tip: " ++ err
      return Nothing

-- Read the contents of a tip file
readTip :: String -> String -> IO (Maybe String)
readTip password fileName = do
  exists <- doesFileExist fileName
  if exists
    then readExistingTip password fileName
    else return Nothing

-- Print the contents of a tip file
printTip :: Bool -> Maybe String -> IO ()
printTip _ Nothing = do
  hPutStrLn stderr tipDoesNotExistString
  exitWith tipDoesNotExistFailure
printTip True (Just contents) =
  putStrLn =<< readProcess "pygmentize" ["-l"
                                        , "sh"
                                        , "-O", "style=emacs"
                                        , "-f", "terminal256"
                                        ] contents
printTip False (Just contents) = putStrLn contents

-- Edit a tip file
editTip :: String -> String -> IO ()
editTip dir tip = do
  let fileName = tipName dir tip
  editorCommand <- getEditorCommand
  exitCode <- system (concat [editorCommand, " ", fileName])
  exitWith exitCode

tipsInDir :: String -> IO [String]
tipsInDir dir = do
  allFiles <- getDirectoryContents dir
  return $ filter (isSuffixOf $ "." ++ tipExtension) allFiles

searchTips :: String -> String -> Bool -> String -> IO ()
searchTips dir regexp noColor password = do
  tipFiles <- tipsInDir dir
  let paths = fmap (dir </>) tipFiles
  contents <- catMaybes <$> mapConcurrently (readExistingTip password) paths
  let lineNumbers = [1..] :: [Int]
      truncatedFileNames = fmap takeBaseName tipFiles
      annotatedLines = concatMap
                       (\(fileName, content) -> zip3
                                                 (repeat fileName)
                                                 lineNumbers
                                                 (lines content))
                       (zip truncatedFileNames contents)
      matching = filter ((=~ regexp) . third) annotatedLines
      alignment = 2 + foldr (\(x, y, _) -> max $ length $ x ++ show y) 0 matching
  mapM_ (printFormated alignment regexp) matching

   where third :: (a, b, c) -> c
         third (_, _, x) = x

         printFormated :: Int -> String -> (String, Int, String) -> IO ()
         printFormated alignment regexp' (fileName, lineNumber, content) = do
           let formattedInfo = printf
                               ("%-" ++ show alignment ++ "s")
                               (fileName ++ ":" ++ show lineNumber)
           if noColor
             then putStrLn $ formattedInfo ++ content
             else do
               colorPutStr Dull Blue formattedInfo
               colorRegexpPutStrLn regexp' content

         colorPutStr :: ColorIntensity -> Color -> String -> IO ()
         colorPutStr fgi fg str = do
           setSGR [SetColor Foreground fgi fg]
           putStr str
           setSGR []

         colorRegexpPutStrLn :: String -> String -> IO ()
         colorRegexpPutStrLn regexp' content = do
           let (first, expression, rest) = content =~ regexp' :: (String, String, String)
           putStr first
           colorPutStr Dull Red expression
           putStrLn rest

listTips :: String -> IO [String]
listTips dir = takeBaseName <<$>> tipsInDir dir
  where (<<$>>) = fmap . fmap
