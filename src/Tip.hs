-- Tip - quick notes for the terminal.
--
-- Written by Peter Duerr.

module Tip
    ( showTip
    , getTipDir
    , editTip
    , searchTips
    , listTips
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
                         , "--no-tty"
                         , "-d", fileName]
                       else
                         ["-q"
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
  hPutStrLn stderr "Tip does not exist (create with -e)."
  exitWith $ ExitFailure 2
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

searchTips :: String -> String -> Bool -> String -> IO ()
searchTips dir regexp noColor password = do
  allFiles <- getDirectoryContents dir
  let tipFiles = filter (isSuffixOf $ "." ++ tipExtension) allFiles
      paths = fmap (dir </>) tipFiles
  contents <- liftM catMaybes $ mapConcurrently (readExistingTip password) paths
  let lineNumbers = [1..] :: [Int]
      truncatedFileNames = fmap takeBaseName tipFiles
      annotatedLines = concatMap
                       (\(fileName, content) -> (zip3
                                                 (repeat fileName)
                                                 lineNumbers
                                                 (lines content)))
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
listTips dir = do
  allFiles <- getDirectoryContents dir
  return $ takeBaseName <$> filter (isSuffixOf $ "." ++ tipExtension) allFiles
