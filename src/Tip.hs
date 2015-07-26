-- Tip - quick notes for the terminal.
--
-- Written by Peter Duerr.

module Tip
    ( showTip
    , getTipDir
    , parseArgs
    , editTip
    , Flag(Edit, NoColor)
    ) where

import System.Environment(getEnv)
import System.IO.Error(catchIOError)
import System.Console.GetOpt(OptDescr(Option)
                            ,ArgDescr(NoArg)
                            ,getOpt
                            ,ArgOrder(Permute)
                            ,usageInfo)
import System.IO(hPutStrLn
                ,stderr)
import System.Exit(exitWith
                  ,exitSuccess
                  ,ExitCode(ExitFailure))
import Data.List(nub)
import System.Process(readProcess
                     ,system)
import System.Directory(doesFileExist)
import Data.Traversable(sequenceA)
import Utils

-- The environment variable with the directory for the tips
tipDirEnvVarName :: String
tipDirEnvVarName = "TIP_DIRECTORY"

-- The default directory for tips
defaultTipDir :: String
defaultTipDir = "~/.tips"

-- The default editor command
defaultEditor :: String
defaultEditor = "emacs"

-- The filename extension for tip files
tipExtension :: String
tipExtension = ".gpg"

-- Flags set from the command line
data Flag
    = Edit -- -e
    | NoColor -- -n
    | Help   -- --help
    deriving (Eq,Ord,Show)

-- The command line options
flags :: [OptDescr Flag]
flags = [Option "e" [] (NoArg Edit)
             "Edit a tip."
        ,Option "n" ["no-color"] (NoArg NoColor)
             "Do not syntax highlight tip."
        ,Option "h" ["help"] (NoArg Help)
             "Print this help message."
        ]

-- Parse the command line arguments
parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = case getOpt Permute flags argv of
    (args, tips, []) -> if Help `elem` args || null tips
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
            else return (nub args, tips)
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: tip [-e] [keyword]"

-- Returns the directory with the tips
-- If possible from environment variable, otherwise falling back to default
getTipDir :: IO String
getTipDir = catchIOError (getEnv tipDirEnvVarName) $ \_ -> return defaultTipDir

-- Get the editor command
-- If possible from environment variable, otherwise falling back to default
getEditorCommand :: IO String
getEditorCommand =
  catchIOError (getEnv "EDITOR") $
  \_ -> do {
    hPutStrLn
    stderr
    "EDITOR environment variable not set, trying to use emacs.";
    return defaultEditor
    }

-- Construnt the name of the tip file from directory, name and extension
tipName :: String -> String -> String
tipName dir tip = dir ++ "/" ++ tip ++ tipExtension

-- Show a tip
showTip :: String -> Bool -> String -> IO ()
showTip dir color tip = do
  let fileName = tipName dir tip
  contents <- readTip fileName
  printTip color contents

-- Read the contents of a tip file
readTip :: String -> IO (Maybe String)
readTip fileName = do
  exists <- doesFileExist fileName
  sequenceA (fromBool exists $ readProcess "gpg" ["-q"
                                                 , "--no-tty"
                                                 , "-d", fileName] [])

-- Print the contents of a tip file
printTip :: Bool -> Maybe String -> IO ()
printTip _ Nothing = do
  putStrLn "Tip does not exist (create with -e)."
  exitWith $ ExitFailure 1
printTip True (Just contents) = do
     pygmentizied <- readProcess "pygmentize" ["-l"
                                              , "sh"
                                              , "-O", "style=emacs"
                                              , "-f", "terminal256"
                                              ] contents
     putStrLn pygmentizied
printTip False (Just contents) = putStrLn contents

-- Edit a tip file
editTip :: String -> String -> IO ()
editTip dir tip = do
  let fileName = tipName dir tip
  editorCommand <- getEditorCommand
  exitCode <- system (concat [editorCommand, " ", fileName])
  exitWith exitCode
