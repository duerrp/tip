-- Tip - quick notes for the terminal.
--
-- Written by Peter Duerr.

module Tip
    ( showTip
    , getTipDir
    , parseArgs
    , editTip
    , Flag(Edit)
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

tipDirEnvVarName :: String
tipDirEnvVarName = "TIP_DIRECTORY"

defaultTipDir :: String
defaultTipDir = "~/.tips"

tipExtension :: String
tipExtension = ".gpg"

data Flag
    = Help   -- --help
    | Edit -- -e
    deriving (Eq,Ord,Show)

flags :: [OptDescr Flag]
flags = [Option "e" [] (NoArg Edit)
             "Edit a tip."
        ,Option "h"    ["help"] (NoArg Help)
             "Print this help message."
        ]

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

getTipDir :: IO String
getTipDir = do {
  getEnv tipDirEnvVarName;
  } `catchIOError` \_ -> return defaultTipDir

getEditorCommand :: IO String
getEditorCommand = do {
  getEnv "EDITOR";
  } `catchIOError` \_ -> do {
  hPutStrLn stderr "EDITOR environment variable not set, trying to use emacs.";
  return "emacs"
  }

tipName :: String -> String -> String
tipName dir tip = dir ++ "/" ++ tip ++ tipExtension

showTip :: String -> String -> IO ()
showTip dir tip = do
  let fileName = tipName dir tip
  content <- readProcess "gpg" ["-q"
                               , "--no-tty"
                               , "-d", fileName] []
  pygmentizied <- readProcess "pygmentize" ["-l"
                                           , "sh"
                                           , "-O", "style=emacs"
                                           , "-f", "terminal256"
                                           ] content
  putStrLn pygmentizied

editTip :: String -> String -> IO ()
editTip dir tip = do
  let fileName = tipName dir tip
  editorCommand <- getEditorCommand
  exitCode <- system (concat [editorCommand, " ", fileName])
  exitWith exitCode
