-- Tip - quick notes for the terminal.
--
-- Written by Peter Duerr.
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment(getArgs)
import qualified System.Console.CmdArgs as CmdArgs
import System.Console.CmdArgs((&=))
import System.Exit(exitWith
                  ,ExitCode(ExitFailure))
import Tip

data Commands = Show_ {tip :: [String], noColor :: Bool}
              | Edit_ {tip_ :: [String]}
              | Find_ {regexp :: String, noColor :: Bool}
              deriving (CmdArgs.Data, CmdArgs.Typeable, Show, Eq)

show_ :: Commands
show_ = Show_ { tip = CmdArgs.def
                      &= CmdArgs.args
                      &= CmdArgs.typ "TIP, ..."
              , noColor = CmdArgs.def
                          &= CmdArgs.help "Do not syntax-highlight the output." }
        &= CmdArgs.explicit
        &= CmdArgs.name "--show"
        &= CmdArgs.help "Show a tip."
        &= CmdArgs.auto

edit :: Commands
edit = Edit_ { tip_ = CmdArgs.def
                      &= CmdArgs.argPos 0
                      &= CmdArgs.typ "TIP" }
       &= CmdArgs.explicit
       &= CmdArgs.name "-e"
       &= CmdArgs.help "Edit a tip."

find :: Commands
find = Find_ { regexp = CmdArgs.def
                        &= CmdArgs.argPos 1
                        &= CmdArgs.typ "REGEXP"
             , noColor = CmdArgs.def
                         &= CmdArgs.help "Do not colorize the output."}
       &= CmdArgs.explicit
       &= CmdArgs.name "-f"
       &= CmdArgs.help "Find a tip (by regexp)."

progModes :: CmdArgs.Mode (CmdArgs.CmdArgs Commands)
progModes = CmdArgs.cmdArgsMode $ CmdArgs.modes [show_, edit, find]
          &= CmdArgs.program "tip"
          &= CmdArgs.summary "Tips from the terminal..."
          &= CmdArgs.helpArg [CmdArgs.explicit, CmdArgs.name "help", CmdArgs.name "h"]

main :: IO ()
main = do
  args' <- getArgs
  case args' of
    [] -> do
        putStrLn "Usage: tip [-h] [-e|-f] [OPTIONS] TIP"
        exitWith $ ExitFailure 1
    _ -> do
        opts <- CmdArgs.cmdArgsRun progModes
        dir <- getTipDir
        case opts of
          Show_ ttip noColor' -> mapM_ (showTip dir noColor') ttip
          Edit_ ttip -> mapM_ (editTip dir) ttip
          Find_ regexp' noColor' -> searchTips dir regexp' noColor'
