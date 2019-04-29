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
import Paths_tip(version)
import Data.Version(showVersion)
import System.IO(hPutStrLn
                ,stderr)
import Tip

data Commands = Show_ {tip :: [String], noColor :: Bool, password :: String}
              | Edit_ {tip_ :: [String]}
              | Find_ {regexp :: String, noColor :: Bool, password :: String}
              | List_ {}
              deriving (CmdArgs.Data, CmdArgs.Typeable, Show, Eq)

show_ :: Commands
show_ = Show_ { tip = CmdArgs.def
                      &= CmdArgs.args
                      &= CmdArgs.typ "TIP, ..."
              , noColor = CmdArgs.def
                          &= CmdArgs.help "Do not syntax-highlight the output."
              , password = CmdArgs.def
                           &= CmdArgs.typ "PASSWORD"
                           &= CmdArgs.help "Use symmetric encrytpion password." }
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
                         &= CmdArgs.help "Do not colorize the output."
             , password = CmdArgs.def
                          -- for some reason needs to be different from above, so
                          -- added space
                          &= CmdArgs.typ "PASSWORD "
                          &= CmdArgs.help "Use symmetric encrytpion password. " }
       &= CmdArgs.explicit
       &= CmdArgs.name "-f"
       &= CmdArgs.help "Find a tip (by regexp)."

list :: Commands
list = List_ {}
       &= CmdArgs.explicit
       &= CmdArgs.name "-l"
       &= CmdArgs.help "List all tips"

progModes :: CmdArgs.Mode (CmdArgs.CmdArgs Commands)
progModes = CmdArgs.cmdArgsMode $ CmdArgs.modes [show_, edit, find, list]
          &= CmdArgs.program "tip"
          &= CmdArgs.summary "Tips from the terminal..."
          &= CmdArgs.helpArg [CmdArgs.explicit,
                              CmdArgs.name "help",
                              CmdArgs.name "h"]
          &= CmdArgs.versionArg [CmdArgs.explicit,
                                 CmdArgs.name "version",
                                 CmdArgs.name "V",
                                 CmdArgs.summary ("tip " ++ showVersion version)]

main :: IO ()
main = do
  args' <- getArgs
  case args' of
    [] -> do
        hPutStrLn stderr "Usage: tip [-h|-l] [-e|-f] [OPTIONS] TIP"
        exitWith $ ExitFailure 1
    _ -> do
        opts <- CmdArgs.cmdArgsRun progModes
        dir <- getTipDir
        case opts of
          Show_ tips noColor' password' -> mapM_ (showTip dir noColor' password') tips
          Edit_ tip' -> mapM_ (editTip dir) tip'
          Find_ regexp' noColor' password' -> searchTips dir regexp' noColor' password'
          List_ -> (listTips dir) >>= mapM_ putStrLn
