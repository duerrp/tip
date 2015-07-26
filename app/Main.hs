-- Tip - quick notes for the terminal.
--
-- Written by Peter Duerr.

module Main where

import System.Environment(getArgs)
import Tip

main :: IO ()
main = do
  (args, tip) <- getArgs >>= parseArgs
  dir <- getTipDir
  let color = NoColor `notElem` args
  if Edit `elem` args then mapM_ (editTip dir) tip
                           else mapM_ (showTip dir color) tip
