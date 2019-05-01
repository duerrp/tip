import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit

import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(ExitFailure, ExitSuccess))
import System.Environment(setEnv)
import Control.Monad(liftM)
import Text.Regex.Posix((=~))

import Tip

runCommand = "test/run_tip.sh"
blueColorEscape = "\\[34m"
commentColorEscape = "\\[38;5;28m"

main :: IO ()
main = defaultMain tests

tests = [ testGroup "Command Line - No Arguments" [

            testCase "Call without arguments returns ExitFailure, prints usage" $
              checkUsageFailure []

            ]
        , testGroup "Command Line - Help" [

              testCase "Call with -h returns ExitSuccess" $
                checkSucceeds ["-h"]

            , testCase "Call with --help returns ExitSuccess" $
                checkSucceeds ["--help"]

            , testCase "Call with -h prints help" $
                assertTipOutMatches "Tips from the terminal" ["--help"]

            , testCase "Call with -h equals call with --help" $
                assertTipOutsEqual ["-h"] ["--help"]
            ]

        , testGroup "Command Line - Version" [

               testCase "Call with -V returns ExitSuccess" $
                 checkSucceeds ["-V"]

             , testCase "Call with --version returns ExitSuccess" $
                 checkSucceeds ["--version"]

             , testCase "Call with --version equals call with -V" $
                 assertTipOutsEqual ["-V"] ["--version"]

             , testCase "Call with --numeric-version returns ExitSuccess" $
                 checkSucceeds ["--numeric-version"]

             , testCase "Call with --numeric-version prints correct version" $
                 do
                   out' <- getVersionNumber
                   assertTipOutMatches out' ["--numeric-version"]
             ]

        , testGroup "Command Line - Show" [

               testCase "Showing existing tip returns ExitSuccess" $
                 checkSucceeds ["-p", "bla", "foo"]

             , testCase "Showing existing tip prints" $
                 assertTipOutMatches "This is a test...\n\n" ["-p", "bla", "foo"]

             , testCase "Showing nonexistant tip fails" $
                 checkTipDoesNotExistFailure ["-p", "bla", "foobar"]

             , testCase "Showing tip uses colors" $
                 assertTipOutCommentColor True ["-p", "bla", "baz"]

             , testCase "Showing tip respects -n" $
                 assertTipOutCommentColor False ["-p", "bla", "-n", "baz"]

             , testCase "Showing tip respects --nocolor" $
                 assertTipOutCommentColor False ["-p", "bla", "--nocolor", "baz"]
             ]

        , testGroup "Command Line - Edit" [

               testCase "Call with -e calls editor command for existent file" $
                assertTipOutEquals "test/files/foo.gpg\n" ["-e", "foo"]

             , testCase "Call with -e calls editor command for nonexistent file" $
                assertTipOutEquals "test/files/foobarbaz.gpg\n" ["-e", "foobarbaz"]

             , testCase "Call with -e without argument fails" $
                checkInsufficentArgumentsFailure ["-e"]
             ]

        , testGroup "Command Line - Find" [

               testCase "Call with -f succeeds even if nothing is found" $
                checkSucceeds ["-f", "-p", "bla", "testing"]

             , testCase "Call with -f finds one line when one line is present" $
                 assertTipOutNLines 1 ["-f", "-p", "bla", "another"]

             , testCase "Call with -f finds two lines when two lines are present" $
                assertTipOutNLines 2 ["-f", "-p", "bla", "test"]

             , testCase "Call with -f colors" $
                 assertTipOutBlueColor True ["-f", "-p", "bla", "test"]

             , testCase "Call with -f respects -n" $
                 assertTipOutBlueColor False ["-f", "-n", "-p", "bla", "test"]

             , testCase "Call with -f respects --nocolor" $
                assertTipOutBlueColor False ["-f", "--nocolor", "-p", "bla", "test"]

             , testCase "Call with -f does not find string not present in tips" $
                assertTipOutEmpty ["-f", "-p", "bla", "foobar"]

             , testCase "Call with -f without argument fails" $
                checkInsufficentArgumentsFailure ["-f", "-p", "bla"]
             ]

        , testGroup "Command Line - List" [

               testCase "Call with -l lists three correct tips" $
                 assertTipOutEquals "bar\nbaz\nfoo\n" ["-l"]
             ]
        ]

hasColorEscape :: String -> String -> Bool
hasColorEscape = flip (=~)

hasCommentColor :: String -> Bool
hasCommentColor = hasColorEscape commentColorEscape

hasBlueColor :: String -> Bool
hasBlueColor = hasColorEscape blueColorEscape

testEnv :: IO ()
testEnv = do
  setEnv tipDirEnvVarName "test/files"
  setEnv "EDITOR" "echo"

getVersionNumber :: IO String
getVersionNumber = second <$> readProcessWithExitCode "test/version.sh" [] []
  where second (a, b, c) = b

runTip :: [String] -> IO (ExitCode, String, String)
runTip args = do
  testEnv
  readProcessWithExitCode runCommand args []

getTipOut :: [String] -> IO String
getTipOut args = do
  (exitCode, out, err) <- runTip args
  exitCode @?= ExitSuccess
  err @?= ""
  return out

checkSucceeds :: [String] -> Assertion
checkSucceeds args = do
  (exitCode, _, _) <- runTip args
  exitCode @?= ExitSuccess

checkFailure :: ExitCode -> String -> [String] -> Assertion
checkFailure exitCode err args = do
  (exitCode, out, err) <- runTip args
  exitCode @?= exitCode
  err @?= err
  out @?= ""

checkInsufficentArgumentsFailure :: [String] -> Assertion
checkInsufficentArgumentsFailure = checkFailure insufficientArgumentFailure insufficientArgumentString

checkUsageFailure :: [String] -> Assertion
checkUsageFailure = checkFailure usageFailure usageString

checkTipDoesNotExistFailure :: [String] -> Assertion
checkTipDoesNotExistFailure = checkFailure tipDoesNotExistFailure tipDoesNotExistString

assertTipOut :: (String -> Assertion) -> [String] -> Assertion
assertTipOut assertion args = do
  out <- getTipOut args
  assertion out

assertTipOutMatches :: String -> [String] -> Assertion
assertTipOutMatches match = assertTipOut $ \out -> assert (out =~ match :: Bool)

assertTipOutEquals :: String -> [String] -> Assertion
assertTipOutEquals match = assertTipOut $ \out -> out @?= match

assertTipOutsEqual :: [String] -> [String] -> Assertion
assertTipOutsEqual args args' = do
  out <- getTipOut args
  out' <- getTipOut args'
  out @?= out'

assertTipOutNLines :: Int -> [String] -> Assertion
assertTipOutNLines n = assertTipOut $ \out -> length (lines out) @?= n

assertTipOutEmpty :: [String] -> Assertion
assertTipOutEmpty = assertTipOutEquals ""

assertTipOutCommentColor :: Bool -> [String] -> Assertion
assertTipOutCommentColor True = assertTipOut $ assert . hasCommentColor
assertTipOutCommentColor False = assertTipOut $ assert . not . hasCommentColor

assertTipOutBlueColor :: Bool -> [String] -> Assertion
assertTipOutBlueColor True = assertTipOut $ assert . hasBlueColor
assertTipOutBlueColor False = assertTipOut $ assert . not . hasBlueColor
