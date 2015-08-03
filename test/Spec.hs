import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck (testProperty)
import Test.HUnit

import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(ExitFailure, ExitSuccess))
import System.Environment(setEnv)
import Control.Monad(liftM)
import Text.Regex.Posix((=~))

runCommand = "test/run_tip.sh"
blueColorEscape = "\\[34m"
commentColorEscape = "\\[38;5;28m"

main :: IO ()
main = defaultMain tests

tests = [ testGroup "Command Line - No Arguments" [
               testCase "Call without arguments returns ExitFailure" testNoArgs0
             , testCase "Call without arguments prints usage" testNoArgs1
             ]
        , testGroup "Command Line - Help" [
               testCase "Call with -h returns ExitSuccess" testHelp0
             , testCase "Call with --help returns ExitSuccess" testHelp1
             , testCase "Call with -h prints help" testHelp2
             , testCase "Call with -h equals call with --help" testHelp3
             ]
        , testGroup "Command Line - Version" [
               testCase "Call with -V returns ExitSuccess" testVersion0
             , testCase "Call with --version returns ExitSuccess" testVersion1
             , testCase "Call with --version returns ExitSuccess" testVersion2
             , testCase "Call with --numeric-version returns ExitSuccess" testVersion3
             , testCase "Call with --numeric-version prints correct version" testVersion4
             ]
        , testGroup "Command Line - Show" [
               testCase "Showing existing tip returns ExitSuccess" testShow0
             , testCase "Showing existing tip prints" testShow1
             , testCase "Showing nonexistant tip fails" testShow2
             , testCase "Showing tip uses colors" testShow3
             , testCase "Showing tip respects -n" testShow4
             , testCase "Showing tip respects --nocolor" testShow5
             ]
        , testGroup "Command Line - Edit" [
               testCase "Call with -e calls editor command" testEdit0
             , testCase "Call with -e without argument fails" testEdit1
             ]
        , testGroup "Command Line - Find" [
               testCase "Call with -f succeeds" testFind0
             , testCase "Call with -f finds one line" testFind1
             , testCase "Call with -f finds two lines" testFind2
             , testCase "Call with -f colors" testFind3
             , testCase "Call with -f respects -n" testFind4
             , testCase "Call with -f respects --nocolor" testFind5
             , testCase "Call with -f finds nothing" testFind6
             , testCase "Call with -f without argument fails" testFind7
             ]
        ]

testNoArgs0 = do
  (exitCode, _, _) <- readProcessWithExitCode runCommand [] []
  exitCode @?= ExitFailure 1

testNoArgs1 = do
  (_, out, err) <- readProcessWithExitCode runCommand [] []
  out @?= ""
  err @?= "Usage: tip [-h] [-e|-f] [OPTIONS] TIP\n"

testHelp0 = do
  (exitCode, _, _) <- readProcessWithExitCode runCommand ["-h"] []
  exitCode @?= ExitSuccess

testHelp1 = do
  (exitCode, _, _) <- readProcessWithExitCode runCommand ["--help"] []
  exitCode @?= ExitSuccess

testHelp2= do
  (_, out, err) <- readProcessWithExitCode runCommand ["--help"] []
  assert $ out /= ""
  let matches = out =~ "Tips from the terminal" :: Bool
  assert matches
  err @?= ""

testHelp3= do
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["-h"] []
  (exitCode', out', err') <- readProcessWithExitCode runCommand ["--help"] []
  out @?= out'

testVersion0 = do
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["-V"] []
  exitCode @?= ExitSuccess

testVersion1 = do
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["--version"] []
  exitCode @?= ExitSuccess

testVersion2 = do
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["-V"] []
  (exitCode', out', err') <- readProcessWithExitCode runCommand ["--version"] []
  out @?= out'

testVersion3 = do
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["--numeric-version"] []
  exitCode @?= ExitSuccess

testVersion4 = do
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["--numeric-version"] []
  (exitCode', out', err') <- readProcessWithExitCode "test/version.sh" [] []
  out @?= out'

testShow0 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["-p", "bla", "foo"] []
  exitCode @?= ExitSuccess

testShow1 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, err) <- readProcessWithExitCode runCommand ["-p", "bla", "foo"] []
  out @?= "This is a test...\n\n"

testShow2 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, _, err) <- readProcessWithExitCode runCommand ["-p", "bla", "foobar"] []
  exitCode @?= ExitFailure 2
  err @?= "Tip does not exist (create with -e).\n"

testShow3 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, _) <- readProcessWithExitCode runCommand ["-p", "bla", "baz"] []
  exitCode @?= ExitSuccess
  let matches = out =~ commentColorEscape :: Bool
  assert matches

testShow4 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, _) <- readProcessWithExitCode runCommand ["-p", "bla", "-n", "baz"] []
  exitCode @?= ExitSuccess
  let matches = out =~ commentColorEscape :: Bool
  assert $ not matches

testShow5 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, _) <- readProcessWithExitCode runCommand ["-p", "bla", "--nocolor", "baz"] []
  exitCode @?= ExitSuccess
  let matches = out =~ commentColorEscape :: Bool
  assert $ not matches

testEdit0 = do
  setEnv "TIP_DIRECTORY" "test/files"
  setEnv "EDITOR" "echo"
  (_, out, _) <- readProcessWithExitCode runCommand ["-e", "foo"] []
  out @?= "test/files/foo.gpg\n"

testEdit1 = do
  setEnv "TIP_DIRECTORY" "test/files"
  setEnv "EDITOR" "echo"
  (exitCode, _, err) <- readProcessWithExitCode runCommand ["-e"] []
  exitCode @?= ExitFailure 1
  err @?= "Requires at least 1 arguments, got 0\n"

testFind0 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, _, _) <- readProcessWithExitCode runCommand ["-f", "-p", "bla", "test"] []
  exitCode @?= ExitSuccess

testFind1 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, _) <- readProcessWithExitCode runCommand ["-f", "-p", "bla", "another"] []
  exitCode @?= ExitSuccess
  length (lines out) @?= 1

testFind2 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, _) <- readProcessWithExitCode runCommand ["-f", "-p", "bla", "test"] []
  exitCode @?= ExitSuccess
  length (lines out) @?= 2

testFind3 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (_, out, _) <- readProcessWithExitCode runCommand ["-f", "-p", "bla", "test"] []
  let matches = out =~ blueColorEscape :: Bool
  assert matches

testFind4 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (_, out, _) <- readProcessWithExitCode runCommand ["-f", "-n", "-p", "bla", "test"] []
  let matches = out =~ blueColorEscape :: Bool
  assert $ not matches

testFind5 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (_, out, _) <- readProcessWithExitCode runCommand ["-f", "--nocolor", "-p", "bla", "test"] []
  let matches = out =~ blueColorEscape :: Bool
  assert $ not matches

testFind6 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, out, _) <- readProcessWithExitCode runCommand ["-f", "-p", "bla", "foobar"] []
  exitCode @?= ExitSuccess
  out @?= ""

testFind7 = do
  setEnv "TIP_DIRECTORY" "test/files"
  (exitCode, _, err) <- readProcessWithExitCode runCommand ["-f", "-p", "bla"] []
  exitCode @?= ExitFailure 1
  err @?= "Requires at least 1 arguments, got 0\n"
