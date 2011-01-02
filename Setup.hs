import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.UserHooks

import System.Directory
import System.Exit
import System.FilePath
import System.Process

testSuiteExe = "b1-tests"

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks { runTests = runTestSuite }

runTestSuite :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTestSuite _ _ _ localBuildInfo = do
  let testDir = buildDir localBuildInfo </> testSuiteExe
  setCurrentDirectory testDir
  exitCode <- system testSuiteExe
  exitWith exitCode

