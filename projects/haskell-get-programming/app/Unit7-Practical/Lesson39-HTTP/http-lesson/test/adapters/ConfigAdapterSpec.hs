import           System.Directory        (getCurrentDirectory)
import           System.IO.Temp          (withTempDirectory)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           ConfigAdapter


prop_loadsConfig :: Property
prop_loadsConfig =
    monadicIO $ do run (withTempDirectory "." "config-test" f)
  where
    f :: FilePath -> IO Bool
    f tmpDir = do
        let noaaToken = "abcde"  -- this should be parameterised
        currentDir <- getCurrentDirectory
        let configPath = tmpDir ++ "/.env"
        writeFile configPath ("NOAA_TOKEN=" ++ noaaToken ++ "\n")
        config <- getConfig configPath
        return $ config == Config { noaaToken = noaaToken }


main :: IO Result
main = do
    putStrLn "Running ConfigAdapterSpec tests"
    quickCheckWithResult stdArgs { maxSuccess = 1000 } prop_loadsConfig
