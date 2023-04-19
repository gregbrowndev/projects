module ConfigAdapter (
    getConfig
) where

import qualified Configuration.Dotenv as D
import           System.Environment

import qualified Ports                as P

getConfig :: FilePath -> IO P.Config
getConfig envPath = do
    putStrLn ("Loading dotenv file: " ++ envPath)
    D.loadFile D.defaultConfig {
         D.configPath = [envPath]
      }
    noaaToken <- getEnv "NOAA_TOKEN"
    return P.Config { P.noaaToken = noaaToken }
