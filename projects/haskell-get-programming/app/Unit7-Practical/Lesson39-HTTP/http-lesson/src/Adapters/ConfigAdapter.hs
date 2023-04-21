module Adapters.ConfigAdapter (
    getConfig
) where

import qualified Configuration.Dotenv as D
import qualified Data.Text            as T
import qualified System.Environment   as E

import qualified Ports                as P


getConfig :: FilePath -> IO P.Config
getConfig envPath = do
    putStrLn ("Loading dotenv file: " ++ envPath)
    D.loadFile D.defaultConfig {
         D.configPath = [envPath]
      }
    noaaToken <- T.pack <$> E.getEnv "NOAA_TOKEN"
    return P.Config { P.noaaToken = noaaToken }
