module Main (main) where

import           System.Exit

import qualified Adapters.ConfigAdapter  as CA
import qualified Adapters.DatasetAdapter as DA
import qualified Adapters.NoaaAdapter    as NA
import qualified Core                    as Core
import qualified Ports                   as P

bootstrap :: IO P.CoreApp
bootstrap = do
    config <- CA.getConfig ".env"
    print config  -- debug
    let noaaAdapter = NA.makeAdapter $ P.noaaToken config
    let datasetAdapter = DA.makeAdapter
    let appCtx = P.AppCtx {
        P.noaaAdapter = noaaAdapter,
        P.datasetAdapter = datasetAdapter
    }
    let app = Core.make appCtx
    return app


main :: IO ()
main = do
    app <- bootstrap
    result <- P.downloadDatasets app
    case result of
        Left err -> do
            putStrLn ("An error occurred: " ++ (show err))
            let exitCode = ExitFailure 1 -- set the exit code to 1
            exitWith exitCode
        Right _ -> putStrLn "Datasets were downloaded successfully"
