module Core ( make ) where

import qualified Ports as P

-- | TODO - replace dependency arguments with Reader Monad
downloadDatasets :: P.AppCtx -> IO (Either P.ApplicationError ())
downloadDatasets ctx = do
    datasets <- getDatasets
--    _ <- B.writeFile "data.json" datasets
    saved <- case datasets of
        Left err -> return $ Left err
        Right ds -> saveDatasets ds
    case saved of
        Left err -> return $ Left err
        Right _  -> return $ Right ()
  where
    getDatasets = P.getDatasets $ P.noaaAdapter ctx
    saveDatasets = P.saveDatasets $ P.datasetAdapter ctx


make :: P.AppCtx -> P.CoreApp
make ctx = P.CoreApp {
    P.downloadDatasets = downloadDatasets ctx
}
