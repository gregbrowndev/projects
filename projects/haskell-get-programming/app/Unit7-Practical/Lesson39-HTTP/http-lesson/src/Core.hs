module Core
    ( downloadDatasets
    ) where

import qualified Ports as P

-- | TODO - replace dependency arguments with Reader Monad
downloadDatasets :: P.AppCtx -> IO (Either P.ApplicationError ())
downloadDatasets ctx = do
    datasets <- getDatasets
    print "saving datasets to file"
--    _ <- B.writeFile "data.json" datasets
    result <- saveDatasets datasets
    case result of
--        Left (P.NoaaAdapterError e) -> return $ Left $ P.NoaaAdapterApplicationError e
--        Left (P.DatasetAdapterError e) -> return $ Left $ P.DatasetAdapterApplicationError e
        Left e         -> return $ Left e
        Right datasets -> return $ Right datasets
  where
    getDatasets = P.getDatasets $ P.noaaAdapter ctx
    saveDatasets = P.saveDatasets $ P.datasetAdapter ctx
