module Adapters.DatasetAdapter (makeAdapter)
  where

import           Control.Exception

import qualified Data.ByteString.Char8 as BC

import qualified Ports                 as P

makeAdapter :: P.DatasetAdapter
makeAdapter = P.DatasetAdapter {
    P.saveDatasets = (\datasets -> do
            print "saving datasets to file"
            result <- try (BC.writeFile "data.json" datasets) :: IO (Either IOError ())
            case result of
                Left _  -> return $ Left P.DatasetSaveError
                Right _ -> return $ Right ()
        )

}
