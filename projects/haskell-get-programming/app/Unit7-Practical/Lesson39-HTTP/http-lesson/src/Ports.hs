module Ports (
    Config(..),
    Datasets,
    DatasetAdapter(..),
--    DatasetAdapterError(..),
    NoaaAdapter(..),
--    NoaaAdapterError(..),
    ApplicationError(..),
    AppCtx(..),
    CoreApp(..)
) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T

type Datasets = BC.ByteString


data Config = Config { noaaToken :: T.Text }
    deriving (Show, Eq)


--data NoaaAdapterError = DownloadError deriving (Show)
data NoaaAdapter = NoaaAdapter {
    getDatasets :: IO (Either ApplicationError Datasets)
}


-- Could add more useful error types to the interface,
-- e.g. file not found / already exists
--data DatasetAdapterError = SaveError deriving (Show)
data DatasetAdapter = DatasetAdapter {
    saveDatasets :: Datasets -> IO (Either ApplicationError ())
}


-- Finding complex error handing in Haskell too difficult:
--  1. its very awkward/cumbersome to map specific adapter errors into ApplicationError
--  2.

--data ApplicationError = NoaaAdapterApplicationError NoaaAdapterError
--                      | DatasetAdapterApplicationError DatasetAdapterError
--                   -- | DomainError

data ApplicationError = NoaaDownloadError | DatasetSaveError deriving (Show)

-- TODO - make ApplicationError a type class that the
--  adapter errors can become a instance of. Use the type class
--  in the use case functions to achieve polymorphism.

-- Alternatively just put all errors into one big data type

data AppCtx = AppCtx {
    noaaAdapter    :: NoaaAdapter,
    datasetAdapter :: DatasetAdapter
}

data CoreApp = CoreApp {
    downloadDatasets :: IO (Either ApplicationError ())
}
