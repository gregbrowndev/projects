module Main (main) where

import           Control.Exception
import           Control.Monad
import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as AesonTypes
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import           GHC.Generics

instance A.ToJSON BC.ByteString where
    toJSON = AesonTypes.String . T.pack . BC.unpack

instance AesonTypes.FromJSON BC.ByteString where
    parseJSON = AesonTypes.withText "ByteString" (return . BC.pack . T.unpack)

data Book = Book {
    title  :: T.Text,
    author :: T.Text,
    year   :: Int
} deriving (Show, Generic)

instance A.FromJSON Book
instance A.ToJSON Book

{- 40.3 Making data types instances of FromJSOM and A.ToJSON

The language extension DeriveGeneric adds support for better
generic programming in Haskell. It allows generic instances of a type
class definition, which makes it possible to create new data types with
no extra code.
-}

myBook :: Book
myBook = Book {
    author = "Greg Brown",
    title = "Learn Haskell",
    year = 2017
}

myBookJSON :: BC.ByteString
myBookJSON = BL.toStrict $ A.encode myBook

bookFromJSON :: Maybe Book
bookFromJSON = A.decode $ BL.fromStrict myBookJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\": \"A Short History of Decay\",\"year\"=1949}"

-- decoding this would result in Nothing, not that useful in practice.
-- Instead, we can use eitherDecode to see the error:

bookResult :: Either String Book
bookResult = A.eitherDecode $ BL.fromStrict wrongJSON

{-
    ghci> bookResult
    Left "Error in $: Failed reading: satisfyWith. Expecting ':' at '=1949}'"
-}

-- Let's parse the JSON from noaa we received in the last lesson
data DatasetResponse = DatasetResponse {
    metadata :: Metadata,
    results  :: [Main.Result]
} deriving (Show, Generic)

instance A.FromJSON DatasetResponse
instance A.ToJSON DatasetResponse

data Metadata = Metadata {
    resultset :: Resultset
} deriving (Show, Generic)

instance A.FromJSON Metadata
instance A.ToJSON Metadata

data Resultset = Resultset {
    offset :: Int,
    count  :: Int,
    limit  :: Int
} deriving (Show, Generic)

instance A.FromJSON Resultset
instance A.ToJSON Resultset

data Result = Result {
    uid          :: BC.ByteString,
    mindate      :: BC.ByteString,
    maxdate      :: BC.ByteString,
    name         :: BC.ByteString,
    datacoverage :: Float,
    id           :: BC.ByteString
} deriving (Show, Generic)

instance A.FromJSON Result
instance A.ToJSON Result

loadDatasets :: FilePath -> IO (Either String DatasetResponse)
loadDatasets filePath = do
    rawJSON <- try (BC.readFile filePath) :: IO (Either IOError BC.ByteString)
    case rawJSON of
        Left err -> return $ Left (show err)
        Right bs -> return $ A.eitherDecode $ BL.fromStrict bs

printResults :: Maybe [Result] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
   forM_ results (print . name)

main :: IO ()
main = do
    datasets <- loadDatasets "data.json"
    case datasets of
        Left err -> print err
        Right ds -> printResults $ Just (results ds)
