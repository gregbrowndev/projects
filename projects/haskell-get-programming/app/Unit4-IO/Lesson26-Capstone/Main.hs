{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString    as B
import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO       as TIO
import           System.Environment (getArgs)

{- Lesson 26: Capstone Project

This capstone covers:

    - Learning about a unique binary format used by libraries
    - Writing tools to bulk-process binary data by using ByteString
    - Working with Unicode data by using the Text type
    - Structuring a large program performing a complicated I/O task

In this project, we'll use book data created by the Harvard library to
make a simple HTML document. The problem is the data is stored in a binary format called MARC (Machine-
Readable Cataloging Record). Our goal is to take the collection of MARC
records and convert it into an HTML document that lists the titles and authors of
every book in the collection.

Download the MARC file from the link below:

    - https://archive.org/download/marc_oregon_summit_records/catalog_files/ohsu_ncnm_wscc_bibs.mrc

Note: there are other files you can play around with too in that directory.
-}

-- First lets tackle the problem of converting our domain model to Html
type Author = T.Text
type Title = T.Text
type Html = T.Text

data Book = Book { author :: Author, title :: Title } deriving Show

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
    where titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
          authorInTags = mconcat ["<em>", (author book), "</em>\n"]


-- A sample set of books to work with
book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race",
    author = "Ligotti, Thomas"
}

book2 = Book {
    title = "A Short History of Decay",
    author = "Cioran, Emil"
}

book3 = Book {
    title = "The Tears of Eros",
    author = "Bataille, Georges"
}

{-
Let's try it out in GHCi:

    ghci> bookToHtml book1
    "<p>\n<strong>The Conspiracy Against the Human Race</strong>\n<em>Ligotti, Thomas</em>\n</p>\n"
-}

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [
                        "<html>\n",
                        "<head><title>books</title>",
                        "<meta charset='utf-8' />",
                        "</head>\n",
                        "<body>\n",
                        booksHtml,
                        "\n</body>\n",
                        "</html>"
                    ]
    where booksHtml = (mconcat . (map bookToHtml)) books

myBooks :: [Book]
myBooks = [book1, book2, book3]

--main :: IO ()
--main = do
--    args <- getArgs
--    let outPath = head args
--    TIO.writeFile outPath (booksToHtml myBooks)

{-
We can run this and see the created books.html file

    ghci> :main app/Unit4-IO/Lesson26-Capstone/books.html

Now lets solve the other half of the problem, reading binary MARC records.

A MARC record is formatted in three sections:

    1. a leader that contains info about the record, such as it length
    2. a directory that defines all of the keys contained within the base record
    3. a base record that contains all of the data in the order described in the directory

This is very similar to a data dictionary seen in other public serialisation formats.
You need both the leader and the directory in order to retrieve data out of the base.

We'll work with the ohsu_ncnm_wscc_bibs.mrc MARC file we downloaded earlier. First,
this file actually contains multiple MARC records. In order to split them, we need
to parse the leaders to see how long each record is.
-}

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

-- the first 24 bytes is the leader
getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

-- the first 5 bytes of the leader is the record length
rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

{-
In order to parse the whole file and split them into individual records,
we'll create a function with a signature:

    nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)

We can think of this function like getting the head and tail of a list.
-}

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where recordLength = getRecordLength marcStream

-- we can then recursive use this function to get a list of all records
allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
    where (next, rest) = nextAndRest marcStream


--main :: IO ()
--main = do
--    args <- getArgs
--    let inPath = args !! 0
--    let outPath = args !! 1
--    marcStream <- B.readFile inPath
--    let marcRecords = allRecords marcStream
--    print (length marcRecords)

{-
Let's run this

    ghci> :main app/Unit4-IO/Lesson26-Capstone/ohsu_ncnm_wscc_bibs.mrc app/Unit4-IO/Lesson26-Capstone/books.html
    140328

Now we've got all the records, we'll need to extract the title and author from each
using the directory and base record.

MARC records store info in fields. Each field has a tag and subfields with more info,
such as author, title, subject, and publication date.
-}

type MarcDirectoryRaw = B.ByteString

-- An important piece of data contained in the leader is the starting byte of the base
-- record. We can use this to parse out the directory portion. This data is 5 bytes
-- starting at the 12th character in the leader.
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
    where remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength rest
    where directoryLength = getDirectoryLength record
          rest = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else entry : splitDirectory rest
   where (entry, rest) = B.splitAt dirEntryLength directory

{-
Each entry is structured like so:
    - Tag of the field (first three chars)
    - Length of the field (next four chars)
    - Location in the base record (last 5 chars)
-}
data FieldMetadata = FieldMetadata {tag :: T.Text, fieldLength :: Int, fieldStart :: Int}
    deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata tag fieldLength fieldStart
    where (tagRaw, rest) = B.splitAt 3 entry
          (fieldLengthRaw, fieldStartRaw) = B.splitAt 4 rest
          tag = E.decodeUtf8 tagRaw
          fieldLength = rawToInt fieldLengthRaw
          fieldStart = rawToInt fieldStartRaw

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
    where recordLength = getRecordLength record
          baseAddress = getBaseAddress record
          baseRecord = B.drop baseAddress record
          baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
          byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

{-
To get the value of a field, we need to look up its location in the record using
FieldMetadata then split the raw field into its subfields. Another problem is that
we do not know for certain whether the subfield will be in the field. We need to
use Maybe.
-}

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if length results < 1
                                  then Nothing
                                  else Just (head results)
    where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
          results = filter ((== aTag) . tag) metadata

lookupSubfield :: (Maybe FieldMetadata) -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
        if results == []
        then Nothing
        else Just ((T.drop 1 . head) results)  -- drop first char which is subfield code
    where rawField = getTextField record fieldMetadata
          subfields = T.split (== fieldDelimiter) rawField
          results = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
    where entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
    where records = allRecords marcStream
          titles = map lookupTitle records
          authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) ->
                            Book {title = fromJust title, author = fromJust author}
                         ) justPairs
    where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

main :: IO ()
main = do
    args <- getArgs
    let inPath = args !! 0
    let outPath = args !! 1
    marcStream <- B.readFile inPath
    let html = processRecords 500 marcStream
    TIO.writeFile outPath html

{-
Let's run the final version of our program:

    ghci> :main app/Unit4-IO/Lesson26-Capstone/ohsu_ncnm_wscc_bibs.mrc  app/Unit4-IO/Lesson26-Capstone/books.html

-}
