import System.Environment(getArgs)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Conversions(fromText)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    imageFile <- B.readFile filePath
    putStrLn "Bytes: "
    print (B.length imageFile)
    putStrLn "Characters: "
    print ((T.length . E.decodeUtf8) imageFile)
