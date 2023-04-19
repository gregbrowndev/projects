module Main (main) where

import           ConfigAdapter (getConfig)
import           Core

main :: IO ()
main = do
    config <- getConfig ".env"
    print config
