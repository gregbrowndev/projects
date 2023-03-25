#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

{-
Having to have an entire stack project set up is a bit tideous. I've felt
friction using the tooling the entire time I've worked on this course.

The problem is the project is structured as a proper application with a
main entrypoint, i.e. app/Main.hs, but we just need to write one-off
scripts.

This article talks about Haskell/stack scripting features:
https://www.fpcomplete.com/haskell/tutorial/stack-script/

We can run this script with:

    $ stack app/Unit0-Playground/Scripting.hs

This will even download any required packages for us automatically.

Another useful tip to get a REPL ready to make HTTP requests is:

    $ stack exec --package http-conduit -- ghci

While the script interpreter is great for small programs, it doesn't
scale nicely to very large projects. At that point, we recommend you
bump up to a full-blown Stack project, which allows for more complex
configuration via a stack.yaml file. You can find more details on this
in How to Build: https://www.fpcomplete.com/haskell/tutorial/stack-build/
-}

main :: IO ()
main = do
    response <- httpLBS "https://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
