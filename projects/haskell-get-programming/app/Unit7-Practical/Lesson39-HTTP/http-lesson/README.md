# Lesson 39: Making HTTP Requests in Haskell

## Quick Start

Build the project:

```shell
stack build
```

Run the program:

```shell
stack exec http-lesson-exe
```

E.g. output:

```text
Loading dotenv file: .env
Config {noaaToken = "zmVBqMPJoolObsgnLPYwkxOSYYscGxcN"}
"saving datasets to file"
Datasets were downloaded successfully
```


## Notes:

After this lesson, I'll be able to:

    - Fetch web pages by using Haskell
    - Generate more complex requests by setting headers and using HTTPS
    - Understand how to approach learning new Haskell types and libraries

In this lesson, we'll fetch National Oceanic and Atmospheric Administration
(NOAA) Climate Date API: https://www.ncdc.noaa.gov/cdo-web/webservices/v2#gettingStarted

This API requires us to send a HTTP request with a header for
authentication. We'll use the Network.HTTP.Simple library. In the next
lesson, we'll parse and work with the JSON responses received from this
API. Here are a few of them:

    - /datasets—Tells you which data sets are available
    - /locations—Gives the locations available to look up
    - /stations—Provides information on available weather observation stations
    - /data—Provides access to the raw data

To make a request we can do:

```haskell
import Network.HTTP.Simple

response = httpLBS "http://news.ycombinator.com"
```

The `response` variable is assigned immediately with no delay. This is 
because `httpLBS` uses lazy evaluation, LBS stands for lazy ByteString.

Note: Network.HTTP.Simple is quite bare bones. There is another popular
library called wreq. However, this library uses another advanced FP
abstraction called Lens. 

If we print `response` in GHCi, we'll get a delay then a long chunk of
output. If we want to extract individual parts of the response, like the
status code, we need to keep in mind that `response` is of type 
`IO (Response a)`, so we need to fmap the accessor functions:

```
ghci> getResponseStatusCode <$> response
200
```

The type of this value is also in a context,
`Control.Monad.IO.Class.MonadIO f => f Int`. An alternative solution is to
assign `response` by using `<-` instead of `=` which allows you to apply
`getResponseStatusCode` normally. 


