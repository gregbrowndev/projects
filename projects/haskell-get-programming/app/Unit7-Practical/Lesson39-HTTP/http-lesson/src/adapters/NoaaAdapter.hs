module NoaaAdapter (
        makeAdapter
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Network.HTTP.Simple

import qualified Ports                 as P

buildRequest :: BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> Request
buildRequest token host method path =
    setRequestMethod method
    $ setRequestHost host
    $ setRequestHeader "token" [token]
    $ setRequestPath path
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

getDatasetsRequest :: T.Text -> Request
getDatasetsRequest token = do
    let tokenBS = TE.encodeUtf8 token
    buildRequest tokenBS noaaHost "GET" apiPath

getDatasets :: T.Text -> IO (Either P.ApplicationError P.Datasets)
getDatasets token = do
    response <- httpBS (getDatasetsRequest token)
    let status = getResponseStatusCode response
    if status == 200
    then return $ Right (getResponseBody response)
    else return $ Left (P.ApplicationError 500 "Couldn't download datasets from NOAA")


makeAdapter :: T.Text -> P.NoaaAdapter
makeAdapter token = P.NoaaAdapter {
    P.getDatasets = getDatasets token
}
