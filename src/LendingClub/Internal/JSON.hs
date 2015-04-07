{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module LendingClub.Internal.JSON
    ( jsonGet
--     , csvGet
--     , csvGetStream
    , investRequest
    ) where

import           Blaze.ByteString.Builder  (fromLazyByteString)
import           Network.Http.Client
import           OpenSSL
import           System.IO.Streams         (InputStream, write)

import           Control.Monad.Reader      (liftIO)

import           Data.Aeson                (FromJSON, ToJSON, encode)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as C
import           Data.Monoid               ((<>))

import           GHC.Generics

import           LendingClub.Authorization
import           LendingClub.Invest
import           LendingClub.Money

-- | Make a GET request to LendingClub's JSON api, return the parsed JSON data
jsonGet
    :: FromJSON a
    => Authorization -- ^ The authorization for the lending club user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> IO a -- ^ JSON response
jsonGet auth url = issueRequest auth url GET "application/json" jsonHandler (Nothing :: Maybe ())

{- TODO Redo for LendingClub
-- | Make a GET request to Prosper's CSV api, return the raw CSV data
csvGet
    :: UserInfo -- ^ The user name and password for the prosper user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> Prosper ByteString -- ^ Raw CSV response
csvGet userInfo url = issueRequest userInfo url GET "text/csv" concatHandler []

-- | Make a GET request to Prosper's CSV api, return the raw CSV data
csvGetStream
    :: UserInfo -- ^ The user name and password for the prosper user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> (InputStream ByteString -> IO a)
    -> Prosper a -- ^ Raw CSV response
csvGetStream userInfo url handler =
    issueRequest userInfo url GET "text/csv" (\_ is -> handler is) []
-}

data InvestRequest = InvestRequest
    { aid    :: !Int
    , orders :: ![Order]
    } deriving (Generic, Show)

data Order = Order
    { loanId          :: !Int
    , requestedAmount :: !Money
    -- , portfolioId :: !Int -- ^ Implement later
    } deriving (Generic, Show)

instance ToJSON Order where
instance ToJSON InvestRequest where

investRequest
    :: Authorization
    -> InvestorId
    -> Int -- ^ The listing id
    -> Money -- ^ Amount
    -> IO InvestResponse -- ^ JSON response from LendingClub
investRequest auth (InvestorId invId) listingId amount =
    issueRequest auth ("accounts/" <> C.pack (show invId) <> "/orders") POST "application/json" jsonHandler $
        Just InvestRequest
            { aid = invId
            , orders = [Order listingId amount]
            }

apiUrl :: ByteString
apiUrl = "api.lendingclub.com"

issueRequest
    :: (FromJSON a, ToJSON b)
    => Authorization
    -> ByteString
    -> Method
    -> ContentType
    -> (Response -> InputStream ByteString -> IO a)
    -> Maybe b
    -> IO a
issueRequest (Authorization auth) url method ct handler mBody = withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- openConnectionSSL ctx apiUrl 443
    req <- buildRequest $ do
        http method $ "/api/investor/v1/" <> url
        setHeader "Authorization" auth
        setContentType ct
    sendRequest con req (setBody mBody)
    resp <- receiveResponse con handler
    closeConnection con
    return resp
  where
    setBody Nothing = emptyBody
    setBody (Just body) = write (Just (buildJSON body))

    buildJSON = fromLazyByteString . encode
