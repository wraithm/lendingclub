{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LendingClub.Commands
    ( invest
    , account
    , allListings
    , notes
    , listingFromNote
    ) where

import           Data.Aeson
import           Data.ByteString.Char8     as C
import           Data.Functor              ((<$>))
import           Data.Monoid               ((<>))
import           Data.Vector               (Vector)
import qualified Data.Vector               as V

import           GHC.Generics

import           LendingClub.Account
import           LendingClub.Authorization
import           LendingClub.Invest
import           LendingClub.Internal.JSON
import           LendingClub.Listing
import           LendingClub.Note          as N
import           LendingClub.Money

invest
    :: Authorization
    -> InvestorId
    -> Money
    -> Listing
    -> IO InvestResponse
invest auth i amt l = investRequest auth i (listingId l) amt

newtype LCListings = LCListings
    { loans :: Vector Listing }
    deriving Generic

instance FromJSON LCListings where

allListings :: Authorization -> IO (Vector Listing)
allListings auth = loans <$> jsonGet auth "loans/listing"

account :: Authorization -> InvestorId -> IO Account
account auth (InvestorId i) =
    jsonGet auth ("accounts/" <> C.pack (show i) <> "/summary")

newtype LCNotes = LCNotes
    { myNotes :: Vector Note }
    deriving Generic

instance FromJSON LCNotes where

notes :: Authorization -> InvestorId -> IO (Vector Note)
notes auth (InvestorId i) =
    myNotes <$> jsonGet auth ("accounts/" <> C.pack (show i) <> "/notes")

-- | Does not talk to LendingClub's listings endpoint.
-- This uses in-memory data to retrieve listings.
--
-- TODO This thing is pretty much useles... Need to access historical listings
listingFromNote :: Authorization -> Note -> IO (Maybe Listing)
listingFromNote auth (Note { N.loanId = lid }) = do
    listings <- allListings auth
    return $ V.find (\l -> listingId l == lid) listings
