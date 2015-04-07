{-# LANGUAGE DeriveGeneric #-}

module LendingClub.Note (Note (..)) where

import           Data.Aeson
import           Data.Text           (Text)

import           GHC.Generics

import           LendingClub.Listing (Grade (..))
import           LendingClub.Money

data Note = Note
    { loanId           :: Int
    , noteId           :: Int
    , orderId          :: Int
    , interestRate     :: Double
    , loanLength       :: Int -- Term
    , loanStatus       :: Text -- Check if should be an ADT
    , grade            :: Grade
    , loanAmount       :: Money
    , noteAmount       :: Money
    , paymentsReceived :: Money
    } deriving (Generic, Show, Eq)
    -- , issueDate :: Time
    -- , orderDate :: Time
    -- , loanStatusDate :: Time

instance FromJSON Note where
