{-# LANGUAGE DeriveGeneric #-}

module LendingClub.Account (Account (..)) where

import           Data.Aeson

import           GHC.Generics

import           LendingClub.Authorization
import           LendingClub.Money

-- | A data structure for holding account information for LendingClub
data Account = Account
    { investorId           :: InvestorId
    , availableCash        :: Money
    , accountTotal         :: Money
    , accruedInterest      :: Money
    , infundingBalance     :: Money
    , receivedInterest     :: Money
    , receivedPrincipal    :: Money
    , receivedLateFees     :: Maybe Money
    , outstandingPrincipal :: Money
    , totalNotes           :: Int
    , totalPortfolios      :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON Account where
