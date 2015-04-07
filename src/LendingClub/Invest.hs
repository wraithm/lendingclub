{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LendingClub.Invest 
    ( InvestResponse (..)
    , ErrorMessage (..)
    , InvestConfirmation (..)
    , ExecutionStatus (..)
    ) where

import           Control.Applicative (pure, (<|>), (<$>), (<*>))

import           Data.Aeson
import           Data.Text           (Text)

import           GHC.Generics

import           LendingClub.Money

data InvestResponse
    = InvestResponse
        { orderInstructId    :: Int
        , orderConfirmations :: [InvestConfirmation]
        }
    | InvestError
        { errors :: [ErrorMessage] }
    deriving (Show, Eq)

instance FromJSON InvestResponse where
    parseJSON (Object v) =
        InvestResponse 
            <$> v .: "orderInstructId" 
            <*> v .: "orderConfirmations"
        <|> InvestError <$> v .: "errors"
    parseJSON _ = pure (InvestError [])
    
data ErrorMessage = ErrorMessage
    { field   :: Text
    , code    :: Text
    , message :: Text
    } deriving (Generic, Show, Eq)

instance FromJSON ErrorMessage where

data InvestConfirmation = InvestConfirmation
    { loanId          :: Int
    , requestedAmount :: Money
    , investedAmount  :: Money
    , executionStatus :: [ExecutionStatus]
    } deriving (Generic, Show, Eq)

instance FromJSON InvestConfirmation where

data ExecutionStatus
    = OrderFulfilled
    | LoanAmountExceeded
    | NotAnInfundingLoan
    | RequestedAmountLow
    | RequestedAmountRounded
    | AugmentedByMerge
    | ElimByMerge
    | InsufficientCash
    | NotAnInvestor
    | NotAValidInvestment
    | NoteAddedToPortfolio
    | NotAValidPortfolio
    | ErrorAddingNoteToPortfolio
    | SystemBusy
    | UnknownError
    deriving (Show, Eq)

instance FromJSON ExecutionStatus where
    parseJSON (String "ORDER_FULFILLED") = pure OrderFulfilled
    parseJSON (String "LOAN_AMNT_EXCEEDED") = pure LoanAmountExceeded
    parseJSON (String "NOT_AN_INFUNDING_LOAN") = pure NotAnInfundingLoan
    parseJSON (String "REQUESTED_AMNT_LOW") = pure RequestedAmountLow
    parseJSON (String "REQUESTED_AMNT_ROUNDED") = pure RequestedAmountRounded
    parseJSON (String "AUGMENTED_BY_MERGE") = pure AugmentedByMerge
    parseJSON (String "ELIM_BY_MERGE") = pure ElimByMerge
    parseJSON (String "INSUFFICIENT_CASH") = pure InsufficientCash
    parseJSON (String "NOT_AN_INVESTOR") = pure NotAnInvestor
    parseJSON (String "NOT_A_VALID_INVESTMENT") = pure NotAValidInvestment
    parseJSON (String "NOTE_ADDED_TO_PORTFOLIO") = pure NoteAddedToPortfolio
    parseJSON (String "NOT_A_VALID_PORTFOLIO") = pure NotAValidPortfolio
    parseJSON (String "ERROR_ADDING_NOTE_TO_PORTFOLIO") = pure ErrorAddingNoteToPortfolio
    parseJSON (String "SYSTEM_BUSY") = pure SystemBusy
    parseJSON _ = pure UnknownError
