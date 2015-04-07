{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module LendingClub.Listing
    ( Listing (..)
    , Offer (..)
    , Credit (..)
    , Grade (..)
    , SubGrade (..)
    , Purpose (..)
    ) where

import           Control.Applicative
import           Control.Monad       (mzero)

import           Data.Aeson
import           Data.Typeable

import           GHC.Generics

import           LendingClub.Money

data Listing = Listing
    { listingId    :: !Int

    , listingGrade :: !Grade -- ^ analogous to ProsperRating
    , subGrade     :: !SubGrade -- ^ maybe analogous to score?
    , purpose      :: !Purpose -- ^ "purpose" in the API

    -- Market data
    , fundedAmount :: !Money

    , offer        :: !Offer -- ^ Contract data
    , credit       :: !Credit -- ^ Credit data
    } deriving Show

-- | Two listings are equivalent if their serial numbers are equal
instance Eq Listing where
    (Listing { listingId = id1 }) == (Listing { listingId = id2 }) =
        id1 == id2

-- | Data related to the listing's offer and contract terms
data Offer = Offer
    { requestAmount :: !Money
    , rate          :: !Double -- ^ Interest rate for the borrower
    , termInMonths  :: !Int -- ^ Integer number of months
    } deriving Show
--     estimatedLossRate :: Double -- Maybe Money?
--     estimatedReturn :: Double -- Maybe Money?
--     startDate       :: Date -- Add dates later
--     creationDate    :: Date
--     verificationStage :: Maybe Int -- Don't know what VerificationStage is...
--     Add WholeLoanStartDate and WholeLoanEndDate

-- | Data related to the credibility of the listing
data Credit = Credit
    { fico                     :: !Int
    , bankcardUtilization      :: !Double
    , isHomeowner              :: !Bool
    , debtToIncome             :: !Double
    , monthsEmployed           :: !(Maybe Int)
    , currentDelinquencies     :: !(Maybe Int)
    , amountDelinquent         :: !(Maybe Money)
    , openCreditLines          :: !(Maybe Int)
    , totOpenRevolvingAccts    :: !(Maybe Int)
    , revolvingBalance         :: !(Maybe Money)
    , revolvingAvailableCredit :: !(Maybe Int) -- ^ Percent
    } deriving Show
--     firstCreditLine :: Date
--     creditLinesLast7Years :: Int
--     inquiriesLast6Months :: Int
--     delinquenciesLast7Years :: Int -- These are maybe too specific to Prosper
--     publicRecordsLast10Years :: Int
--     oldestTradeOpenDate :: Date

-- TODO Need to compare nullables to Prosper's nullables
instance FromJSON Listing where
    parseJSON (Object v) = Listing
            <$> v .: "id"

            -- Lending club specific data
            <*> v .: "grade"
            <*> v .: "subGrade"
            <*> v .: "purpose"

            -- Market data
            <*> v .: "fundedAmount"

            <*> (Offer
            <$> v .: "loanAmount"
            <*> v .: "intRate" -- Interest Rate, might not be the same as "borrower rate"
            <*> v .: "term")

            <*> (Credit
            <$> v .: "ficoRangeLow" -- Choose the low-end of the FICO range
            <*> v .: "bcUtil"
            <*> (lcHomeowner <$> v .: "homeOwnership")
            <*> v .: "dti"
            <*> v .:? "empLength"
            <*> v .:? "accNowDelinq"
            <*> v .:? "delinqAmnt"
            <*> v .:? "openAcc" -- TODO check if "open credit lines" is the same as in Prosper
            <*> v .:? "numRevAccts"
            <*> v .:? "revolBal"
            <*> v .:? "revolUtil" -- TODO check if this is the same as in Prosper
            )
    parseJSON _ = mzero

data Homeownership
    = RENT
    | OWN
    | MORTGAGE
    | OTHER
    deriving (Show, Eq, Typeable, Generic)

instance FromJSON Homeownership where

lcHomeowner :: Homeownership -> Bool
lcHomeowner OWN = True
lcHomeowner MORTGAGE = True -- TODO This is debatable... We should think about this
lcHomeowner _ = False

data Grade
    = G
    | F
    | E
    | D
    | C
    | B
    | A
    deriving (Show, Eq, Ord, Typeable, Generic, Read, Enum)

instance FromJSON Grade where
instance ToJSON Grade where

data SubGrade
    = G5
    | G4
    | G3
    | G2
    | G1
    | F5
    | F4
    | F3
    | F2
    | F1
    | E5
    | E4
    | E3
    | E2
    | E1
    | D5
    | D4
    | D3
    | D2
    | D1
    | C5
    | C4
    | C3
    | C2
    | C1
    | B5
    | B4
    | B3
    | B2
    | B1
    | A5
    | A4
    | A3
    | A2
    | A1
    deriving (Show, Eq, Ord, Typeable, Generic, Read, Enum)

instance FromJSON SubGrade where

data Purpose
    = DebtConsolidation
    | Medical
    | HomeImprovement
    | RenewableEnergy
    | SmallBusiness
    | Wedding
    | Vacation
    | Moving
    | House
    | Car
    | MajorPurchase
    | CreditCard
    | Other
    deriving (Show, Eq, Read)

instance FromJSON Purpose where
    parseJSON (String "debt_consolidation") = pure DebtConsolidation
    parseJSON (String "medical") = pure Medical
    parseJSON (String "home_improvement") = pure HomeImprovement
    parseJSON (String "renewable_energy") = pure RenewableEnergy
    parseJSON (String "small_business") = pure SmallBusiness
    parseJSON (String "wedding") = pure Wedding
    parseJSON (String "vacation") = pure Vacation
    parseJSON (String "moving") = pure Moving
    parseJSON (String "house") = pure House
    parseJSON (String "car") = pure Car
    parseJSON (String "major_purchase") = pure MajorPurchase
    parseJSON (String "credit_card") = pure CreditCard
    parseJSON _ = pure Other
