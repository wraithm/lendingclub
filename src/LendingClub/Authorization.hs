module LendingClub.Authorization 
    ( Authorization (..)
    , InvestorId (..)
    ) where

import           Control.Applicative (pure)
import           Control.Monad       (mzero)

import           Data.Aeson
import           Data.ByteString
import           Data.Scientific     (toBoundedInteger)

newtype Authorization = Authorization
    { authorization :: ByteString }
    deriving (Show, Eq)

newtype InvestorId = InvestorId Int
    deriving (Show, Eq)

instance FromJSON InvestorId where
    parseJSON (Number x) = 
        maybe mzero (pure . InvestorId) (toBoundedInteger x)
    parseJSON _ = mzero
