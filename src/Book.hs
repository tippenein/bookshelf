{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Book where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

emptyBookshelf = Bookshelf []

data Book
  = Book
  { author :: String
  , title :: String
  , location :: String
  , format :: String
  } deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Bookshelf
  = Bookshelf
  { books :: [Book] }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
