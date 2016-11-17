{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Book where

import GHC.Generics
import Data.Aeson

instance Ord Book where
  b1@(Book a t _ _) `compare` b2@(Book a2 t2 _ _) = a2 `compare` a

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
