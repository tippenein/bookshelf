{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Book where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T

instance Ord Book where
  b1@(Book a t _ _) `compare` b2@(Book a2 t2 _ _) = a2 `compare` a

data Book
  = Book
  { author :: Text
  , title :: Text
  , location :: Text
  , format :: Text
  } deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data Bookshelf
  = Bookshelf
  { books :: [Book] }
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
