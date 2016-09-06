{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module BookServer.Types where

import Data.Aeson
import Data.Text
import GHC.Generics

data Bookshelf = Bookshelf {
    books :: [Book]
  } deriving (Show, Eq, ToJSON, FromJSON, Generic)

data Book = Book {
    author   :: Text
  , title    :: Text
  , format   :: Text
  , location :: Text
  } deriving (Show, Eq, ToJSON, FromJSON, Generic)
