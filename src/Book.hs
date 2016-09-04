{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Book where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS

data Book
  = Book
  {
    author :: String
  , title :: String
  , location :: String
  , format :: String
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

type Bookshelf = [Book]

bookDump :: IO [Book]
bookDump = do
  d <- decode <$> LBS.readFile "books.json"
  case d of
    Nothing -> pure []
    Just a -> pure a
