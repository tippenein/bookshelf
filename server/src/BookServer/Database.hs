{-# LANGUAGE OverloadedStrings #-}
module BookServer.Database (listBooks) where

import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text (Text, isInfixOf)

import BookServer.Types

jsonFile :: FilePath
jsonFile = "../books.json"

getJSON = B.readFile jsonFile

emptyBookshelf = Bookshelf { books = []}

listBooks :: Maybe Text -> IO Bookshelf
listBooks query = do
  d <- eitherDecode <$> getJSON
  case d of
    Left err -> error $ show err
    Right bookshelf -> return $ search query bookshelf

filtered q = filter filterBook
  where
    filterBook book =
      (q `isInfixOf` title book) ||
      (q `isInfixOf` author book)

search :: Maybe Text -> Bookshelf -> Bookshelf
search query bookshelf =
  case query of
    Just q -> Bookshelf { books = filtered q (books bookshelf) }
    Nothing -> bookshelf
