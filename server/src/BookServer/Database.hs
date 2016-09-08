{-# LANGUAGE OverloadedStrings #-}
module BookServer.Database where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, isInfixOf, toLower)

import BookServer.Types

jsonFile :: FilePath
jsonFile = "../books.json"

emptyBookshelf :: Bookshelf
emptyBookshelf = Bookshelf { books = [] }

listBooks :: Maybe Text -> IO Bookshelf
listBooks query = do
  d <- eitherDecode <$> B.readFile jsonFile
  case d of
    Left err -> error $ show err
    Right bookshelf -> return $ search query bookshelf

filtered q = filter filterBook
  where
    filterBook book =
      (q `isInfixOf` downcase (title book)) ||
      (q `isInfixOf` downcase (author book))

downcase :: Text -> Text
downcase = toLower

search :: Maybe Text -> Bookshelf -> Bookshelf
search (Just q) bookshelf = Bookshelf { books = filtered q (books bookshelf) }
search Nothing bookshelf = bookshelf
