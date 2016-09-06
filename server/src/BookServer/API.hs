{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module BookServer.API (
  bookAPI,
  BookAPI
  ) where

import BookServer.Types
import Data.Proxy
import Data.Text (Text)
import Servant.API

bookAPI :: Proxy BookAPI
bookAPI = Proxy

type BookAPI = ListBooks

type ListBooks = "books"
  :> QueryParam "q" Text
  :> Get '[JSON] Bookshelf
