{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib where

import Reflex
import Reflex.Dom
import Control.Monad
import qualified Data.Map as Map
import Data.Monoid
import Data.Text
import Book
import Data.Maybe
import System.IO.Unsafe

-- fetchBooks = unsafePerformIO getBooks
headElement :: MonadWidget t m => m ()
headElement = do
  el "title" (text "Bookshelf")
  styleSheet "css/style.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  styleSheet "http://fonts.googleapis.com/css?family=Lato"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

data BookshelfAction
  = InitialLoad
  | Search String
  deriving (Eq, Ord, Read, Show)

urlEncode :: Text -> Maybe [(Text, Maybe Text)] -> Text
urlEncode base qs = undefined

bodyElement :: MonadWidget t m => m ()
bodyElement = elClass "div" "wrapper" $ do
  headerWithClass "Your Bookshelf" "right"
  bookshelfWidget

header :: MonadWidget t m => String -> m ()
header = el "h1" . text

headerWithClass t c = elClass "h1" c $ text t

req :: Maybe String -> XhrRequest
req query = XhrRequest "GET" uri def
  where
    uri = "http://localhost:8081/books" ++ "?q=" ++ fromMaybe "" query

bookshelfWidget :: MonadWidget t m => m ()
bookshelfWidget = el "div" $ do
  postBuild <- getPostBuild

  q <- textInput def
  searchClick <- button "search"
  let searchEvent = tagDyn (value q) searchClick
      loadingWidget = text "fetching books"
      bookRequestEvent = leftmost [ Just <$> searchEvent
                                  , Nothing <$ postBuild
                                  ]
  rsp :: Event t XhrResponse <- performRequestAsync $ req <$> bookRequestEvent
  let rspBookshelf :: Event t Bookshelf = fmapMaybe decodeXhrResponse rsp
  let books' :: Event t [Book] = fmap books rspBookshelf
  widgetHold loadingWidget $ fmap shelf_for books'
  pure ()

shelf_for :: MonadWidget t m => [Book] -> m ()
shelf_for s = unstyledListWith book_element s

book_element b = do
  let textDynamic = title b <> " by " <> author b
  elClass "li" "book-binding" $ text textDynamic

unstyledListWith mapF items =
  elClass "ul" "unstyled" $
    mapM_ mapF items
