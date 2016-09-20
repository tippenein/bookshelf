{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib where

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Data.Text

import Book
import qualified Component

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" (text "Bookshelf")
  styleSheet "css/style.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  styleSheet "http://fonts.googleapis.com/css?family=Lato"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
  where
    styleSheet _link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", _link)
      ]) $ pure ()

data BookshelfAction
  = InitialLoad
  | NoOp
  | Query String
  | Select Book
  deriving (Eq, Read, Show)

bookshelfContainer :: MonadWidget t m => m () -> m ()
bookshelfContainer m = do
  headerWithClass "Your Bookshelf" "center"
  elClass "div" "six columns bookshelf" $ m

metadataContainer :: MonadWidget t m => m () -> m ()
metadataContainer = elClass "div" "four columns metadata"

bookshelfLoading :: MonadWidget t m => m ()
bookshelfLoading = bookshelfContainer $ do
  text "loading books..."
  pure ()

metadataLoading :: MonadWidget t m => m ()
metadataLoading = metadataContainer $ do
  text ""
  pure ()

searchInput :: MonadWidget t m => m (TextInput t)
searchInput = do
  textInput $
    def & attributes .~ constDyn
      (mconcat [ "class" =: "books-search"
               , "placeholder" =: "Search books"
               ]
      )

bodyElement :: MonadWidget t m => m ()
bodyElement = elClass "div" "wrapper" $ do
  postBuild <- getPostBuild

  q <- searchInput

  let searchInputDyn = _textInput_value q

  bookClick <- button "book"
  let bookRequestEvent = leftmost [ Just <$> updated searchInputDyn
                                  , Nothing <$ postBuild
                                  ]

  rsp :: Event t XhrResponse <- performRequestAsync $ req <$> bookRequestEvent
  let books' :: Event t [Book] = books <$> fmapMaybe decodeXhrResponse rsp
  let metadata :: Event t Book = Book "a" "b" "c" "d" <$ bookClick

  widgetHold bookshelfLoading $ fmap shelf_for books'

  widgetHold metadataLoading $ fmap metadata_for metadata
  pure ()

header :: MonadWidget t m => String -> m ()
header = el "h1" . text

metadata_for :: MonadWidget t m => Book -> m ()
metadata_for book = metadataContainer $ do

  elClass "span" "book-meta" $ do
    elClass "p" "book-format" $ text (format book)
  pure ()

shelf_for :: MonadWidget t m => [Book] -> m ()
shelf_for books = bookshelfContainer $ do
  elClass "dl" "unstyled" $
    mapM_ book_element books

book_element b = do
  elClass "span" "book-binding" $ do
    elClass "dt" "book-title" $ text (title b)
    elClass "dd" "book-author" $ text (author b)
  pure ()

headerWithClass t c = elClass "h1" c $ text t

defaultUrl = "http://localhost:8081/books"

req :: Maybe String -> XhrRequest
req Nothing = XhrRequest "GET" defaultUrl def
req (Just q)= XhrRequest "GET" uri def
  where
    uri = defaultUrl ++ "?q=" ++ q

urlEncode :: Text -> Maybe [(Text, Maybe Text)] -> Text
urlEncode base qs = undefined
