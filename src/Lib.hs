{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib where

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Data.Text
import Data.Foldable
import Data.Traversable

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
  | Query String
  | Select Book
  deriving (Eq, Read, Show)

data ShelfAction
  = BookClick
  | SendToDevice

type MM a = Map.Map Int a

searchInput :: MonadWidget t m => m (TextInput t)
searchInput = do
  textInput $
    def & attributes .~ constDyn
      (mconcat [ "class" =: "books-search"
               , "placeholder" =: "Search books"
               ]
      )

formatPairs = Prelude.zip ts ts
  where
    ts = ["epub", "mobi", "azw3", "pdf", "other"]

bodyElement :: MonadWidget t m => m ()
bodyElement = elClass "div" "wrapper" $ do
  postBuild <- getPostBuild

  q <- searchInput

  let query = _textInput_value q
  let bookRequestEvent = leftmost [ Just <$> updated query
                                  , Nothing <$ postBuild
                                  ]

  rsp :: Event t XhrResponse <- performRequestAsync $ req <$> bookRequestEvent
  let booksResponse :: Event t [Book] = books <$> fmapMaybe decodeXhrResponse rsp

  let ops = Map.fromList $ formatPairs

  d <- dropdown "epub" (constDyn ops) def

  bookClicked <- widgetHold bookshelfLoading $ fmap shelf_for booksResponse
  -- bookClicked <- switch (current (mergeList <$> book_events))
  -- dynText =<< fmap title bookClicked

  -- widgetHold metadataLoading $ metadata_for =<< bookClicked
  pure ()

shelf_for :: MonadWidget t m => [Book] -> m ()
shelf_for books = do
  _ <- simpleList (constDyn books) bookEl
  pure ()
    -- elClass "dl" "unstyled" $
    --   fmap bookEl b

bookEl :: MonadWidget t m => Dynamic t Book -> m ()
bookEl b = do
  el "div" $ do
    elClass "dt" "book-title" $ mapDyn title b
    elClass "dd" "book-author" $ mapDyn author b
  pure()
  -- pure $ domEvent Click b

metadata_for :: MonadWidget t m => Book -> m ()
metadata_for book = do
  elClass "span" "book-meta" $ do
    el "p" $ text "format"
    elClass "p" "book-format" $ text (format book)

-- simpleList :: Dynamic [v] -> (Dynamic v -> m a) -> m (Dynamic [a])

-- shelf_for :: MonadWidget t m => Dynamic t [Book] -> m (Dynamic t [Event t Book])
-- shelf_for books = fmap bookEl books

bookshelfContainer :: MonadWidget t m => m () -> m ()
bookshelfContainer m = do
  elClass "h1" "center" $ text "your bookshelf"
  elClass "div" "six columns bookshelf" $ m

metadataContainer :: MonadWidget t m => m () -> m ()
metadataContainer = elClass "div" "four columns metadata"

bookshelfLoading :: MonadWidget t m => m ()
bookshelfLoading = bookshelfContainer $ do
  text "loading books..."
  pure ()

metadataLoading :: MonadWidget t m => m ()
metadataLoading = metadataContainer $ do
  text "..."
  pure ()

header :: MonadWidget t m => String -> m ()
header = el "h1" . text

defaultUrl = "http://localhost:8081/books"

req :: Maybe String -> XhrRequest
req Nothing = XhrRequest "GET" defaultUrl def
req (Just q)= XhrRequest "GET" uri def
  where
    uri = defaultUrl ++ "?q=" ++ q

-- urlEncode :: Text -> Maybe [(Text, Maybe Text)] -> Text
-- urlEncode base qs = undefined
