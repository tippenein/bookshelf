{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib where

import Reflex
import Reflex.Dom
import Control.Monad
import Control.Lens
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
bodyElement = el "div" $ do
  header "Your Bookshelf"
  bookshelfWidget

header :: MonadWidget t m => String -> m ()
header = el "h1" . text

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
  let uri query = "http://localhost:8081/books" ++ "?q=" ++ fromMaybe "" query
  let req query = XhrRequest "GET" (uri query) def
  rsp :: Event t XhrResponse <- performRequestAsync $ req <$> bookRequestEvent
  let rspBookshelf :: Event t Bookshelf = fmapMaybe (\r -> decodeXhrResponse r) rsp
  let books' :: Event t [Book] = fmap books rspBookshelf
  widgetHold loadingWidget $ fmap (\s -> shelf_for s) books' --fmap (\s -> shelves s) books'
  pure ()

  -- let req md = XhrRequest "GET" (maybe defReq (\d -> defReq ++ "&date=" ++ d) md) def
  -- rec rsp :: Event t XhrResponse <- performRequestAsync $ fmap req $
  --       leftmost [ Nothing <$ pb
  --                , fmap Just validDate ]
shelf_for :: MonadWidget t m => [Book] -> m ()
shelf_for s = do
  elClass "ul" "unstyled" $ do
    mapM_ book_element s

book_element b = do
  let textDynamic = title b <> " by " <> author b
  el "li" $ text textDynamic
