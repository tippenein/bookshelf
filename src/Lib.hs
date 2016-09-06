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


-- bookshelf = do
--   pb :: Event t () <- getPostBuild
--   let uri = "http://localhost:8080/books"
--   let req = XhrRequest "GET" uri def
--   submitEvent <- button "Submit"
--   let submitApiKey = tagDyn (value apiKey) submitEvent
--       loadingWidget = text "Waiting for an API Key..."
--   widgetHold loadingWidget $ fmap (\s -> bookshelf_for s) submitApiKey
--   rec
--     rsp :: Event t XhrResponse <- performRequestAsync req
--     let rspBookshelf :: Event t Bookshelf = fmapMaybe (\r -> decodeXhrResponse r) rsp
--     buttonEvent <- button "click me"
--     asyncEvent <- performRequestAsync (tag (constant defaultReq) buttonEvent)
--   return ()
header :: MonadWidget t m => String -> m ()
header = el "h1" . text

bookshelfWidget :: MonadWidget t m => m ()
bookshelfWidget = el "div" $ do
  postBuild <- getPostBuild

  let uri = "http://localhost:8081/books"
  let req = XhrRequest "GET" uri def
  rsp :: Event t XhrResponse <- performRequestAsync (tag (constant req) postBuild)
  let rspBookshelf :: Event t Bookshelf = fmapMaybe (\r -> decodeXhrResponse r) rsp
  let books' :: Event t [Book] = fmap books rspBookshelf
  let loadingWidget = text "fetching books"
  widgetHold loadingWidget $ fmap (\s -> mapM_ book_element s) books'
  pure ()

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  header "Your Bookshelf"
  bookshelfWidget

book_element b = do
  let textDynamic = title b ++ " by " ++ author b -- "<p>" ++ title b ++ "</p>"
  el "span" $ text textDynamic
