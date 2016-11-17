{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib where

import Reflex
import Reflex.Dom
import Data.Monoid ((<>))
import qualified Data.Map as Map

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

update :: Action -> Model -> Model
update InitialLoad m = m
update (Query s) m = m { _filters = "q" =: s }
update (BooksResponse rsp) m = m { _books = Map.fromList $ zip [(1::Int)..] rsp }
update (SelectBook k) m = m { _selected = Just k }
update (SendToDevice _) m = m
update (FilterChange k v) m = m { _filters = Map.insert k v (_filters m) }
  -- shelfEvents <- mapDyn shelf_for $ _books model

data Model
  = Model
  { _books :: Map.Map Int Book
  , _selected :: Maybe Int
  , _filters :: Map.Map String String
  , _error :: Maybe String
  }

initialModel :: Model
initialModel
  = Model
  { _books = Map.empty
  , _filters = Map.empty
  , _selected = Nothing
  , _error = Nothing
  }

data Action
  = InitialLoad
  | Query String
  | SelectBook Int
  | FilterChange String String
  | BooksResponse [Book]
  | SendToDevice Book
  deriving (Eq, Read, Show)

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = elClass "div" "wrapper" $ do
  postBuild <- getPostBuild
  q <- Component.searchInput

  let query = _textInput_value q
  let bookRequestEvents = [ Query <$> updated query
                          , InitialLoad <$ postBuild
                          ]

  -- filter options
  selectedFormat <- Component.dropdownWith dropdownOptions

  -- remote events
  let requestEvent = leftmost bookRequestEvents
  rsp :: Event t XhrResponse <- performRequestAsync $ mkReq <$> requestEvent
  let booksResponse :: Event t [Book] = books <$> fmapMaybe decodeXhrResponse rsp

  bs <- mapDyn _books model
  selectedBook <- mapDyn _selected model
  bookSelected <- el "ul" $ Component.selectableList selectedBook bs $ \sel p -> do
    domEvent Click <$> bookEl sel p

  pure $ leftmost $
    [ SelectBook <$> bookSelected
    , BooksResponse <$> booksResponse
    , FilterChange "format" <$> updated selectedFormat
    , requestEvent
    ]

bookEl :: (MonadWidget t m)
       => Dynamic t Bool
       -> Dynamic t Book
       -> m(El t)
bookEl sel b = do
  attrs <- mapDyn (\s -> monoidGuard s $ Component.selectedStyle ) sel
  (element,_) <- elDynAttr' "li" attrs $ do
    dynText =<< mapDyn title b
    text " - "
    dynText =<< mapDyn author b
  pure element

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

dropdownOptions = ["epub", "mobi", "azw3", "pdf", "other"]

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec changes <- view model
      model <- foldDyn update initialModel changes
  pure ()

-- shelf_for :: [Book] -> Event t Integer
-- shelf_for books = do
--   event <- selectViewListWithKey_ (constDyn 1) (constDyn books') $ \k bookDyn _switched ->
--     case Map.lookup k (Map.fromList $ zip [1..] books) of
--       Just b -> do
--         pure $ updated bookDyn
--       Nothing -> error "what"
--   pure event

-- bookEl :: MonadWidget t m => Dynamic t (Map.Map String String) -> Book -> m ()
-- bookEl attrs b = elDynAttr' "div" attrs $ do
--   elClass "dt" "book-title" $ text $ title b
--   elClass "dd" "book-author" $ text $ author b

defaultUrl = "http://localhost:8081/books"

mkReq :: Action -> XhrRequest
mkReq InitialLoad= XhrRequest "GET" defaultUrl def
mkReq (Query q)= XhrRequest "GET" uri def
  where
    uri = defaultUrl ++ "?q=" ++ q

-- urlEncode :: Text -> Maybe [(Text, Maybe Text)] -> Text
-- urlEncode base qs = undefined
