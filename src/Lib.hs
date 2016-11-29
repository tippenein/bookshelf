{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Lib where

import Reflex
import Reflex.Dom
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T

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

data Model
  = Model
  { _books :: Map.Map Int Book
  , _filters :: Map.Map Text Text
  , _selected :: Maybe Int
  , _error :: Maybe Text
  } deriving Show

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
  | Query Text
  | BooksResponse [Book]
  | SelectBook Int
  | FilterChange Text Text
  | SendToDevice Book
  deriving (Eq, Read, Show)

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model = elClass "div" "twelve columns" $ do
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

  let bookMap = fmap _books model
  let selectedBook = fmap _selected model
  bookSelected <- divClass "row" $ do
    bs' <- shelfContainer $ do
      bs <- el "ul" $ Component.selectableList selectedBook bookMap $ \sel p -> do
        domEvent Click <$> bookEl sel p
      pure bs

    Component.metadataContainer $ do
      display $ zipDynWith maybeLookup selectedBook bookMap
    pure bs'

  pure $ leftmost $
    [ SelectBook <$> bookSelected
    , BooksResponse <$> booksResponse
    , FilterChange "format" <$> updated selectedFormat
    , requestEvent
    ]

shelfContainer :: MonadWidget t m => m a -> m a
shelfContainer body = elClass "div" "wrapper six columns" $ body

metadataContainer :: MonadWidget t m => m a -> m a
metadataContainer body = elClass "div" "six columns" $ body

maybeLookup :: Maybe Int -> Map.Map Int a -> Maybe a
maybeLookup midx m = case midx of
  Nothing -> Nothing
  Just idx -> Map.lookup idx m

nothing :: MonadWidget t m => m ()
nothing = el "p" $ text ""

maybeBookWidget :: MonadWidget t m => Maybe Book -> m ()
maybeBookWidget mb = case mb of
  Nothing -> nothing
  Just b -> el "p" $ text $ title b

bookEl :: (MonadWidget t m)
       => Dynamic t Bool
       -> Dynamic t Book
       -> m(El t)
bookEl sel b = do
  let commonAttrs = constDyn $ "class" =: "book-binding"
  let attrs = fmap (\s -> monoidGuard s $ Component.selectedStyle ) sel
  (e,_) <- elDynAttr' "li" (attrs <> commonAttrs) $ do
    dynText $ fmap title b
    text " - "
    dynText $ fmap author b
  pure e

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

dropdownOptions :: [Text]
dropdownOptions = ["epub", "mobi", "azw3", "pdf", "other"]

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec changes <- view model
      model <- foldDyn update initialModel changes
  pure ()

defaultUrl :: Text
defaultUrl = "http://localhost:8081/books"

mkReq :: Action -> XhrRequest ()
mkReq InitialLoad = XhrRequest "GET" defaultUrl def
mkReq (Query q)= XhrRequest "GET" uri def
  where
    uri = defaultUrl <> "?q=" <> q

-- urlEncode :: Text -> Maybe [(Text, Maybe Text)] -> Text
-- urlEncode base qs = undefined
