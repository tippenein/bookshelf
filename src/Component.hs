module Component where

import Book
import Reflex.Dom
import           Data.Map         (Map)
import qualified Data.Map as Map

searchInput :: MonadWidget t m => m (TextInput t)
searchInput = do
  textInput $
    def & attributes .~ constDyn
      (mconcat [ "class" =: "books-search"
               , "placeholder" =: "Search books"
               ]
      )

formatSimplePairs :: [String] -> [(String, String)]
formatSimplePairs ts = zip ts ts 

selectableList :: (MonadWidget t m, Ord k)
               => Dynamic t (Maybe k) -- Key of element that may be selected
               -> Dynamic t (Map k v) -- Map of elements to be shown in the list
               -> (Dynamic t Bool -> Dynamic t v -> m (Event t a))
               -- ^ Action that renders a widget for an element. The element may fire events
               -> m (Event t k) -- List fires events whenever an element is selected
selectableList selection elems mkEntry = do
  selectEntry <- listWithKey elems $ \k v -> do
      isSelected <- forDyn selection $ \s -> s == Just k
      fmap (const k) <$> mkEntry isSelected v
  switchPromptlyDyn <$> mapDyn (leftmost . Map.elems) selectEntry

selectedStyle = "style" =: "font-weight: bold"

dropdownWith pairs = do
  let ops = Map.fromList $ formatSimplePairs pairs
  formatDropdown <- dropdown "mobi" (constDyn ops) def
  let selectedFormat = _dropdown_value formatDropdown
  pure selectedFormat
metadata_for :: MonadWidget t m => Book -> m ()
metadata_for book = do
  elClass "span" "book-meta" $ do
    el "p" $ text "format"
    elClass "p" "book-format" $ text (format book)

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
