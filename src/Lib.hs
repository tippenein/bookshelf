{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Data.Monoid
import Data.Text
import Book

books = [Book "whatever" "stuff" "more" "yup"]
bookElement :: MonadWidget t m => Book -> m ()
bookElement b = do
  -- textDynamic <- title b -- "<p>" ++ title b ++ "</p>"
  el "span" $ text $ title b -- textDynamic
  -- rec
  --   let bookClass = Map.singleton "class" "book-binding"
  --   (buttonElement, _) <- elAttr "button" bookClass $ el "span" $ text textDynamic
  --   buttonElementClickEvent <- return $ domEvent Click buttonElement
  --   toggleDynamic <- toggle False buttonElementClickEvent
  --   textDynamic <- "<p>" ++ title b ++ "</p>"
  -- return ()

bookshelf_for :: MonadWidget t m => [Book] -> m ()
bookshelf_for bs = mapM_ bookElement bs
