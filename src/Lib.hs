{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (
  bookshelf
) where

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Book

bookshelf :: MonadWidget t m => m ()
bookshelf = do
  rec
    let onButtonClass  = Map.singleton "class" "onButton"
    let offButtonClass = Map.singleton "class" "offButton"
    (buttonElement, _) <- elDynAttr' "button" cssClassDynamic $ elDynHtml' "span" textDynamic
    buttonElementClickEvent <- return $ domEvent Click buttonElement
    toggleDynamic <- toggle False buttonElementClickEvent
    cssClassDynamic <- mapDyn (
        \ x -> if x then (
          onButtonClass :: Map.Map String String
        ) else (
          offButtonClass :: Map.Map String String
        )
      ) toggleDynamic
    textDynamic <- mapDyn (
        \ x -> if x then (
          "<i class='fa fa-circle-o-notch' aria-hidden='true'></i>" :: String
        ) else (
          "<i class='fa fa-power-off' aria-hidden='true'></i>" :: String
      )) toggleDynamic
  return ()
