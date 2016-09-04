{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Reflex
import Reflex.Dom
import qualified Data.Map as Map

main :: IO ()
main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  el "title" (text "Bookshelf")
  styleSheet "css/style.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  where
    styleSheet link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ bookshelf_for books
