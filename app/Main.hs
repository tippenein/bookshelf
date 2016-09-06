{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Reflex.Dom (mainWidgetWithHead)

main :: IO ()
main = mainWidgetWithHead headElement bodyElement
