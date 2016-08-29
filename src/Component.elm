module Component exposing (..)

import Html.Attributes exposing (..)
import Html exposing (Html, Attribute, div, input, text)
import Html.Events exposing (onInput)

import Book exposing (..)

bookList books  = 
    div [] (List.map bookElement books)

bookElement b = Html.div [bookStyl] [text (b.title ++ " by " ++ b.author)]

searchBox model action =
  Html.input [
        class "u-full-width search-box"
      , type' "search"
      , placeholder "search.."
      , onInput action
      , value model.queryString
    ]
    []

bookStyl = style
    [ ("display", "inline-block")
    ]
