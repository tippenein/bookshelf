import Http as Http
import Json.Decode as Json
import Task exposing (..)
import Html.App exposing (..)
import Html exposing (Html, div)
import Book exposing (..)
import Component exposing (..)

main : Program Never
main = Html.App.program
  { init = model ! [fetchBooks]
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }

type alias Model =
  { books : List Book
  , queryString : String
  }

book = Book "author" "title" "format" "location"

fetchBooks = Cmd.none
searchBooks s = Cmd.none

model : Model
model =
  { books = [book]
  , queryString = ""
  }

-- UPDATE

type Action
  = NoOp
  | FetchBooks
  | SearchBooks String
  | BooksFetched (List Book)

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    NoOp ->
      model ! []
    FetchBooks ->
      model ! [fetchBooks]
    SearchBooks s ->
      { model | queryString = s } ! [searchBooks s]
    BooksFetched documents ->
      { model | books = documents } ! []

-- VIEW

view : Model -> Html Action
view model =
  div []
    [
      searchBox model SearchBooks
    , bookList model.books
    ]
