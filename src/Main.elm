module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html exposing (main_)
import Html exposing (h1)
import Html exposing (input)
import Html.Attributes exposing (placeholder)
import Html exposing (span)
import Html exposing (p)
import Html exposing (li)
import Html exposing (ul)



-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL


type alias Model =
    {
        first : String,
        second : String,
        result : Result
    }

type alias Result =
    {
        solutions: List String,
        startsWith: List String,
        endsWith: List String
    }


init : Model
init =
    Model "flaske" "horn" <| Result ["post", "for"] [] []


-- UPDATE


type Msg
  = Search


update : Msg -> Model -> Model
update msg model =
  case msg of
    Search ->
        Model "foo" "bar" <| Result ["baz"] [] []




-- VIEW


view : Model -> Html Msg
view model =
    main_ [] [
        h1 [] [text "Finn bindeordet"],
        p [] [text "Fyll inn start og sluttordet for å finne bindeordet i midten"],
        input [placeholder model.first] [],
        ul [] (List.map (\l -> li [] [ text l ]) model.result.solutions),
        input [placeholder model.second] [],
        button [onClick Search] [text "SØK"]
    ]