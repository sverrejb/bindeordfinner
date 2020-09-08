module Main exposing (Model, Msg(..), QueryBody, SearchResult, apiUrl, doSearch, init, main, queryEncoder, resultDecoder, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, input, li, main_, p, span, text, ul)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Encode as Encode
import Url



-- MAIN


main =
    Browser.element { init = init, subscriptions = \_ -> Sub.none, update = update, view = view }


-- MODEL

type alias Model =
    { first : String
    , second : String
    , result : SearchResult
    }


type alias QueryBody =
    { first : String, second : String }


type alias SearchResult =
    { solutions : List String
    , startsWith : List String
    , endsWith : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "flaske" "horn" <| SearchResult [ "post", "for" ] [] [], Cmd.none )



-- UPDATE


type Msg
    = Search QueryBody
    | GotSearchResult (Result Http.Error SearchResult)
    | Pass


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search queryBody ->
            ( model, doSearch queryBody )

        Pass ->
            ( model, Cmd.none )

        GotSearchResult _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
   main_ [] [
            h1 [] [ text "Finn bindeordet" ]
            , p [] [ text "Fyll inn start og sluttordet for å finne bindeordet i midten" ]
            , input [ placeholder model.first ] []
            , ul [] (List.map (\l -> li [] [ text l ]) model.result.solutions)
            , input [ placeholder model.second ] []
            , button [ onClick <| Search (QueryBody "foo" "bar") ] [ text "SØK" ]
   ]
            



-- HTTP


doSearch : QueryBody -> Cmd Msg
doSearch queryBody =
    Http.post
        { url = apiUrl
        , expect = Http.expectJson GotSearchResult resultDecoder
        , body = Http.jsonBody (queryEncoder queryBody)
        }


queryEncoder : QueryBody -> Encode.Value
queryEncoder queryBody =
    Encode.object
        [ ( "first", Encode.string queryBody.first )
        , ( "second", Encode.string queryBody.second )
        ]


resultDecoder : Decoder SearchResult
resultDecoder =
    Decode.map3
        SearchResult
        (Decode.field "first" Decode.string |> Decode.list)
        (Decode.field "startsWith" Decode.string |> Decode.list)
        (Decode.field "endsWith" Decode.string |> Decode.list)



-- CONSTS


apiUrl : String
apiUrl =
    "https://1bcdrx2x9c.execute-api.us-east-1.amazonaws.com/default/findGlueWords"
