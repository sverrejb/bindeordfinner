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
import Debug
import Html exposing (header)



-- MAIN


main =
    Browser.element { init = init, subscriptions = \_ -> Sub.none, update = update, view = view }


-- MODEL

type Model =
    Failiure
    | Initial
    | Loading
    | Success SearchResult

type alias QueryBody =
    { first : String, second : String }


type alias SearchResult =
    { solutions : List String
    , startsWith : List String
    , endsWith : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initial, Cmd.none)



-- UPDATE


type Msg
    = Search QueryBody
    | GotSearchResult (Result Http.Error SearchResult)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search queryBody ->
            ( Loading, doSearch queryBody )

        GotSearchResult result ->
            case result of
            Ok resultBody ->
                (Success resultBody, Cmd.none)
            Err err ->
                (Debug.log <| Debug.toString err)
                (Failiure, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ header [] [ h1 [] [ text "Bindeordfinner"]],
    main_ [] [
        p [] [ text "Fyll inn start og sluttordet for å finne bindeordet i midten" ],
    viewApp model]]
   

viewApp : Model -> Html Msg
viewApp model = 
    case model of
    Failiure -> span [] [text "noe gikk galt"]
    Initial -> viewForm model
    Loading -> viewLoading
    Success result -> viewResult result

viewForm : Model -> Html Msg
viewForm model = main_ [] [input [ placeholder "smør" ] []
            , ul [] (List.map (\l -> li [] [ text l ]) ["klatt"])
            , input [ placeholder "maler" ] []
            , button [ onClick <| Search (QueryBody "smør" "maler") ] [ text "SØK" ]]

viewResult : SearchResult -> Html Msg
viewResult result =
    text "hurra"

viewLoading : Html Msg
viewLoading = text "Laster ..."

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
        (Decode.field "solutions" (list string))
        (Decode.field "startsWith" (list string))
        (Decode.field "endsWith" (list string))



-- CONSTS


apiUrl : String
apiUrl =
    "https://1bcdrx2x9c.execute-api.us-east-1.amazonaws.com/findGlueWords"
