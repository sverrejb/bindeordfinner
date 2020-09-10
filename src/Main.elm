module Main exposing (Model, Msg(..), QueryBody, SearchResult, apiUrl, doSearch, init, main, queryEncoder, resultDecoder, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, header, input, li, main_, p, span, text, ul)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Encode as Encode
import Url
import Html.Events exposing (onInput)
import Html exposing (h2)



-- MAIN


main =
    Browser.element { init = init, subscriptions = \_ -> Sub.none, update = update, view = view }



-- MODEL


type alias Model =
    { first : String
    , second : String
    , result : SearchResult
    }


type SearchResult
    = Failiure
    | Loading
    | Success SearchResultBody


type alias QueryBody =
    { first : String, second : String }


type alias SearchResultBody =
    { solutions : List String
    , startsWith : List String
    , endsWith : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model firstPlaceholder secondPlaceholder <| Success <| SearchResultBody resultsPlaceholder [] [], Cmd.none )



-- UPDATE


type Msg
    = Search QueryBody
    | GotSearchResult (Result Http.Error SearchResultBody)
    | UpdateFirst String
    | UpdateSecond String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search queryBody ->
            ( { model | result = Loading }, doSearch queryBody )

        GotSearchResult result ->
            case result of
                Ok searchResultBody ->
                    ( { model | result = Success searchResultBody }, Cmd.none )

                Err err ->
                    ( { model | result = Failiure }, Cmd.none )
        
        UpdateFirst newFirst ->
            ( { model | first = newFirst }, Cmd.none)
        
        UpdateSecond newSecond ->
            ( { model | second = newSecond}, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header [] [ h1 [] [ text "Bindeordfinner" ] ]
        , main_ []
            [ p [] [ text "Fyll inn start og sluttordet for å finne bindeordet i midten" ]
            , viewApp model
            ]
        ]


viewApp : Model -> Html Msg
viewApp model =
    main_ []
        [ input [ placeholder firstPlaceholder, onInput UpdateFirst ] []
        , input [ placeholder secondPlaceholder, onInput UpdateSecond ] []
        , button [ onClick <| Search (QueryBody model.first model.second) ] [ text "SØK" ]
        , viewResult model.result
        ]


viewResult : SearchResult -> Html Msg
viewResult result =
    case result of
        Loading ->
            viewLoading

        Success solutionResponse ->
            div [] [h2 [] [text "Mulige løsninger:"]
            ,ul [] (List.map (\l -> viewResultItem l) solutionResponse.solutions)]
            

        Failiure ->
            viewFailiure

viewResultItem : String -> Html Msg
viewResultItem resultItem =
    li [] [text resultItem]


viewLoading : Html Msg
viewLoading =
    text "Laster ..."


viewFailiure : Html Msg
viewFailiure =
    text "Noe gikk galt. Prøv igjen."



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


resultDecoder : Decoder SearchResultBody
resultDecoder =
    Decode.map3
        SearchResultBody
        (Decode.field "solutions" (list string))
        (Decode.field "startsWith" (list string))
        (Decode.field "endsWith" (list string))



-- CONSTS


apiUrl : String
apiUrl =
    "https://1bcdrx2x9c.execute-api.us-east-1.amazonaws.com/findGlueWords"

firstPlaceholder : String
firstPlaceholder = "øye"

secondPlaceholder : String
secondPlaceholder = "kart"

resultsPlaceholder: List String
resultsPlaceholder = ["eple", "farge"]