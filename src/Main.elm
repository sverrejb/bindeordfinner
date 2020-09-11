module Main exposing (Model, Msg(..), QueryBody, SearchResult, apiUrl, doSearch, init, main, queryEncoder, resultDecoder, update, view)

import Browser
import Html exposing (Html, button, div, h1, h2, header, input, li, main_, p, text, ul)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Encode as Encode
import String.Interpolate exposing (interpolate)
import Html exposing (source)
import Html exposing (h3)



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
    { first : String
    , second : String
    }


type alias SearchResultBody =
    { firstWord : String
    , secondWord : String
    , solutions : List String
    , startsWith : List String
    , endsWith : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model firstPlaceholder secondPlaceholder <| Success <| SearchResultBody firstPlaceholder secondPlaceholder resultsPlaceholder [] [], Cmd.none )



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

                Err _ ->
                    ( { model | result = Failiure }, Cmd.none )

        UpdateFirst newFirst ->
            ( { model | first = newFirst }, Cmd.none )

        UpdateSecond newSecond ->
            ( { model | second = newSecond }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header [] [ h1 [] [ text "Bindeordfinner" ] ]
        , main_ []
            [ p [] [ text "Fyll inn start- og sluttordet for å finne bindeordet i midten" ]
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
            if List.length solutionResponse.solutions > 0 then
                div []
                    [ h2 [] [ text "Mulige løsninger:" ]
                    , ul [] (List.map (\l -> viewResultItem l solutionResponse.firstWord solutionResponse.secondWord) solutionResponse.solutions)
                    ]

            else
                div []
                    [ h2 [] [ text "Ingen nøyaktige treff" ]
                    , p [] [ text "Viser ord som slutter og starter på søkeordene." ]
                    , viewFirstList solutionResponse.startsWith
                    , viewSecondList solutionResponse.endsWith
                    ]

        Failiure ->
            viewFailiure


viewResultItem : String -> String -> String -> Html Msg
viewResultItem resultItem firstWord secondWord =
    li [] [ text <| interpolate "{0} - som gir {1}{0} og {0}{2}" [ resultItem, firstWord, secondWord ] ]


viewLoading : Html Msg
viewLoading =
    p [] [ text "Laster ..." ]

viewFirstList : List String -> Html Msg
viewFirstList list =
    div [] [
        h3 [] [text "Ord som slutter med"]
        ,ul [] (List.map (\l -> li [] [text l]) list)
    ]

viewSecondList : List String -> Html Msg
viewSecondList list =
    div [] [
        h3 [] [text "Ord som starter med"]
        ,ul [] (List.map (\l -> li [] [text l]) list)
    ]


viewFailiure : Html Msg
viewFailiure =
    p [] [ text "Noe gikk galt. Prøv igjen." ]



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
    Decode.map5
        SearchResultBody
        (Decode.field "firstWord" string)
        (Decode.field "secondWord" string)
        (Decode.field "solutions" (list string))
        (Decode.field "startsWith" (list string))
        (Decode.field "endsWith" (list string))



-- CONSTS


apiUrl : String
apiUrl =
    "https://1bcdrx2x9c.execute-api.us-east-1.amazonaws.com/findGlueWords"


firstPlaceholder : String
firstPlaceholder =
    "øye"


secondPlaceholder : String
secondPlaceholder =
    "kart"


resultsPlaceholder : List String
resultsPlaceholder =
    [ "eple", "farge" ]
