module Main exposing (Model, Msg(..), QueryBody, SearchResult, apiUrl, doSearch, init, main, queryEncoder, resultDecoder, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Encode as Encode
import String.Interpolate exposing (interpolate)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = \_ -> Sub.none, update = update, view = view }



-- MODEL


type alias Model =
    { first : String
    , second : String
    , result : SearchResult
    }

type WordType = First | Second

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
    layout
        [ Background.color <| rgb255 70 83 98
        ]
    <|
        column [ padding 12, centerX, height fill, Background.color <| rgb255 194 234 189 ] [ viewHeader, viewSearch model, viewResultStates model.result ]


viewHeader : Element Msg
viewHeader =
    el [Region.heading 1, centerX] <| text "Bindeordfinner"


viewSearch : Model -> Element Msg
viewSearch model =
    row [ spacing 36]
        [ Input.text []
            { onChange = UpdateFirst
            , text = model.first
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Første ord"
            }
            ,
            Input.text []
            { onChange = UpdateSecond
            , text = model.second
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Andre ord"
            }
            ,
            Input.button []
            {label = text "Søk"
            , onPress = Just <| Search <| QueryBody model.first model.second}
        ]

viewResultStates : SearchResult -> Element Msg
viewResultStates result =
    case result of
        Loading -> text "Laster"
        Success response -> viewResult response
        Failiure -> text "Noe gikk galt"

viewResult : SearchResultBody -> Element Msg
viewResult searchResult = 
    column [ spacing 36] [
        el [] <| text "Resultat"
        ,column []  (List.map (\l -> viewResultItem l searchResult.firstWord searchResult.secondWord) searchResult.solutions)
        ,row [ spaceEvenly ] [ el [alignTop, alignRight] <| viewWordlist searchResult.startsWith searchResult.firstWord First,
                    el [alignTop, alignLeft] <| viewWordlist searchResult.endsWith searchResult.secondWord Second]
    ]

viewWordlist : List String -> String -> WordType -> Element Msg
viewWordlist list word wordtype =
    column [] <| List.map (\l -> text l ) list

viewResultItem : String -> String -> String -> Element Msg
viewResultItem resultItem firstWord secondWord =
    el [] <| text <| interpolate "{0} - som gir {1}{0} og {0}{2}" [ resultItem, firstWord, secondWord ]


{-
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
                       [ h2 [] [ text "Ingen nøyaktige treff." ]
                       , p [] [ text "Viser ord som slutter og starter på søkeordene. Kanskje du finner svaret likevel?" ]
                       , viewFirstList solutionResponse.startsWith solutionResponse.firstWord
                       , viewSecondList solutionResponse.endsWith solutionResponse.secondWord
                       ]

           Failiure ->
               viewFailiure


   viewResultItem : String -> String -> String -> Html Msg
   viewResultItem resultItem firstWord secondWord =
       li [] [ text <| interpolate "{0} - som gir {1}{0} og {0}{2}" [ resultItem, firstWord, secondWord ] ]


   viewLoading : Html Msg
   viewLoading =
       p [] [ text "Laster ..." ]


   viewFirstList : List String -> String -> Html Msg
   viewFirstList list firstWord =
       div []
           [ h3 [] [ text "Ord som starter med \"", text firstWord, text "\"" ]
           , ul [] (List.map (\l -> li [] [ text firstWord, text l ]) list)
           ]


   viewSecondList : List String -> String -> Html Msg
   viewSecondList list secondWord =
       div []
           [ h3 [] [ text "Ord som slutter med \"", text secondWord, text "\"" ]
           , ul [] (List.map (\l -> li [] [ text l, text secondWord ]) list)
           ]


   viewFailiure : Html Msg
   viewFailiure =
       p [] [ text "Noe gikk galt. Prøv igjen." ]


-}
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
