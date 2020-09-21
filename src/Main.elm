module Main exposing (Model, Msg(..), QueryBody, SearchResult, apiUrl, doSearch, init, main, queryEncoder, resultDecoder, update, view)

import Browser exposing (document)
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, column, el, fill, height, layout, padding, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html, button)
import Http
import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Encode as Encode
import String.Interpolate exposing (interpolate)



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, subscriptions = \_ -> Sub.none, update = update, view = document }



-- MODEL


type alias Model =
    { first : String
    , second : String
    , showLists : Bool
    , result : SearchResult
    }


type WordPosition
    = First
    | Second


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
    ( Model "" "" False <| Success <| SearchResultBody firstPlaceholder secondPlaceholder resultsPlaceholder [] [], Cmd.none )



-- UPDATE


type Msg
    = Search QueryBody
    | GotSearchResult (Result Http.Error SearchResultBody)
    | UpdateFirst String
    | UpdateSecond String
    | ToggleLists


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

        ToggleLists ->
            ( { model | showLists = not model.showLists }, Cmd.none )



-- VIEW


document : Model -> Browser.Document Msg
document model =
    { title = "Bindeordfinner", body = view model }


view : Model -> List (Html Msg)
view model =
    [ layout
        [ Background.color bgcolor1 ]
      <|
        column [ padding 12, spacing 36, centerX, height fill, Background.color bgcolor2 ] [ viewHeader, viewSearch model, viewResultStates model.result model.showLists ]
    ]


viewHeader : Element Msg
viewHeader =
    el [ Region.heading 1, centerX, Font.size 36 ] <| text "Bindeordfinner"


viewSearch : Model -> Element Msg
viewSearch model =
    row [ spacing 36 ]
        [ Input.text []
            { onChange = UpdateFirst
            , text = model.first
            , placeholder = Just (Input.placeholder [] (text firstPlaceholder))
            , label = Input.labelAbove [] <| text "Første ord"
            }
        , Input.text []
            { onChange = UpdateSecond
            , text = model.second
            , placeholder = Just (Input.placeholder [] (text secondPlaceholder))
            , label = Input.labelAbove [] <| text "Andre ord"
            }
        , Input.button
            buttonAttrs
            { label = text "Søk"
            , onPress = Just <| Search <| QueryBody model.first model.second
            }
        ]


viewResultStates : SearchResult -> Bool -> Element Msg
viewResultStates result showList =
    case result of
        Loading ->
            text "Laster ..."

        Success response ->
            viewResult response showList

        Failiure ->
            text "Noe gikk galt"


viewResult : SearchResultBody -> Bool -> Element Msg
viewResult searchResult showLists =
    column [ paddingXY 0 12, spacing 24, width fill ]
        [ el [ Region.heading 2 ] <| text "Resultat:"
        , column [] (List.map (\l -> viewResultItem l searchResult.firstWord searchResult.secondWord) searchResult.solutions)
        , viewWordResults searchResult showLists
        ]


viewWordResults : SearchResultBody -> Bool -> Element Msg
viewWordResults searchResult showLists =
    if List.length searchResult.endsWith > 0 && List.length searchResult.startsWith > 0 then
        column [ spacing 32 ]
            [ Input.button
                buttonAttrs
                { label = text "Vis treff i ordbok", onPress = Just ToggleLists }
            , if showLists then
                row [ spacing 36 ]
                    [ el [ alignTop, alignRight ] <| viewWordlist searchResult.startsWith searchResult.firstWord First
                    , el [ alignTop, alignLeft ] <| viewWordlist searchResult.endsWith searchResult.secondWord Second
                    ]

              else
                Element.none
            ]

    else
        Element.none


viewWordlist : List String -> String -> WordPosition -> Element Msg
viewWordlist list word wordPosition =
    column [] <| listHeading wordPosition word :: List.map (\l -> text <| listWord wordPosition word l) list


viewResultItem : String -> String -> String -> Element Msg
viewResultItem resultItem firstWord secondWord =
    el [] <| text <| interpolate "{0} - som gir {1}{0} og {0}{2}" [ resultItem, firstWord, secondWord ]


listHeading : WordPosition -> String -> Element Msg
listHeading wordPosition word =
    case wordPosition of
        First ->
            el [] <| text ("Ord som begynner på \"" ++ word ++ "\":")

        Second ->
            el [] <| text ("Ord som slutter på \"" ++ word ++ "\":")


listWord : WordPosition -> String -> String -> String
listWord wordPosition word l =
    case wordPosition of
        First ->
            word ++ l

        Second ->
            l ++ word



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


bgcolor1 : Color
bgcolor1 =
    rgb255 156 173 164


bgcolor2 : Color
bgcolor2 =
    rgb255 251 226 229


buttoncolor : Color
buttoncolor =
    rgb255 250 190 167


buttonAttrs : List (Element.Attribute msg)
buttonAttrs =
    [ alignBottom
    , Background.color buttoncolor
    , Font.color white
    , paddingXY 32 14
    , Border.rounded 3
    ]


white : Color
white =
    rgb255 255 255 255
