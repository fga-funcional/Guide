module Guide exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Utils exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E


main =
    Browser.sandbox
        { init = guide
        , view = view
        , update = update
        }

css : Html msg
css =
    Html.node "link" [ rel "stylesheet", href "./Ui/style.css" ] []


--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------


type alias Model =
    { pages : List Page
    , index : Int
    }


type alias Page =
    { id : Int
    , title : String
    , content : String
    }


init : Model
init =
    { pages = [], index = 0 }


{-| Create simple guide element
-}
mkPage : Int -> String -> String -> Page
mkPage id title data =
    Page id title data


{-| Empty mkPage used as a fallback
-}
emptyPage : Page
emptyPage =
    mkPage "Empty" "This mkPage is empty."



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Next
    | Prev
    | ChangeTo Int
    | GetAPI 
    | GotAPI (Result Http.Error (List Page))


update : Msg -> Model -> Model
update msg m =
    case msg of
        Next ->
            if m.index < List.length m.pages - 1 then
                { m | index = m.index + 1 }

            else
                m

        Prev ->
            if m.index > 0 then
                { m | index = m.index - 1 }

            else
                m

        ChangeTo i ->
            { m | index = i }

        _ ->
            m



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


view : Model -> Html Msg
view m =
    div [ class "main" ]
        [ viewDrawer m
        , viewPage m
        , css
        ]


viewDrawer : Model -> Html Msg
viewDrawer m =
    let
        titles =
            List.indexedMap viewTitle m.pages

        viewTitle i page =
            let
                weight =
                    if i == m.index then
                        "700"

                    else
                        "500"
            in
            a [ style "font-weight" weight, onClick (ChangeTo i)] [ text page.title ]
    in
    div [ class "drawer" ] [ htmlList (ol []) (li []) titles ]


viewPage : Model -> Html Msg
viewPage m =
    let
        page =
            getAt m.index m.pages |> withDefault emptyPage
    in
    div [ class "page" ]
        [ div []
            [ h1 [] [ text page.title ]
            , p [] [ text page.content ]
            , div [ class "buttonsWrapper" ] 
            [ button [ onClick Prev ] [ text "back" ]
            , button [ onClick Next ] [ text "next" ]
            ]
            ]
        ]

-- ----------------------------
-- -- Decoders
-- ----------------------------

guideDecoder : D.Decoder (List Page)
guideDecoder =
    D.list (D.map3 mkPage
        (D.at ["id"] D.int)
        (D.at ["title"] D.string)
        (D.at ["content"] D.string)
    )


getGuides : Http.Request (List Page)
getGuides =
  Http.get "http://localhost:3000/guides" (guideDecoder)

----------------------------
-- Example
----------------------------

guide : Model
guide =
    { init
        | pages =
            [ mkPage 1 "Introduction" "Elm is cool..."
            , mkPage 2 "Preparing your environment" "You'll need to install..."
            , mkPage 3 "Conclusion" "It was great, heh?"
            ]
    }