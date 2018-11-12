module Guide exposing (Model, Msg, init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Utils exposing (..)


main =
    Browser.sandbox
        { init = example
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
    , title : String
    }


type alias Page =
    { title : String
    , content : String
    }


init : Model
init =
    { pages = [], index = 0, title = "Tutorial" }


{-| Create simple flag element
-}
mkPage : String -> String -> Page
mkPage title data =
    Page title data


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



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | pages =
            [ mkPage "Introduction" "Elm is cool..."
            , mkPage "Preparing your environment" "You'll need to install..."
            , mkPage "Conclusion" "It was great, heh?"
            ]
        , title = "Introduction to ELM"
    }