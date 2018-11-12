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
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] 0,  Http.send GotAPI getGuides)


{-| Create simple guide element
-}
mkPage : Int -> String -> String -> Page
mkPage id title data =
    Page id title data


{-| Empty mkPage used as a fallback
-}
emptyPage : Page
emptyPage =
    mkPage 0 "Empty" "This mkPage is empty."



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            (m, Cmd.none)
        GetAPI ->
            ( m, Http.send GotAPI getGuides )
        GotAPI result ->
            case result of
                Err httpError ->
                    let
                        _ =
                            Debug.log "Request error" httpError
                    in
                        ( m, Cmd.none )

                Ok pages ->
                    ( { m | pages = pages }, Cmd.none )
        Next ->
            if m.index < List.length m.pages - 1 then
                ({ m | index = m.index + 1 }, Cmd.none )

            else
                (m, Cmd.none)

        Prev ->
            if m.index > 0 then
                ({ m | index = m.index - 1 }, Cmd.none )

            else
                (m, Cmd.none)

        ChangeTo i ->
            ({ m | index = i }, Cmd.none)


  
--------------------------------------------------------------------------------
-- SUBSCRIPTIONS
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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

----------------------------
-- HTTP
----------------------------

getGuides : Http.Request (List Page)
getGuides =
  Http.get "http://localhost:3000/guides" (guideDecoder)


guideDecoder : D.Decoder (List Page)
guideDecoder =
    D.list (D.map3 mkPage
        (D.at ["id"] D.int)
        (D.at ["title"] D.string)
        (D.at ["content"] D.string)
    )
