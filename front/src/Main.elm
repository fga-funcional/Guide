module Guide exposing (..)

import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Utils exposing (..)
import Http exposing (..)
import Markdown exposing (..)
import Url exposing (..)
import Json.Decode as D
import Json.Encode as E
import String exposing (fromInt)

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
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
    , key : Nav.Key
    , url : Url
    }


type alias Page =
    { id : Int
    , title : String
    , content : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model [] 0 key url, Http.send GotAPI getGuides )


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
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            (m, Cmd.none)
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( m, Nav.pushUrl m.key (Url.toString url) )

                Browser.External href ->
                    ( m, Nav.load href )
        UrlChanged url ->
            ( { m | url = url }
            , Cmd.none
            )
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
            ({ m | index = i - 1 }, Cmd.none)


--------------------------------------------------------------------------------
-- SUBSCRIPTIONS
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------


view : Model -> Document Msg
view m =
    { title = "Home", body = [div [ class "main" ]
        [ viewDrawer m
        , viewPage m
        , css
        ]]}


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
            a [ style "font-weight" weight, class "title", href (fromInt page.id), onClick(ChangeTo(page.id)) ] [ text page.title ]
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
            , Markdown.toHtml [ class "markdown-body" ] page.content
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
