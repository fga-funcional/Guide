module Evaluation exposing (..)

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
import List exposing (append)

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
    { evaluations : List String
    , current : String
    , key : Nav.Key
    , url : Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model [] "" key url, Cmd.none )

--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Evaluate String
    | UpdateText String


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

        UpdateText v -> 
          ( { m | current = v }, Cmd.none)

        Evaluate v ->
            ({ m | evaluations = append [v] m.evaluations }, Cmd.none)


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
    { title = "Evaluation", body = [div [ class "page" ]
        [ h3 [] [ text "O que achou do tutorial? Comente aqui!"]
        , viewTextArea "Dê um feedback sobre o tutorial. É possível utilizar Markdown! :)" m.current UpdateText
        , sendButton m
        , viewEvaluations m
        , css
        ]]}

viewTextArea : String -> String -> (String -> msg) -> Html msg
viewTextArea p v toMsg =
  textarea [ placeholder p, value v, onInput toMsg ] []

sendButton m =
    p [class "button", onClick(Evaluate(m.current))] [ text "ENVIAR" ]

viewEvaluations : Model -> Html Msg
viewEvaluations m =
    let
      evaluations =
        List.indexedMap viewEvaluation m.evaluations
      viewEvaluation i eval =
        p [] [ text eval ]
    in
      div [ class "evaluation" ] [ htmlList (ol []) (li []) evaluations ]