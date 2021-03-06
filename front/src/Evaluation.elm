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

mkEvaluation : String -> String
mkEvaluation value =
    value

type alias Model =
    { evaluations : List String
    , current : String
    , key : Nav.Key
    , url : Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model [] "" key url, Http.send GotAPI getEvaluations )

--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GetAPI 
    | GotAPI (Result Http.Error (List String))
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

        GetAPI ->
          ( m, Http.send GotAPI getEvaluations )

        GotAPI result ->
            case result of
                Err httpError ->
                    let
                        _ =
                            Debug.log "Request error" httpError
                    in
                        ( m, Cmd.none )

                Ok evaluations ->
                    ( { m | evaluations = evaluations }, Cmd.none )

        UpdateText v -> 
          ( { m | current = v }, Cmd.none)

        Evaluate v ->
            ({ m | evaluations = append [v] m.evaluations, current = "" }, Cmd.none)


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
        , viewTextArea "Dê um feedback sobre o tutorial. É possível comentar em Markdown :)" m.current UpdateText
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
        Markdown.toHtml [ class "markdown-body" ] eval
    in
      div [ class "evaluations-list" ] [ htmlList (ul []) (div [ class "evaluation" ]) evaluations ]

----------------------------
-- HTTP
----------------------------

getEvaluations : Http.Request (List String)
getEvaluations =
  Http.get "http://localhost:3000/evaluations" (evaluationDecoder)


{-| Create simple guide element
-}
evaluationDecoder : D.Decoder (List String)
evaluationDecoder =
    D.list (D.map mkEvaluation (D.string))