-- module Routing exposing
--     ( Route(..)
--     , baseUrl
--     , makeUrl
--     , idFromUrl
--     , routeParser
--     , router
--     )

-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Url
-- import Url.Parser exposing ((</>), Parser, oneOf, parse, string)


-- type Route
--     = Main
--     | Evaluation

-- baseUrl : String
-- baseUrl =
--     "http://localhost:3000/"

-- router : Url.Url -> Cmd Msg
-- router url =
--     case maybeRoute of
--         Nothing ->
--             Cmd.none

--         Just route ->
--             Cmd.none

-- idFromUrl : String -> String
-- idFromUrl url =
--     let
--         fUrl = String.filter (\x -> x /= '?') url
--         maybeId = String.split "/" fUrl |> List.reverse |> List.head
--     in

-- makeUrl : List String -> String
-- makeUrl lst =
--     baseUrl ++ String.join "/" lst

-- routeParser : Parser (Route -> a) a
-- routeParser =
--     oneOf
--         [ Url.Parser.map Main
--         , Url.Parser.map Evaluation
--         ]


--     case maybeId of
--         Nothing -> ""
--         Just id -> id
