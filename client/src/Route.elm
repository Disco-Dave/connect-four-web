module Route exposing (BasePath, Route(..), parse, push, replace, toString)

import Browser.Navigation as Navigation
import GameId exposing (GameId)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>))


type alias BasePath =
    String


type Route
    = Home
    | Game GameId


parse : BasePath -> Url -> Maybe Route
parse basePath =
    let
        routeParser =
            Parser.oneOf
                [ Parser.map Home Parser.top
                , Parser.map (Game << GameId.fromString) Parser.string
                ]
    in
    Parser.parse <|
        if String.isEmpty basePath then
            routeParser

        else
            Parser.s basePath </> routeParser


toString : BasePath -> Route -> String
toString basePath route =
    case route of
        Home ->
            if String.isEmpty basePath then
                "/"
            else
                Builder.absolute [ basePath ] []

        Game gameId ->
            if String.isEmpty basePath then
                GameId.toString gameId
            else
                Builder.absolute [ basePath, GameId.toString gameId ] []


push : Navigation.Key -> BasePath -> Route -> Cmd msg
push navigationKey basePath =
    Navigation.pushUrl navigationKey << toString basePath


replace : Navigation.Key -> BasePath -> Route -> Cmd msg
replace navigationKey basePath =
    Navigation.replaceUrl navigationKey << toString basePath
