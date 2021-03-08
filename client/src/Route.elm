module Route exposing (Route(..), parse, push, replace)

import Browser.Navigation as Navigation
import GameId exposing (GameId)
import Url exposing (Url)
import Url.Parser as Parser


type Route
    = Home
    | Game GameId


parse : Url -> Maybe Route
parse =
    Parser.parse <|
        Parser.oneOf
            [ Parser.map Home Parser.top
            , Parser.map (Game << GameId.fromString) Parser.string
            ]


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Game gameId ->
            "/" ++ GameId.toString gameId


push : Navigation.Key -> Route -> Cmd msg
push navigationKey =
    Navigation.pushUrl navigationKey << toString


replace : Navigation.Key -> Route -> Cmd msg
replace navigationKey =
    Navigation.replaceUrl navigationKey << toString
