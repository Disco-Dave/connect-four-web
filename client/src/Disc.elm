module Disc exposing
    ( Disc(..)
    , decoder
    , encode
    , fromString
    , opposite
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type Disc
    = YellowDisc
    | RedDisc


opposite : Disc -> Disc
opposite disc =
    case disc of
        YellowDisc ->
            RedDisc

        RedDisc ->
            YellowDisc


toString : Disc -> String
toString disc =
    case disc of
        YellowDisc ->
            "yellow"

        RedDisc ->
            "red"


fromString : String -> Maybe Disc
fromString disc =
    case String.toLower (String.trim disc) of
        "yellow" ->
            Just YellowDisc

        "red" ->
            Just RedDisc

        _ ->
            Nothing


decoder : Decode.Decoder Disc
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case fromString s of
                    Just disc ->
                        Decode.succeed disc

                    Nothing ->
                        Decode.fail "Disc may only be \"yellow\" or \"red\""
            )


encode : Disc -> Encode.Value
encode =
    toString >> Encode.string
