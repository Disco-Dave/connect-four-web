module GameId exposing
    ( GameId
    , decoder
    , encode
    , fromString
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type GameId
    = GameId String


fromString : String -> GameId
fromString =
    GameId


toString : GameId -> String
toString (GameId gameId) =
    gameId


decoder : Decode.Decoder GameId
decoder =
    Decode.map fromString Decode.string


encode : GameId -> Encode.Value
encode =
    toString >> Encode.string
