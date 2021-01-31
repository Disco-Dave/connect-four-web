module GameId exposing (GameId, fromString, toString)


type GameId
    = GameId String


fromString : String -> GameId
fromString =
    GameId


toString : GameId -> String
toString (GameId gameId) =
    gameId
