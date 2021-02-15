port module Ports exposing
    ( close
    , connect
    , receive
    , send
    )

import Json.Decode exposing (Value)


port connect : Value -> Cmd msg


port send : Value -> Cmd msg


port receive : (Value -> msg) -> Sub msg


port close : () -> Cmd msg
