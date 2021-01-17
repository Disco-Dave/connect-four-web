module Main where

import qualified ConnectFour.GameRepo as GameRepo
import ConnectFour.WebSocket (serverApp)
import qualified Network.WebSockets as WebSockets

main :: IO ()
main = do
  repo <- GameRepo.newGameRepo
  WebSockets.runServer "localhost" 5123 (serverApp repo)
