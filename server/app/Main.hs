module Main where

import ConnectFour (getPort, newGameRepo, start)

main :: IO ()
main = do
  repo <- newGameRepo
  port <- getPort
  start repo port
