module Main where

import Server (startServer)

main :: IO ()
main = do
  putStrLn "Starting Haskell backend server on port 3000..."
  startServer

