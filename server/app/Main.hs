module Main (main) where

import Server

main :: IO ()
main = do
  Server.run
