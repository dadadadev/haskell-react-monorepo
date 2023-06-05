module Main (main) where

import Server1 as S1

main :: IO ()
main = do
  S1.runServer
