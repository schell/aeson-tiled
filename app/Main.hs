module Main where

import Data.Aeson.Tiled

main :: IO ()
main = print =<< loadTiledmap "example.json"
