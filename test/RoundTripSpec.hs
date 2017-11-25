{-# LANGUAGE LambdaCase #-}

module RoundTripSpec where

import Data.Aeson (encode, eitherDecode)
import Test.Hspec
import Control.Monad (forM_)

import Data.Aeson.Tiled

files :: [FilePath]
files = [ "maps/example.json"
        , "maps/test1.json"
        , "maps/test2.json"
        , "maps/test3.json"
        , "maps/test4.json"
        ]

spec :: Spec
spec = describe "Round tripping" . forM_ files $ \file ->
    it (file ++ " should roundtrip and end up with the same Tiledmap") $
      loadTiledmap file >>= \case
        Right tm -> do
          let bs = encode tm
          eitherDecode bs `shouldBe` Right tm
        Left x -> fail x
