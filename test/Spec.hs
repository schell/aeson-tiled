{-# LANGUAGE LambdaCase #-}
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

main :: IO ()
main = hspec $ forM_ files $ \file ->
  describe ("With " ++ show file) $
    it "loading and encoding and decoding should end up with the same Tiledmap" $
      loadTiledmap file >>= \case
        Right tm -> do
          let bs = encode tm
          eitherDecode bs `shouldBe` Right tm
        _ -> fail $ "Could not decode Tiledmap from " ++ show file
