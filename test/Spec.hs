{-# LANGUAGE LambdaCase #-}
import Data.Aeson (encode, eitherDecode)
import Test.Hspec
import Control.Monad (forM_)

import Data.Aeson.Tiled

main :: IO ()
main = hspec $ forM_ ["example.json"] $ \file ->
  describe ("With " ++ show file) $ do
    it "loading should produce a 'Right Tiledmap{..}'" $
      (loadTiledmap file >>=) . flip shouldSatisfy $ \case
        (Right _) -> True
        _         -> False
    it "loading and encoding and decoding should end up with the same Tiledmap" $
      loadTiledmap "example.json" >>= \case
        Right tm -> do
          let bs = encode tm
          eitherDecode bs `shouldBe` Right tm
        _ -> fail $ "Could not decode Tiledmap from " ++ show file
