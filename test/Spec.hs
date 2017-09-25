{-# LANGUAGE LambdaCase #-}
import Data.Aeson.Tiled
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Loading example.json" $
    it "should produce a 'Right Tiledmap{..}'" $
      (loadTiledmap "example.json" >>=) . flip shouldSatisfy $ \case
        (Right _) -> True
        _         -> False
