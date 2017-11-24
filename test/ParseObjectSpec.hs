{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module ParseObjectSpec where

import Data.Aeson (encode, eitherDecode)
import Test.Hspec
import Data.Either (isRight)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as C8

import Data.Aeson.Tiled

file :: FilePath
file = "maps/objects/obj1.json"

spec :: Spec
spec = describe "Obj1" $ do
  it "should parse just fine" $ do
    eobj <- fmap (eitherDecode @Object) $ C8.readFile file
    eobj `shouldSatisfy` isRight

