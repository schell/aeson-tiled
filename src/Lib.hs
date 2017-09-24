module Lib
    ( someFunc
    ) where

import           Data.Aeson
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Text  (Text)
import qualified Data.Text  as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"
