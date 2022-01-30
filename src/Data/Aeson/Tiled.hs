{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- | This module provides Haskell types for Tiled's JSON exports, which you can
-- read about at http://doc.mapeditor.org/en/latest/reference/json-map-format/.
-- That said - as of the writing of this module the JSON documentation does not
-- cover some of the types and records that are available in the format. For
-- those you should read the TMX documentation at
-- http://doc.mapeditor.org/en/latest/reference/tmx-map-format/
module Data.Aeson.Tiled
  ( -- * Tiled map editor types, their aeson instances and map loading
    module Data.Aeson.Tiled
    -- * Re-exports for working with Tiled types
  , module Data.Map
  , module Data.Vector
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (forM)
import           Data.Aeson                 hiding (Object)
import qualified Data.Aeson                 as A
import           Data.Aeson.Types           (Parser, typeMismatch)
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import           Data.Vector                (Vector)
import           GHC.Exts                   (fromList, toList)
import           GHC.Generics               (Generic)


-- | A globally indexed identifier.
newtype GlobalId = GlobalId { unGlobalId :: Int }
  deriving (Ord, Eq, Enum, Num, Generic, Show, FromJSON, ToJSON, FromJSONKey, ToJSONKey)


-- | A locally indexed identifier.
newtype LocalId = LocalId { unLocalId :: Int }
  deriving (Ord, Eq, Enum, Num, Generic, Show, FromJSON, ToJSON, FromJSONKey, ToJSONKey)


data XYPair a = XYPair a a

instance FromJSON a => FromJSON (XYPair a) where
  parseJSON (A.Object o) =
    XYPair <$> o .: "x"
           <*> o .: "y"
  parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON a => ToJSON (XYPair a) where
  toJSON (XYPair x y) =
    object [ "x" .= x
           , "y" .= y
           ]

fromXYPair :: XYPair a -> (a, a)
fromXYPair (XYPair x y) = (x, y)

toXYPair :: (a, a) -> XYPair a
toXYPair (x, y) = XYPair x y

omitNulls :: Value -> Value
omitNulls (A.Object hs) = A.Object
                        . fromList
                        . filter ((/= Null) . snd)
                        $ toList hs
omitNulls x = x

parseDefault :: FromJSON a => A.Object -> Text -> a -> Parser a
parseDefault o s d = fromMaybe d <$> o .:? s


data Object = Object { objectId         :: Int
                       -- ^ Incremental id - unique across all objects
                     , objectWidth      :: Double
                       -- ^ Width in pixels. Ignored if using a gid.
                     , objectHeight     :: Double
                       -- ^ Height in pixels. Ignored if using a gid.
                     , objectName       :: String
                       -- ^ String assigned to name field in editor
                     , objectType       :: String
                       -- ^ String assigned to type field in editor
                     , objectProperties :: Map Text Text
                       -- ^ String key-value pairs
                     , objectVisible    :: Bool
                       -- ^ Whether object is shown in editor.
                     , objectX          :: Double
                       -- ^ x coordinate in pixels
                     , objectY          :: Double
                       -- ^ y coordinate in pixels
                     , objectRotation   :: Float
                       -- ^ Angle in degrees clockwise
                     , objectGid        :: Maybe GlobalId
                       -- ^ GID, only if object comes from a Tilemap
                     , objectEllipse    :: Bool
                       -- ^ Used to mark an object as an ellipse
                     , objectPolygon    :: Maybe (Vector (Double, Double))
                       -- ^ A list of x,y coordinates in pixels
                     , objectPolyline   :: Maybe (Vector (Double, Double))
                       -- ^ A list of x,y coordinates in pixels
                     , objectText       :: Map Text Text
                       -- ^ String key-value pairs
                     } deriving (Eq, Generic, Show)

instance FromJSON Object where
  parseJSON (A.Object o) = Object <$> o .: "id"
                                  <*> o .: "width"
                                  <*> o .: "height"
                                  <*> o .: "name"
                                  <*> o .: "type"
                                  <*> parseDefault o "properties" M.empty
                                  <*> o .: "visible"
                                  <*> o .: "x"
                                  <*> o .: "y"
                                  <*> o .: "rotation"
                                  <*> o .:? "gid"
                                  <*> parseDefault o "ellipse" False
                                  <*> (fmap . fmap . fmap) fromXYPair (o .:? "polygon")
                                  <*> (fmap . fmap . fmap) fromXYPair (o .:? "polyline")
                                  <*> parseDefault o "text" M.empty
  parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Object where
  toJSON Object{..} = omitNulls $
    object [ "id"         .= objectId
           , "width"      .= objectWidth
           , "height"     .= objectHeight
           , "name"       .= objectName
           , "type"       .= objectType
           , "properties" .= objectProperties
           , "visible"    .= objectVisible
           , "x"          .= objectX
           , "y"          .= objectY
           , "rotation"   .= objectRotation
           , "gid"        .= objectGid
           , "ellipse"    .= objectEllipse
           , "polygon"    .= (fmap . fmap) toXYPair objectPolygon
           , "polyline"   .= (fmap . fmap) toXYPair objectPolyline
           , "text"       .= objectText
           ]


data Layer = Layer { layerWidth      :: Double
                     -- ^ Column count. Same as map width for fixed-size maps.
                   , layerHeight     :: Double
                     -- ^ Row count. Same as map height for fixed-size maps.
                   , layerName       :: String
                     -- ^ Name assigned to this layer
                   , layerType       :: String
                     -- ^ “tilelayer”, “objectgroup”, or “imagelayer”
                   , layerVisible    :: Bool
                     -- ^ Whether layer is shown or hidden in editor
                   , layerX          :: Double
                     -- ^ Horizontal layer offset in tiles. Always 0.
                   , layerY          :: Double
                     -- ^ Vertical layer offset in tiles. Always 0.
                   , layerData       :: Maybe (Vector GlobalId)
                     -- ^ Array of GIDs. tilelayer only.
                   , layerObjects    :: Maybe (Vector Object)
                     -- ^ Array of Objects. objectgroup only.
                   , layerProperties :: Map Text Text
                     -- ^ string key-value pairs.
                   , layerOpacity    :: Float
                     -- ^ Value between 0 and 1
                   , layerDraworder  :: String
                     -- ^ “topdown” (default) or “index”. objectgroup only.
                   } deriving (Eq, Generic, Show)

instance FromJSON Layer where
  parseJSON (A.Object o) = Layer <$> (o .: "width"      <|> pure 0)
                                 <*> (o .: "height"     <|> pure 0)
                                 <*>  o .: "name"
                                 <*>  o .: "type"
                                 <*>  o .: "visible"
                                 <*>  o .: "x"
                                 <*>  o .: "y"
                                 <*> (o .: "data"       <|> pure Nothing)
                                 <*> o .:? "objects"
                                 <*> (o .: "properties" <|> pure mempty)
                                 <*>  o .: "opacity"
                                 <*> (o .: "draworder"  <|> pure "topdown")
  parseJSON invalid = typeMismatch "Layer" invalid

instance ToJSON Layer where
  toJSON Layer{..} = omitNulls $
    object [ "width"      .= layerWidth
           , "height"     .= layerHeight
           , "name"       .= layerName
           , "type"       .= layerType
           , "visible"    .= layerVisible
           , "x"          .= layerX
           , "y"          .= layerY
           , "data"       .= layerData
           , "objects"    .= layerObjects
           , "properties" .= layerProperties
           , "opacity"    .= layerOpacity
           , "draworder"  .= layerDraworder
           ]


data Terrain = Terrain { terrainName :: String
                         -- ^ Name of terrain
                       , terrainTile :: LocalId
                         -- ^ Local ID of tile representing terrain
                       } deriving (Eq, Generic, Show)

instance FromJSON Terrain where
  parseJSON (A.Object o) = Terrain <$> o .: "name"
                                   <*> o .: "tile"
  parseJSON invalid = typeMismatch "Terrain" invalid

instance ToJSON Terrain where
  toJSON Terrain{..} = object [ "name" .= terrainName
                              , "tile" .= terrainTile
                              ]



data Frame = Frame { frameDuration :: Int
                   , frameTileId   :: LocalId
                   } deriving (Eq, Generic, Show)

instance FromJSON Frame where
  parseJSON (A.Object o) = Frame <$> o .: "duration"
                                 <*> o .: "tileid"
  parseJSON invalid = typeMismatch "Frame" invalid

instance ToJSON Frame where
  toJSON Frame{..} = object [ "duration" .= frameDuration
                            , "tileid"   .= frameTileId
                            ]


data Tile = Tile { tileId          :: LocalId
                 , tileProperties  :: Map Text Text
                 , tileImage       :: Maybe Value
                 , tileObjectGroup :: Maybe (Vector Object)
                 , tileAnimation   :: Maybe (Vector Frame)
                 } deriving (Eq, Generic, Show)

instance FromJSON Tile where
  parseJSON (A.Object o) = Tile 0 <$> (o .: "properties"  <|> pure mempty)
                                  <*> (o .: "image"       <|> pure Nothing)
                                  <*> (o .: "objectGroup" <|> pure mempty)
                                  <*> (o .: "animation"   <|> pure mempty)
  parseJSON invalid = typeMismatch "Tile" invalid

instance ToJSON Tile where
  toJSON Tile{..} = object [ "properties"   .= tileProperties
                           , "image"        .= tileImage
                           , "objectGroup"  .= tileObjectGroup
                           , "animation"    .= tileAnimation
                           ]


data Tileset = Tileset { tilesetFirstgid       :: GlobalId
                         -- ^ GID corresponding to the first tile in the set
                       , tilesetImage          :: String
                         -- ^ Image used for tiles in this set
                       , tilesetName           :: String
                         -- ^ Name given to this tileset
                       , tilesetTilewidth      :: Int
                         -- ^ Maximum width of tiles in this set
                       , tilesetTileheight     :: Int
                         -- ^ Maximum height of tiles in this set
                       , tilesetImagewidth     :: Int
                         -- ^ Width of source image in pixels
                       , tilesetImageheight    :: Int
                         -- ^ Height of source image in pixels
                       , tilesetProperties     :: Map Text Text
                         -- ^ String key-value pairs
                       , tilesetPropertytypes  :: Map Text Text
                         -- ^ String key-value pairs
                       , tilesetMargin         :: Int
                         -- ^ Buffer between image edge and first tile (pixels)
                       , tilesetSpacing        :: Int
                         -- ^ Spacing between adjacent tiles in image (pixels)
                       , tilesetTileproperties :: Map GlobalId (Map Text Text)
                         -- ^ Per-tile properties, indexed by gid as string
                       , tilesetTerrains       :: Vector Terrain
                         -- ^ Array of Terrains (optional)
                       , tilesetColumns        :: Int
                         -- ^ The number of tile columns in the tileset
                       , tilesetTilecount      :: Int
                         -- ^ The number of tiles in this tileset
                       , tilesetTiles          :: Map LocalId Tile
                         -- ^ Tiles (optional)
                       } deriving (Eq, Generic, Show)

newtype TransitiveTilesetMap = TransitiveTilesetMap (Map LocalId Value)
  deriving (Show, Eq, Generic, FromJSON)

parseTiles :: A.Object -> Parser (Map LocalId Tile)
parseTiles o = do
  TransitiveTilesetMap localId2Value <- o .: "tiles"
  localIdAndTiles <- forM (M.toList localId2Value) $ \(lid, val) -> do
    tile <- parseJSON val
    return (lid, tile{ tileId = lid })
  return $ M.fromList localIdAndTiles

instance FromJSON Tileset where
  parseJSON (A.Object o) = Tileset <$>  o .: "firstgid"
                                   <*>  o .: "image"
                                   <*>  o .: "name"
                                   <*>  o .: "tilewidth"
                                   <*>  o .: "tileheight"
                                   <*>  o .: "imagewidth"
                                   <*>  o .: "imageheight"
                                   <*> (o .: "properties"     <|> pure mempty)
                                   <*> (o .: "propertytypes"  <|> pure mempty)
                                   <*>  o .: "margin"
                                   <*>  o .: "spacing"
                                   <*> (o .: "tileproperties" <|> pure mempty)
                                   <*> (o .: "terrains"       <|> pure mempty)
                                   <*>  o .: "columns"
                                   <*>  o .: "tilecount"
                                   <*> (parseTiles o          <|> pure mempty)
  parseJSON invalid = typeMismatch "Tileset" invalid

instance ToJSON Tileset where
  toJSON Tileset{..} = object [ "firstgid"       .= tilesetFirstgid
                              , "image"          .= tilesetImage
                              , "name"           .= tilesetName
                              , "tilewidth"      .= tilesetTilewidth
                              , "tileheight"     .= tilesetTileheight
                              , "imagewidth"     .= tilesetImagewidth
                              , "imageheight"    .= tilesetImageheight
                              , "properties"     .= tilesetProperties
                              , "propertytypes"  .= tilesetPropertytypes
                              , "margin"         .= tilesetMargin
                              , "spacing"        .= tilesetSpacing
                              , "tileproperties" .= tilesetTileproperties
                              , "terrains"       .= tilesetTerrains
                              , "columns"        .= tilesetColumns
                              , "tilecount"      .= tilesetTilecount
                              , "tiles"          .= tilesetTiles
                              ]


-- | The full monty.
data Tiledmap = Tiledmap { tiledmapVersion         :: Float
                           -- ^ The JSON format version
                         , tiledmapTiledversion    :: String
                           -- ^ The Tiled version used to save the file
                         , tiledmapWidth           :: Int
                           -- ^ Number of tile columns
                         , tiledmapHeight          :: Int
                           -- ^ Number of tile rows
                         , tiledmapTilewidth       :: Double
                           -- ^ Map grid width.
                         , tiledmapTileheight      :: Double
                           -- ^ Map grid height.
                         , tiledmapOrientation     :: String
                           -- ^ Orthogonal, isometric, or staggered
                         , tiledmapLayers          :: Vector Layer
                           -- ^ Array of Layers
                         , tiledmapTilesets        :: Vector Tileset
                           -- ^ Array of Tilesets
                         , tiledmapBackgroundcolor :: Maybe String
                           -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB) (optional)
                         , tiledmapRenderorder     :: String
                           -- ^ Rendering direction (orthogonal maps only)
                         , tiledmapProperties      :: Map Text Text
                           -- ^ String key-value pairs
                         , tiledmapNextobjectid    :: Int
                           -- ^ Auto-increments for each placed object
                         } deriving (Eq, Generic, Show)

instance FromJSON Tiledmap where
  parseJSON (A.Object o) = Tiledmap <$>  o .: "version"
                                    <*>  o .: "tiledversion"
                                    <*>  o .: "width"
                                    <*>  o .: "height"
                                    <*>  o .: "tilewidth"
                                    <*>  o .: "tileheight"
                                    <*>  o .: "orientation"
                                    <*>  o .: "layers"
                                    <*>  o .: "tilesets"
                                    <*> (o .: "backgroundcolor" <|> pure Nothing)
                                    <*>  o .: "renderorder"
                                    <*> (o .: "properties"      <|> pure mempty)
                                    <*>  o .: "nextobjectid"
  parseJSON invalid = typeMismatch "Tiledmap" invalid

instance ToJSON Tiledmap where
  toJSON Tiledmap{..} = object [ "version"         .= tiledmapVersion
                               , "tiledversion"    .= tiledmapTiledversion
                               , "width"           .= tiledmapWidth
                               , "height"          .= tiledmapHeight
                               , "tilewidth"       .= tiledmapTilewidth
                               , "tileheight"      .= tiledmapTileheight
                               , "orientation"     .= tiledmapOrientation
                               , "layers"          .= tiledmapLayers
                               , "tilesets"        .= tiledmapTilesets
                               , "backgroundcolor" .= tiledmapBackgroundcolor
                               , "renderorder"     .= tiledmapRenderorder
                               , "properties"      .= tiledmapProperties
                               , "nextobjectid"    .= tiledmapNextobjectid
                               ]


-- | Load a Tiled map from the given 'FilePath'.
loadTiledmap :: FilePath -> IO (Either String Tiledmap)
loadTiledmap = fmap eitherDecode . C8.readFile
