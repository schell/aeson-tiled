{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.Tiled.Types where

import           Data.Aeson       hiding (Object)
import qualified Data.Aeson       as A
import           Data.Aeson.Types (typeMismatch)
import           Data.Map         (Map)
import           Data.Text        (Text)
import           Data.Vector      (Vector)
import           GHC.Generics     (Generic)


newtype GlobalId = GlobalId { unGlobalId :: Int }
                 deriving (Generic, Show)


newtype LocalId = LocalId { unLocalId :: Int }
                deriving (Generic, Show)


data Object = Object { objectId         :: Int
                       -- ^ Incremental id - unique across all objects
                     , objectWidth      :: Int
                       -- ^ Width in pixels. Ignored if using a gid.
                     , objectHeight     :: Int
                       -- ^ Height in pixels. Ignored if using a gid.
                     , objectName       :: String
                       -- ^ String assigned to name field in editor
                     , objectType       :: String
                       -- ^ String assigned to type field in editor
                     , objectProperties :: Map Text Text
                       -- ^ String key-value pairs
                     , objectVisible    :: Bool
                       -- ^ Whether object is shown in editor.
                     , objectX          :: Int
                       -- ^ x coordinate in pixels
                     , objectY          :: Int
                       -- ^ y coordinate in pixels
                     , objectRotation   :: Float
                       -- ^ Angle in degrees clockwise
                     , objectGid        :: GlobalId
                       -- ^ GID, only if object comes from a Tilemap
                     , objectEllipse    :: Bool
                       -- ^ Used to mark an object as an ellipse
                     , objectPolygon    :: Vector (Int, Int)
                       -- ^ A list of x,y coordinates in pixels
                     , objectPolyline   :: Vector (Int, Int)
                       -- ^ A list of x,y coordinates in pixels
                     , objectText       :: Map Text Text
                       -- ^ String key-value pairs
                     } deriving (Generic, Show)


data Layer = Layer { layerWidth      :: Int
                     -- ^ Column count. Same as map width for fixed-size maps.
                   , layerHeight     :: Int
                     -- ^ Row count. Same as map height for fixed-size maps.
                   , layerName       :: String
                     -- ^ Name assigned to this layer
                   , layerType       :: String
                     -- ^ “tilelayer”, “objectgroup”, or “imagelayer”
                   , layerVisible    :: Bool
                     -- ^ Whether layer is shown or hidden in editor
                   , layerX          :: Int
                     -- ^ Horizontal layer offset in tiles. Always 0.
                   , layerY          :: Int
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
                   } deriving (Generic, Show)


data Terrain = Terrain { terrainName :: String
                         -- ^ Name of terrain
                       , terrainTile :: LocalId
                         -- ^ Local ID of tile representing terrain
                       } deriving (Generic, Show)


data Frame = Frame { frameDuration :: Int
                   , frameTileId   :: LocalId
                   } deriving (Generic, Show)


data Tile = Tile { tileId          :: LocalId
                 , tileProperties  :: Map Text Text
                 , tileImage       :: Maybe Value
                 , tileObjectGroup :: Vector Object
                 , tileAnimation   :: Vector Frame
                 } deriving (Generic, Show)


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
                       , tilesetTiles          :: Map GlobalId (Map LocalId (Int, Int, Int, Int))
                         -- ^ Gid-indexed Tiles (optional)
                       } deriving (Generic, Show)


-- | The full monty.
data Tiledmap = Tiledmap { tiledmapVersion         :: Float
                           -- ^ The JSON format version
                         , tiledmapTiledversion    :: String
                           -- ^ The Tiled version used to save the file
                         , tiledmapWidth           :: Int
                           -- ^ Number of tile columns
                         , tiledmapHeight          :: Int
                           -- ^ Number of tile rows
                         , tiledmapTilewidth       :: Int
                           -- ^ Map grid width.
                         , tiledmapTileheight      :: Int
                           -- ^ Map grid height.
                         , tiledmapOrientation     :: String
                           -- ^ Orthogonal, isometric, or staggered
                         , tiledmapLayers          :: Vector Layer
                           -- ^ Array of Layers
                         , tiledmapTilesets        :: Vector Tileset
                           -- ^ Array of Tilesets
                         , tiledmapBackgroundcolor :: String
                           -- ^ Hex-formatted color (#RRGGBB or #AARRGGBB) (optional)
                         , tiledmapRenderorder     :: String
                           -- ^ Rendering direction (orthogonal maps only)
                         , tiledmapProperties      :: Map Text Text
                           -- ^ String key-value pairs
                         , tiledmapNextobjectid    :: Int
                           -- ^ Auto-increments for each placed object
                         } deriving (Generic, Show)


instance FromJSON Tiledmap where
  parseJSON (A.Object o) = Tiledmap <$> o .: "version"
                                    <*> o .: "tiledversion"
                                    <*> o .: "width"
                                    <*> o .: "height"
                                    <*> o .: "tilewidth"
                                    <*> o .: "tileheight"
                                    <*> o .: "orientation"
                                    <*> o .: "layers"
                                    <*> o .: "tilesets"
                                    <*> o .: "backgroundcolor"
                                    <*> o .: "renderorder"
                                    <*> o .: "properties"
                                    <*> o .: "nextobjectid"
  parseJSON invalid = typeMismatch "Tiledmap" invalid



---- | Orientations
--data MapOrientation = Orthogonal | Isometric deriving (Show, Eq)
--
---- | Properties
--type Properties = [(String, String)]
--
---- | A tiled map.
--data Tiledmap = Tiledmap
--         { mapPath             :: FilePath -- ^ The file path of the map file.
--         , mapOrientation      :: MapOrientation
--         , mapWidth, mapHeight :: Int
--         , mapTileWidth        :: Int
--         , mapTileHeight       :: Int
--         , mapProperties       :: Properties
--         , mapTilesets         :: [Tileset]
--         , mapLayers           :: [Layer]
--         } deriving (Show, Eq)
--
---- | A set of tiles that can be used.
--data Tileset = Tileset
--             { tsName                    :: String
--             , tsInitialGid              :: Word32
--             , tsTileWidth, tsTileHeight :: Int
--             , tsTileCount               :: Int
--             , tsSpacing, tsMargin       :: Int
--             , tsImages                  :: [Image] -- ^ Multiple images not
--                                                    -- yet supported in tiled.
--             , tsProperties              :: Properties
--             , tsTiles                   :: [Tile]
--             , tsColumns                 :: Int
--             } deriving (Show, Eq)
--
---- | One frame of an animation.
--data Frame = Frame { frameTileId   :: Word32
--                   -- ^ The local ID of a tile within the parent TileSet.
--                   , frameDuration :: Int
--                   -- ^ How long (in milliseconds) this frame should be
--                   -- displayed before advancing to the next frame.
--                   } deriving (Show, Eq, Ord)
--
---- | Contains a list of animation frames.
--newtype Animation = Animation { animationFrames :: [Frame] }
--                  deriving (Show, Eq, Ord)
--
---- | A tile as defined in a TileSet.
--data Tile = Tile { tileId          :: Word32
--                 -- ^ The local tile ID within its tileset.
--                 -- TODO: Add terrain and probability
--                 , tileProperties  :: Properties
--                 , tileImage       :: Maybe Image
--                 , tileObjectGroup :: [Object]
--                 , tileAnimation   :: Maybe Animation
--                 } deriving (Show, Eq)
--
---- | An image containing tiles.
--data Image = Image
--           { iSource         :: FilePath
--           , iTrans          :: Maybe (Word8, Word8, Word8)
--           , iWidth, iHeight :: Int
--           } deriving (Show, Eq)
--
---- | An object, usable for stuff not repetitively aligned on a grid.
--data Object = Object
--            { objectName                :: Maybe String
--            , objectType                :: Maybe String
--            , objectProperties          :: Properties
--            , objectX, objectY          :: Double
--            , objectWidth, objectHeight :: Maybe Double
--            , objectGid                 :: Maybe Word32
--            , objectPolygon             :: Maybe Polygon
--            , objectPolyline            :: Maybe Polyline
--            } deriving (Show, Eq)
--
---- | A polygon.
--newtype Polygon = Polygon [(Int, Int)] deriving (Show, Eq)
--
---- | A polyline.
--newtype Polyline = Polyline [(Int, Int)] deriving (Show, Eq)
--
---- | A single tile index as is stored in a layer.
--data TileIndex = TileIndex { tileIndexGid           :: Word32
--                           , tileIndexIsVFlipped    :: Bool
--                           , tileIndexIsHFlipped    :: Bool
--                           , tileIndexIsDiagFlipped :: Bool
--                           } deriving (Show, Eq, Ord)
--
---- | A collection of multidimensional tile data.
--type TileData = Vector (Vector (Maybe TileIndex))
--
---- | A layer's tile contents.
--data LayerContents = LayerContentsTiles   TileData
--                   | LayerContentsObjects [Object]
--                   | LayerContentsImage   Image
--                   deriving (Eq)
--
--instance Show LayerContents where
--    show (LayerContentsTiles _)      = "LayerContentsTiles "   ++ show "..."
--    show (LayerContentsObjects objs) = "LayerContentsObjects " ++ show objs
--    show (LayerContentsImage img)    = "LayerContentsImage "   ++ show img
--
---- | An entire layer of tiled data.
--data Layer = Layer { layerName       :: String
--                   , layerOpacity    :: Float
--                   , layerIsVisible  :: Bool
--                   , layerProperties :: Properties
--                   , layerOffset     :: (Int, Int)
--                   , layerContents   :: LayerContents
--                   } deriving (Show, Eq)
