-------------------------------------------------------------------------------
-- |
-- Module      : Config.JSON.Bytes
-- Maintainer  : Jim McStanton
-- Stability   : experimental
-- Portability : portable
--
-- Functions for decoding and encoding JSON values from ByteStrings
--
-------------------------------------------------------------------------------
module Config.JSON.Bytes where

import           Data.Aeson
import           Data.HashMap.Lazy (HashMap)
import           Data.Traversable
import           Prelude (Either(..), String, (.), fmap, pure)

import           Config.JSON.Types

{-|
Converts read ByteStrings into raw Aeson Values for manipulation.
If any cannot be decoded properly this returns Left.
-}
decodeBytes :: CommonConfigBytes
  -> HashMap (ConfigFile Env) (ConfigBytes Env PreProcess)
  -> Either String (CommonConfig, HashMap (ConfigFile Env) (Config Env PreProcess))
decodeBytes commonBytes envs = do
  commonJson     <- decodeBytes' commonBytes
  pathsToConfigs <- traverse decodeBytes' envs
  pure (commonJson, pathsToConfigs)

{-|
Decodes read bytes into the related JSON-wrapping type.
-}
decodeBytes' :: (ByteType a, ConfigType a) => ConfigBytes a b -> Either String (Config a b)
decodeBytes' = fmap fromValue . eitherDecode . toBytes 

{-|
Encodes processed JSON values to ByteStrings that are ready
to write to disk.
-}
encodeBytes :: HashMap (ConfigFile Env) (Config Env PostProcess)
  -> HashMap (ConfigFile Env) (ConfigBytes Env PostProcess)
encodeBytes = fmap (fromBytes . encode)
