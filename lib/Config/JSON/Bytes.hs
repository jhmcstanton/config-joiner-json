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
decodeFiles :: CommonConfigBytes
  -> HashMap EnvConfigFile (EnvConfigBytes PreProcess)
  -> Either String (CommonConfig, HashMap EnvConfigFile (EnvConfig PreProcess))
decodeFiles (CommonConfigBytes commonBytes) envs = do
  commonJson     <- eitherDecode commonBytes
  pathsToConfigs <- traverse (eitherDecode . envConfigBytes) envs
  pure (CommonConfig commonJson, fmap EnvConfig pathsToConfigs)

{-|
Encodes processed JSON values to ByteStrings that are ready
to write to disk.
-}
encodeFiles :: HashMap EnvConfigFile (EnvConfig PostProcess)
  -> HashMap EnvConfigFile (EnvConfigBytes PostProcess)
encodeFiles = fmap (EnvConfigBytes . encode)
