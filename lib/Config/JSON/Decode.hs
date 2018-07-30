module Config.JSON.Decode where

import           Control.Monad (sequence)
import           Data.Aeson
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Prelude (Either(..), String, (.), ($), fmap, return)

import           Config.JSON.Types

{-|
Converts read ByteStrings into raw Aeson Values for manipulation.
If any cannot be decoded properly this returns Left.
-}
decodeBytes :: CommonConfigBytes
  -> HashMap EnvConfigFile EnvConfigBytes
  -> Either String (CommonConfig, HashMap EnvConfigFile (EnvConfig PreProcess))
decodeBytes (CommonConfigBytes commonBytes) envs = do
  commonJson <- eitherDecode commonBytes
  pathsToVals <- sequence . fmap (sequence . fmap (eitherDecode . envConfigBytes)) . M.toList $ envs
  let pathsToConfigs = fmap (fmap EnvConfig) pathsToVals
  return (CommonConfig commonJson, M.fromList pathsToConfigs)

