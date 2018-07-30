module           Config.JSON.IO where

import           Control.Monad (sequence)
import           Data.Aeson
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.ByteString.Lazy (readFile)
import           Prelude (IO, Either(..), String, (.), ($), fmap, return, zip)

import           Config.JSON.Types

{-|
Reads the common configuration file and all common configuration file and
all environment configuration files (if any) into their Byte representations.

Note that this may throw an exception if an error occurs during file reading.
-}
readConfigFiles :: CommonConfigFile
  -> [EnvConfigFile]
  -> IO (CommonConfigBytes, HashMap EnvConfigFile EnvConfigBytes)
readConfigFiles (CommonConfigFile common) envFiles = do
  commonBytes <- readFile common
  envBytes <- sequence . fmap (readFile . envConfigSourceFile) $ envFiles
  let wrappedBytes = fmap EnvConfigBytes envBytes
  let pathsToBytes = M.fromList (zip envFiles wrappedBytes)
  return (CommonConfigBytes commonBytes, pathsToBytes)

{-|
Converts read ByteStrings into raw Aeson Values for manipulation.
If any cannot be decoded properly this returns Left.
-}
decodeBytes :: CommonConfigBytes
  -> HashMap EnvConfigFile EnvConfigBytes
  -> Either String (CommonConfig, HashMap EnvConfigFile EnvConfig)
decodeBytes (CommonConfigBytes commonBytes) envs = do
  commonJson <- eitherDecode commonBytes
  pathsToVals <- sequence . fmap (sequence . fmap (eitherDecode . envConfigBytes)) . M.toList $ envs
  let pathsToConfigs = fmap (fmap EnvConfig) pathsToVals
  return (CommonConfig commonJson, M.fromList pathsToConfigs)


