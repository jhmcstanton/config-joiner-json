module           Config.JSON.IO (readConfigFiles) where

import           Control.Monad (sequence)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.ByteString.Lazy (readFile)
import           Prelude (IO, (.), ($), fmap, return, zip)

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
