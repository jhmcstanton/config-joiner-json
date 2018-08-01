module           Config.JSON.IO (
    readConfigFiles,
    writeConfigFiles
  ) where

import           Control.Monad (sequence)
import           Data.Foldable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.ByteString.Lazy (readFile, writeFile)
import           Prelude (IO, (.), ($), fmap, pure, uncurry, zip)

import           Config.JSON.Types

{-|
Reads the common configuration file and all common configuration file and
all environment configuration files (if any) into their Byte representations.

Note that this may throw an exception if an error occurs during file reading.
-}
readConfigFiles :: CommonConfigFile
  -> [EnvConfigFile]
  -> IO (CommonConfigBytes, HashMap EnvConfigFile (EnvConfigBytes PreProcess))
readConfigFiles (CommonConfigFile common) envFiles = do
  commonBytes <- readFile common
  envBytes <- sequence . fmap (readFile . envConfigSourceFile) $ envFiles
  let wrappedBytes = fmap EnvConfigBytes envBytes
  let pathsToBytes = M.fromList (zip envFiles wrappedBytes)
  pure (CommonConfigBytes commonBytes, pathsToBytes)

{-|
Writes generated JSON values to their target locations.

Note that this may throw an exception if an error occurs during file writing,
and file generation may be in a partial status.
-}
writeConfigFiles :: HashMap EnvConfigFile (EnvConfigBytes PostProcess) -> IO ()
writeConfigFiles = traverse_ (uncurry writeConfigFile) . M.toList 

{-|
Writes a generated JSON value to its target location.

Note that this may throw an exception if an error occurs during file writing.
-}
writeConfigFile :: EnvConfigFile -> (EnvConfigBytes PostProcess) -> IO ()
writeConfigFile (EnvConfigFile _ target) (EnvConfigBytes json) = writeFile target json
