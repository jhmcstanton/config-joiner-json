-------------------------------------------------------------------------------
-- |
-- Module      : Config.JSON
-- Maintainer  : Jim McStanton
-- Stability   : experimental
-- Portability : portable
--
-- Helper main functions for manipulating JSON files.
--
-------------------------------------------------------------------------------
module Config.JSON where

import           Config.JSON.Bytes
import           Config.JSON.IO
import           Config.JSON.Join
import           Config.JSON.Types

import           Prelude (Either(..), IO, (.), ($), error)

{-|
Helper main method when using this as a library. Plumbs together
the work of reading, parsing, joining, and finally writing the
configuration files.
-}
joinMain :: CommonConfigFile -> [EnvConfigFile] -> IO ()
joinMain common envs = do
  (commonBytes, envBytes) <- readConfigFiles common envs
  let decodedFiles = decodeBytes commonBytes envBytes
  case decodedFiles of
    Left msg -> error msg
    Right (commonJson, envJsons) -> do
      let outputEnvBytes = encodeBytes . join commonJson $ envJsons
      writeConfigFiles outputEnvBytes
