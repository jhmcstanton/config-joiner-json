{-# LANGUAGE EmptyDataDecls, InstanceSigs #-}
module Config.JSON.Types where

import           Data.Aeson (Value)
import           Data.ByteString.Lazy
import           Data.Hashable

--
-- Parsed JSON newtypes
--

-- |The configuration values that are common across all environments.
newtype CommonConfig = CommonConfig { commonValue :: Value }

-- |The environment specific values.
newtype EnvConfig a = EnvConfig { envValue :: Value }

--
-- Status types to indicate if an environment file is pre or post
-- processing
--

-- |Indicates that the tagged type has not been processed
data PreProcess

-- |Indicates that the tagged type has been processed
data PostProcess


--
-- File newtypes
--

-- |The path to the common configuration file
newtype CommonConfigFile = CommonConfigFile {
  commonConfigFile :: FilePath
  } deriving (Eq, Ord, Show)

-- |The path to the source environment configuration file
-- |and the target output file.
data EnvConfigFile = EnvConfigFile {
  envConfigSourceFile :: FilePath,
  envConfigTargetFile :: FilePath
  } deriving (Eq, Ord, Show)

instance Hashable EnvConfigFile where
  hashWithSalt :: Int -> EnvConfigFile -> Int
  hashWithSalt salt (EnvConfigFile source target) =
    let
      sourceHash = hashWithSalt salt source
      targetHash = hashWithSalt salt target
    in
      sourceHash * targetHash

-- |Smart constructor for creating instances of EnvConfigFile.
-- |Ensures that the source and target are not equal
envConfigFile :: FilePath -> FilePath -> Maybe EnvConfigFile
envConfigFile source target =
  if source == target
  then Nothing
  else Just $ EnvConfigFile source target

-- |The contents the common configuration file
newtype CommonConfigBytes = CommonConfigBytes { commonConfigBytes :: ByteString }

-- |The contents of an environment configuration file
newtype EnvConfigBytes = EnvConfigBytes { envConfigBytes :: ByteString }
