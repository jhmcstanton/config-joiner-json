{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}
-------------------------------------------------------------------------------
-- |
-- Module      : Config.JSON.Types
-- Maintainer  : Jim McStanton
-- Stability   : experimental
-- Portability : portable
--
-- Types related to Config.JSON.
--
-------------------------------------------------------------------------------
module Config.JSON.Types (
    CommonConfig(..),
    EnvConfig(..),
    ProcessState(..),
    CommonConfigFile(..),
    EnvConfigFile(..),
    CommonConfigBytes(..),
    EnvConfigBytes(..),
    envConfigFile
  ) where

import           Data.Aeson (
    ToJSON,
    Value,
    toJSON
  )
import           Data.ByteString.Lazy
import           Data.Hashable
import           Prelude

--
-- Parsed JSON newtypes
--

-- |The configuration values that are common across all environments.
newtype CommonConfig = CommonConfig { commonValue :: Value }

-- |The environment specific values.
newtype EnvConfig (a :: ProcessState) = EnvConfig { envValue :: Value }

instance ToJSON   (EnvConfig a) where
    toJSON = envValue

--
-- Status types to indicate if an environment file is pre or post
-- processing
--

-- |Current processing state of the file. 
data ProcessState :: * where
  -- |Indicates that the tagged type has not been processed
  PreProcess  :: ProcessState
  -- |Indicates that the tagged type has been processed
  PostProcess :: ProcessState


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
newtype EnvConfigBytes (a :: ProcessState) = EnvConfigBytes { envConfigBytes :: ByteString }
