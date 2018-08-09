{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE TypeFamilies #-}
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
    Common,
    Env,
    ConfigType,
    Config(..),
    CommonConfig,
    PreProcess,
    PostProcess,
    CommonConfigFile(..),
    EnvConfigFile(..),
    ByteType,
    ConfigBytes(..),
    CommonConfigBytes,
    fromValue,
    toValue,
    fromBytes,
    toBytes,
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


-- |Indicates that the tagged type is related to a Common Configuration file in some way.
data Common

-- |Indicates that the tagged type is related to a specific or environment configuration file.
data Env

--
-- Parsed JSON newtypes
--

-- |Type for handling parsed JSON
class ConfigType a where
  data Config a :: * -> *
  fromValue     :: Value -> Config a b
  toValue       :: Config a b -> Value

-- |The JSON type for Common files.
instance ConfigType Common where
  data Config Common a          = CommonConfig' Value
  fromValue                     = CommonConfig'
  toValue (CommonConfig' value) = value

-- |The JSON type for Common files.
type CommonConfig = Config Common ()

-- |The JSON type for Environment files.
instance ConfigType Env where
  data Config Env a             = EnvConfig Value
  fromValue                     = EnvConfig
  toValue (EnvConfig value)     = value

instance (ConfigType a) => ToJSON (Config a b) where
    toJSON = toValue

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

-- |Low-level type for handling bytes.
class ByteType a where
  data ConfigBytes a :: * -> *
  toBytes            :: ConfigBytes a b -> ByteString
  fromBytes          :: ByteString -> ConfigBytes a b

-- |The contents the common configuration file
instance ByteType Common where
  data ConfigBytes Common a         = CommonConfigBytes ByteString
  toBytes (CommonConfigBytes bytes) = bytes
  fromBytes                         = CommonConfigBytes

-- |The contents the common configuration file
type CommonConfigBytes = ConfigBytes Common ()

-- |The contents of an environment configuration file
instance ByteType Env where
  data ConfigBytes Env a            = EnvConfigBytes ByteString
  toBytes (EnvConfigBytes bytes)    = bytes
  fromBytes                         = EnvConfigBytes
