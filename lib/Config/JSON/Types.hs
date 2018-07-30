module Config.JSON.Types where

import           Data.Aeson (Value)
-- |The configuration values that are common across all environments.
newtype CommonConfig = CommonConfig Value

-- |The environment specific values.
newtype EnvConfig = EnvConfig Value

