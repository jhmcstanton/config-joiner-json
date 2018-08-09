-------------------------------------------------------------------------------
-- |
-- Module      : Config.JSON.Join
-- Maintainer  : Jim McStanton
-- Stability   : experimental
-- Portability : portable
--
-- Functions for joining common configuration values with specific values.
--
-------------------------------------------------------------------------------
module Config.JSON.Join (join, join') where

import           Data.Aeson
import qualified Data.HashMap.Strict as M

import           Prelude (Functor, fmap, ($))

import           Config.JSON.Types (
  CommonConfig,
  Config(..),
  Env,
  PreProcess,
  PostProcess,
  fromValue
  )

{-|
Joins a common JSON configuration file with
0 or more environment files. These files can be partial (missing
the common fields) or completed (all fields overridden from the common
config). Overridden fields will be picked in the output JSON.

Note that this function expects that the two JSON files have the same common
root.
-}
join :: Functor f => CommonConfig -> f (Config Env PreProcess) -> f (Config Env PostProcess)
join common = fmap (join' common)

{-|
Joins a single common configuration file with a single
environment specific configuration file. The environment
file can be partial (missing the common fields) or completed (all
common fields are overridden/populated). Overridden fields will be
picked in the output JSON.

Note that this function expects that the two JSON files have the same common
root.
-}
join' :: CommonConfig -> Config Env PreProcess -> Config Env PostProcess
join' (CommonConfig' common) (EnvConfig env) = fromValue (joinJson common env) where
  joinJson :: Value -> Value -> Value
  joinJson (Object commonObj) (Object envObj) = Object $ M.unionWith joinJson commonObj envObj
  joinJson (Array _) envArr@(Array _) = envArr
  joinJson (String _) envStr@(String _) = envStr
  joinJson (Number _) envNum@(Number _) = envNum
  joinJson Null Null = Null
  joinJson _ any = any
