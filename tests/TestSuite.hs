module Main ( main ) where

import qualified Config.JSON.Types.Tests

import           Prelude (IO, ($))
import           Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "config-joiner-json"
  [
    Config.JSON.Types.Tests.tests
  ]
