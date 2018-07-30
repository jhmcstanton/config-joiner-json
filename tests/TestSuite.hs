module Main ( main ) where

import qualified Config.JSON.Types.Tests

import           Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "config-joiner-json"
  [
    Config.JSON.Types.Tests.tests
  ]
