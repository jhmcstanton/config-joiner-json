module Config.JSON.Types.Tests (tests) where

import           Config.JSON.TestTypes
import           Config.JSON.Types

import           Prelude (Maybe(..), ($), fmap, uncurry)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit ((@=?), testCase)

tests :: TestTree
tests = testGroup "Config.JSON.Types.Tests" $ fmap (uncurry testCase)
  [
    samePathEnvConfigFile,
    differentPathEnvConfigFile
  ]


samePathEnvConfigFile :: Test
samePathEnvConfigFile = ("Same path envConfigFile smart constructor", do
    let path = "some/path"
        expected = Nothing
        actual = envConfigFile path path
    expected @=? actual
  )

differentPathEnvConfigFile :: Test
differentPathEnvConfigFile = ("Different path envConfigFile smart constructor", do
    let expected = Just $ EnvConfigFile "source/path" "target/path"
        actual = envConfigFile "source/path" "target/path"
    expected @=? actual
  )
