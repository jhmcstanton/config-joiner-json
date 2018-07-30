module Config.JSON.Types.Tests (tests) where

import           Config.JSON.Types

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (Assertion, (@=?), testCase)

tests :: TestTree
tests = testGroup "Config.JSON.Types.Tests" $ fmap (uncurry testCase)
  [
    ("Same path envConfigFile smart constructor", samePathEnvConfigFile),
    ("Different path envConfigFile smart constructor", differentPathEnvConfigFile)
  ]


samePathEnvConfigFile :: Assertion
samePathEnvConfigFile = do
  let path = "some/path"
      expected = Nothing
      actual = envConfigFile path path
  expected @=? actual

differentPathEnvConfigFile :: Assertion
differentPathEnvConfigFile = do
  let expected = Just $ EnvConfigFile "source/path" "target/path"
      actual = envConfigFile "source/path" "target/path"
  expected @=? actual
