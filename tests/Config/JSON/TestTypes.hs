module Config.JSON.TestTypes (Test) where

import           Prelude (String)
import           Test.Tasty.HUnit( Assertion )

type Test = (String, Assertion)
