module Test.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)

main :: Effect Unit
main = launchAff_ do
  specs <- discover ".*Spec"
  un Identity $ runSpecT testConfig [consoleReporter] specs
  where
      testConfig = { slow: Milliseconds $ toNumber 5000 , timeout: Just $ Milliseconds $ toNumber 30000, exit: false }
