module Test.Main where

import Prelude
  ( Unit
  , ($)
  , pure
  , unit
  )
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import LexerSpec (spec) as LexerSpec

main :: Effect Unit
main =
  launchAff_
    $ runSpec
        [ consoleReporter ]
        LexerSpec.spec