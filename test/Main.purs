module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (run, runTestWith)
import Test.Unit.Output.Fancy as Fancy

import Test.Money.CoinSet as Test.Money.CoinSet
import Test.Money.USD as Test.Money.USD
import Test.Money.USDSet as Test.Money.USDSet

main ∷ ∀ eff. Eff
  ( console ∷ CONSOLE
  , testOutput ∷ TESTOUTPUT
  , avar ∷ AVAR
  , random ∷ RANDOM
  | eff
  ) Unit
main = run $ runTestWith Fancy.runTest do
  Test.Money.CoinSet.main
  Test.Money.USD.main
  Test.Money.USDSet.main