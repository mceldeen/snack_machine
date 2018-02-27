module Test.Money.USDSet where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Money.USDSet (USDSet)
import Test.RingHelpers (additiveInverse)
import Test.SemiringHelpers (annihilation, associative, commutative, identity, leftDistributivity, rightDistributivity)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

main :: ∀ e. TestSuite ( random :: RANDOM | e )
main =
  suite "Money.USDSet" do
    suite "USDSet" do
      suite "Semiring" do
        suite "addition" do
          test "associative" $ quickCheck ((associative add) ∷ USDSet → USDSet → USDSet → Boolean)
          test "identity" $ quickCheck ((identity add zero) ∷  USDSet → Boolean)
          test "commutative" $ quickCheck ((commutative add) ∷ USDSet → USDSet → Boolean)

        suite "mutliplication" do
          test "associative" $ quickCheck ((associative mul) ∷ USDSet → USDSet → USDSet → Boolean)
          test "identity" $ quickCheck ((identity mul one) ∷  USDSet → Boolean)

        suite "addition with multiplication" do
          test "left distributivity" $ quickCheck ((leftDistributivity mul add) ∷ USDSet → USDSet → USDSet → Boolean)
          test "right distributivity" $ quickCheck ((rightDistributivity mul add) ∷ USDSet → USDSet → USDSet → Boolean)

        test "annihilation" $ quickCheck ((annihilation mul zero) ∷ USDSet → Boolean)

      suite "Ring" do
        test "additive inverse" $ quickCheck ((additiveInverse sub add zero) ∷ USDSet → Boolean)
