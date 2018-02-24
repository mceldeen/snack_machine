module Test.Money.USD where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Money.USD (USD)
import Test.RingHelpers (additiveInverse)
import Test.SemiringHelpers (annihilation, associative, commutative, identity, leftDistributivity, rightDistributivity)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

main :: ∀ e. TestSuite ( random :: RANDOM | e )
main =
  suite "Money.USD" do
    suite "USD" do
      suite "semiring" do
        suite "addition" do
          test "associative" $ quickCheck ((associative add) ∷ USD → USD → USD → Boolean)
          test "identity" $ quickCheck ((identity add zero) ∷  USD → Boolean)
          test "commutative" $ quickCheck ((commutative add) ∷ USD → USD → Boolean)

        suite "mutliplication" do
          test "associative" $ quickCheck ((associative mul) ∷ USD → USD → USD → Boolean)
          test "identity" $ quickCheck ((identity mul one) ∷  USD → Boolean)

        suite "addition with multiplication" do
          test "left distributivity" $ quickCheck ((leftDistributivity mul add) ∷ USD → USD → USD → Boolean)
          test "right distributivity" $ quickCheck ((rightDistributivity mul add) ∷ USD → USD → USD → Boolean)

        test "annihilation" $ quickCheck ((annihilation mul zero) ∷ USD → Boolean)

      suite "ring" do
        test "additive inverse" $ quickCheck ((additiveInverse sub add zero) ∷ USD → Boolean)
