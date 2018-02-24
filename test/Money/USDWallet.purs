module Test.Money.USDWallet where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Money.USDWallet (USDWallet)
import Test.RingHelpers (additiveInverse)
import Test.SemiringHelpers (annihilation, associative, commutative, identity, leftDistributivity, rightDistributivity)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

main :: ∀ e. TestSuite ( random :: RANDOM | e )
main =
  suite "Money.USDWallet" do
    suite "USDWallet" do
      suite "Semiring" do
        suite "addition" do
          test "associative" $ quickCheck ((associative add) ∷ USDWallet → USDWallet → USDWallet → Boolean)
          test "identity" $ quickCheck ((identity add zero) ∷  USDWallet → Boolean)
          test "commutative" $ quickCheck ((commutative add) ∷ USDWallet → USDWallet → Boolean)

        suite "mutliplication" do
          test "associative" $ quickCheck ((associative mul) ∷ USDWallet → USDWallet → USDWallet → Boolean)
          test "identity" $ quickCheck ((identity mul one) ∷  USDWallet → Boolean)

        suite "addition with multiplication" do
          test "left distributivity" $ quickCheck ((leftDistributivity mul add) ∷ USDWallet → USDWallet → USDWallet → Boolean)
          test "right distributivity" $ quickCheck ((rightDistributivity mul add) ∷ USDWallet → USDWallet → USDWallet → Boolean)

        test "annihilation" $ quickCheck ((annihilation mul zero) ∷ USDWallet → Boolean)

      suite "Ring" do
        test "additive inverse" $ quickCheck ((additiveInverse sub add zero) ∷ USDWallet → Boolean)
