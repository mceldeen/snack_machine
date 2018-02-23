module Test.Money.USD where

import Prelude

import Control.Monad.Eff.Random (RANDOM)
import Money.USD (USD, USDWallet)
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

associative ∷ ∀ a. Eq a ⇒ (a → a → a) → a → a → a → Boolean
associative op a b c =
  (a `op` b) `op` c == a `op` (b `op` c)

identity ∷ ∀ a. Eq a ⇒ (a → a → a) → a → a → Boolean
identity op zero a =
  zero `op` a == a `op` zero
    && zero `op` a == a
    && a `op` zero == a

commutative ∷ ∀ a. Eq a ⇒ (a → a → a) → a → a → Boolean
commutative op a b =
  a `op` b == b `op` a

leftDistributivity ∷ ∀ a. Eq a ⇒ (a → a → a) → (a → a → a) → a → a → a → Boolean
leftDistributivity mul add a b c =
  a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

rightDistributivity ∷ ∀ a. Eq a ⇒ (a → a → a) → (a → a → a) → a → a → a → Boolean
rightDistributivity mul add a b c =
  (a `add` b) `mul` c == (a `mul` c) `add` (b `mul` c)

annihilation ∷ ∀ a. Eq a ⇒ (a → a → a) → a → a → Boolean
annihilation op zero a =
  zero `op` a == a `op` zero
    && a `op` zero == zero
    && zero `op` a == zero

additiveInverse ∷ ∀ a. Eq a ⇒ (a → a → a) → (a → a → a) → a → a → Boolean
additiveInverse sub add zero a =
  a `sub` a == zero
    && (zero `sub` a) `add` a == zero
