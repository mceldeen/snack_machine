module Money.USD (USD, usd) where

import Prelude (class Eq, (<$>))
import Data.Semiring (class Semiring, add, zero, mul, one)
import Data.Ring (class Ring, sub)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Control.Monad.Gen.Class (chooseInt)

newtype USD = USD Int

derive instance eqUSD ∷ Eq USD

instance semiringUSD ∷ Semiring USD where
    add (USD a) (USD b) = USD (add a b)
    zero = USD zero
    mul (USD a) (USD b) = USD (mul a b)
    one = USD one

instance ringUSD ∷ Ring USD where
    sub (USD a) (USD b) = USD (sub a b)

instance arbitraryUSD ∷ Arbitrary USD where
    arbitrary = USD <$> chooseInt 0 10000

usd ∷ Int → USD
usd cents = USD cents