module Money.USD
    ( USD
    , cents
    ) where

import Control.Monad.Gen.Class (chooseInt)
import Data.Lens (Lens', lens)
import Data.Ring (class Ring, sub)
import Data.Semiring (class Semiring, add, zero, mul, one)
import Prelude (class Eq, (<$>))
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype USD = USD Int

cents ∷ Lens' USD Int
cents =
    lens 
        (\(USD cents) → cents)
        (\(USD _) cents → USD cents)

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