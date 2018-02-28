module Money.USD
    ( USD
    , cents
    , fromCents
    , toCents
    , dollarsAndCents
    , fromDollarsAndCents
    , toDollarsAndCents
    ) where

import Control.Monad.Gen.Class (chooseInt)
import Data.Eq (class Eq)
import Data.Functor ((<$>))
import Data.Lens (Lens', lens)
import Data.Monoid ((<>))
import Data.Ord (class Ord)
import Data.Ring (class Ring, (-), (*), (+))
import Data.Semiring (class Semiring, zero, one)
import Data.EuclideanRing ((/))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Prelude (const)
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype USD = USD Int

cents ∷ Lens' USD Int
cents =
    lens toCents (const fromCents)

fromCents ∷ Int → USD
fromCents c =
    USD c

toCents ∷ USD → Int
toCents (USD c) =
    c

dollarsAndCents ∷ Lens' USD (Tuple Int Int)
dollarsAndCents =
    lens toDollarsAndCents (const fromDollarsAndCents)

fromDollarsAndCents ∷ Tuple Int Int → USD
fromDollarsAndCents (Tuple dollars cents) =
    USD ((dollars * 100) + cents)

toDollarsAndCents ∷ USD → Tuple Int Int
toDollarsAndCents (USD v) =
    Tuple d c
    where
        d = v / 100
        c = v - (d * 100)

derive instance eqUSD ∷ Eq USD

derive instance ordUSD ∷ Ord USD

instance showUSD ∷ Show USD where
    show (USD cents) =
        show cents <> "¢"

instance semiringUSD ∷ Semiring USD where
    add (USD a) (USD b) = USD (a + b)
    zero = USD zero
    mul (USD a) (USD b) = USD (a * b)
    one = USD one

instance ringUSD ∷ Ring USD where
    sub (USD a) (USD b) = USD (a - b)

instance arbitraryUSD ∷ Arbitrary USD where
    arbitrary = USD <$> chooseInt 0 10000
