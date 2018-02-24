module Test.RingHelpers where

import Data.Eq (class Eq, (==))
import Data.HeytingAlgebra ((&&))

additiveInverse ∷ ∀ a. Eq a ⇒ (a → a → a) → (a → a → a) → a → a → Boolean
additiveInverse sub add zero a =
  a `sub` a == zero
    && (zero `sub` a) `add` a == zero