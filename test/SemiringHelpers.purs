module Test.SemiringHelpers where

import Data.Eq (class Eq, (==))
import Data.HeytingAlgebra ((&&))

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