module Money.CanMakeChange where

import Data.Either (Either)
import Data.Ring (class Ring)
import Data.Show (class Show)
import Prelude (class Eq)

data MakeChangeError = CannotMakeChange

instance makeChangeErrorShow ∷ Show MakeChangeError where
    show CannotMakeChange = "CannotMakeChange"

derive instance eqChangeErrorShow ∷ Eq MakeChangeError

-- | link between physical monetary value (wallet) and hypothetical monetary value (amount).
class (Ring moneySet, Ring amount) ⇐ CanMakeChange moneySet amount where
    -- | figures out how to allocate value from the moneySet that is equivalent to the amount.
    -- | returns MakeChangeError if it can't allocate the exact amount from the moneySet.
    makeChange ∷ moneySet → amount → Either MakeChangeError moneySet