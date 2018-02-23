module Money.CanMakeChange where

import Data.Either (Either)
import Data.Ring (class Ring)
import Data.Show (class Show)

data MakeChangeError = CannotMakeChange

instance makeChangeErrorShow ∷ Show MakeChangeError where
    show CannotMakeChange = "CannotMakeChange"

class (Ring bag, Ring amount) ⇐ CanMakeChange bag amount where
    -- | Figures out how much to take out of a bag to equal the amount 
    makeChange ∷ bag → amount → Either MakeChangeError bag