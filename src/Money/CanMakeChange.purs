module Money.CanMakeChange where

import Control.Monad ((>>=))
import Data.Either (Either(..), either)
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((/))
import Data.Functor ((<#>))
import Data.Lens (Lens', (.~), (^.))
import Data.Ord (min, (<))
import Data.Ring (class Ring, zero, (*), (-))
import Data.Show (class Show)
import Money.USD (USD, cents)
import Money.USDWallet (USDWallet, fiveDollarBills, pennies, oneDollarBills, quarters, dimes, twentyDollarBills)
import Prelude (const, ($))

data MakeChangeError = CannotMakeChange

instance makeChangeErrorShow ∷ Show MakeChangeError where
    show CannotMakeChange = "CannotMakeChange"

derive instance eqChangeErrorShow ∷ Eq MakeChangeError

-- | link between physical monetary value (wallet) and hypothetical monetary value (amount).
class (Ring moneySet, Ring amount) ⇐ CanMakeChange moneySet amount where
    -- | figures out how to allocate value from the moneySet that is equivalent to the amount.
    -- | returns MakeChangeError if it can't allocate the exact amount from the moneySet.
    makeChange ∷ moneySet → amount → Either MakeChangeError moneySet

instance canMakeChangeInt ∷ CanMakeChange Int Int where
    makeChange moneySet amount =
        if moneySet < amount then
            Left CannotMakeChange
        else
            Right amount

instance canMakeChangeUSDWalletWithUSD ∷ CanMakeChange USDWallet USD where
    makeChange moneySet amount =
          swapRightAndLeft result
        where
            swapRightAndLeft =
                either Right Left

            result =
                Right { change: (zero ∷ USDWallet), amountInCents: (amount ^. cents) }
                    >>= makeChangeForDenomination 2000 twentyDollarBills
                    >>= makeChangeForDenomination 500 fiveDollarBills
                    >>= makeChangeForDenomination 100 oneDollarBills
                    >>= makeChangeForDenomination 25 quarters
                    >>= makeChangeForDenomination 10 dimes
                    >>= makeChangeForDenomination 1 pennies
                    <#> const CannotMakeChange

            makeChangeForDenomination
                ∷ Int
                → Lens' USDWallet Int
                → {change ∷ USDWallet, amountInCents ∷ Int}
                → Either USDWallet {change ∷ USDWallet, amountInCents ∷ Int}
            makeChangeForDenomination denominationInCents denominationLens {change, amountInCents} =
                let
                    maxUnitsToConsume = amountInCents / denominationInCents
                    unitsToConsume = min maxUnitsToConsume (moneySet ^. denominationLens)
                    remainder = amountInCents - (unitsToConsume * denominationInCents)
                in
                    if remainder == 0 then
                        Left $ denominationLens .~ unitsToConsume $ change
                    else
                        Right $
                            { change: denominationLens .~ unitsToConsume $ change
                            , amountInCents: remainder
                            }