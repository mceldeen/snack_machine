module Money.CoinSet
    ( MakeChangeError(..)
    , class CoinSet
    , makeChange
    , isSingleCoin
    ) where

import CoinConversion (class CoinConversion, convertToSmallestCoin)
import Control.Monad ((>>=))
import Data.Either (Either(..), either)
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((/))
import Data.Functor ((<#>))
import Data.Lens (Lens', (.~), (^.))
import Data.Monoid ((<>))
import Data.Ord (min, (<))
import Data.Ring (class Ring, zero, (*), (-), (+))
import Data.Show (class Show, show)
import Money.USDSet (USDSet, fiveDollarBills, pennies, oneDollarBills, quarters, dimes, twentyDollarBills)
import Prelude ((#), ($))


data MakeChangeError = CannotMakeChange Int

instance makeChangeErrorShow ∷ Show MakeChangeError where
    show (CannotMakeChange remainder) = "CannotMakeChange " <> show remainder

derive instance eqChangeErrorShow ∷ Eq MakeChangeError

class (Ring moneySet) ⇐ CoinSet moneySet where
    -- | figures out how to allocate value from the moneySet that is equivalent to the amount.
    -- | returns MakeChangeError if it can't allocate the exact amount from the moneySet.
    -- | Must satisfy the following:
    -- |   `convertToValue <$> makeChange (a + b) c == Right c` where `convertToValue b >= c`
    makeChange ∷ 
        ∀ amount
        . CoinConversion amount moneySet
        ⇒ moneySet
        → amount
        → Either MakeChangeError moneySet

    isSingleCoin ∷ moneySet → Boolean

instance coinSetInt ∷ CoinSet Int where
    isSingleCoin = (==) 1

    makeChange coinSet amount =
        let amountInCoinSet = convertToSmallestCoin amount
        in if coinSet < amountInCoinSet then
            Left $ CannotMakeChange (amountInCoinSet - coinSet)
        else
            Right amountInCoinSet

instance coinSetUSDSetWithUSD ∷ CoinSet USDSet where
    isSingleCoin moneySet =
          moneySet ^. pennies
        + moneySet ^. dimes
        + moneySet ^. quarters
        + moneySet ^. oneDollarBills
        + moneySet ^. fiveDollarBills
        + moneySet ^. twentyDollarBills
        == 1

    makeChange moneySet amount =
          Right { change: (zero ∷ USDSet), amountInCents: amountInPennies }
            >>= makeChangeForDenomination 2000 twentyDollarBills
            >>= makeChangeForDenomination 500 fiveDollarBills
            >>= makeChangeForDenomination 100 oneDollarBills
            >>= makeChangeForDenomination 25 quarters
            >>= makeChangeForDenomination 10 dimes
            >>= makeChangeForDenomination 1 pennies
            <#> (\{amountInCents} → CannotMakeChange amountInCents)
            # flipEither
        where
            flipEither = either Right Left

            amountInPennies = 
                convertToSmallestCoin amount ^. pennies

            makeChangeForDenomination
                ∷ Int
                → Lens' USDSet Int
                → {change ∷ USDSet, amountInCents ∷ Int}
                → Either USDSet {change ∷ USDSet, amountInCents ∷ Int}
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
