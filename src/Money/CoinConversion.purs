module CoinConversion where

import Data.Foldable (sum)
import Data.Lens ((.~), (^.))
import Data.Ring (class Ring, zero, (*))
import Money.USD (USD, cents, fromCents)
import Money.USDSet (USDSet, dimes, fiveDollarBills, oneDollarBills, pennies, quarters, twentyDollarBills)
import Prelude (($))

class (Ring coinSet, Ring coin) ⇐ CoinConversion coin coinSet where
    -- | MUST BE IN TERMS OF THE SMALLEST DENOMINATION IN THE COINSET
    convertToSmallestCoin ∷ coin → coinSet

    convertToValue ∷ coinSet → coin

instance coinConversionIntInt ∷ CoinConversion Int Int where
    convertToSmallestCoin coin = coin
    convertToValue coinSet = coinSet

instance coinConversionUSDUSDSet ∷ CoinConversion USD USDSet where
    convertToSmallestCoin usd =
        pennies .~ (usd ^. cents) $ zero

    convertToValue coinSet =
        let
            valueInCents =
                sum [
                    (coinSet ^. pennies             ) * 1,
                    (coinSet ^. dimes               ) * 10,
                    (coinSet ^. quarters            ) * 25,
                    (coinSet ^. oneDollarBills      ) * 100,
                    (coinSet ^. fiveDollarBills     ) * 500,
                    (coinSet ^. twentyDollarBills   ) * 2000
                ]
        in fromCents valueInCents
