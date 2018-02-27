module CoinConversion where

import Data.Lens ((.~), (^.))
import Data.Ring (class Ring, zero)
import Money.USD (USD, cents)
import Money.USDSet (USDSet, pennies)
import Prelude (($))

class (Ring coinSet, Ring coin) ⇐ CoinConversion coin coinSet where
    -- | MUST BE IN TERMS OF THE SMALLEST DENOMINATION IN THE COINSET
    convertToSmallestCoin ∷ coin → coinSet

instance coinConversionIntInt ∷ CoinConversion Int Int where
    convertToSmallestCoin coin = coin

instance coinConversionUSDUSDSet ∷ CoinConversion USD USDSet where
    convertToSmallestCoin usd =
        pennies .~ (usd ^. cents) $ zero
