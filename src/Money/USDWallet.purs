module Money.USDWallet 
    ( USDWallet
    , usdWallet
    , oneCentCount
    , tenCentCount
    , quarterCount
    , oneDollarCount
    , fiveDollarCount
    , twentyDollarCount
    ) where

import Control.Monad.Gen.Class (chooseInt)
import Data.Either (Either(..), either)
import Data.Lens (Lens', lens, set, view)
import Data.Ring (class Ring)
import Data.Semiring (class Semiring, one, zero)
import Money.CanMakeChange (class CanMakeChange, MakeChangeError(..))
import Money.USD (USD, cents)
import Prelude (class Eq, const, min, ($), (*), (+), (-), (/), (<#>), (<$>), (<*>), (==), (>>=))
import Test.QuickCheck.Arbitrary (class Arbitrary)


newtype USDWallet = USDWallet 
    { oneCentCount ∷ Int
    , tenCentCount ∷ Int
    , quarterCount ∷ Int
    , oneDollarCount ∷ Int
    , fiveDollarCount ∷ Int
    , twentyDollarCount ∷ Int
    }

usdWallet ∷ Int → Int → Int → Int → Int → Int → USDWallet
usdWallet oneCentCount tenCentCount quarterCount oneDollarCount fiveDollarCount twentyDollarCount =
    USDWallet {  oneCentCount, tenCentCount, quarterCount, oneDollarCount, fiveDollarCount, twentyDollarCount }

twentyDollarCount :: Lens' USDWallet Int
twentyDollarCount =
    lens 
        (\(USDWallet bag) → bag.twentyDollarCount) 
        (\(USDWallet bag) twentyDollarCount → USDWallet $ bag { twentyDollarCount = twentyDollarCount })

fiveDollarCount :: Lens' USDWallet Int
fiveDollarCount =
    lens 
        (\(USDWallet bag) → bag.fiveDollarCount) 
        (\(USDWallet bag) fiveDollarCount → USDWallet $ bag { fiveDollarCount = fiveDollarCount })

oneDollarCount :: Lens' USDWallet Int
oneDollarCount =
    lens 
        (\(USDWallet bag) → bag.oneDollarCount) 
        (\(USDWallet bag) oneDollarCount → USDWallet $ bag { oneDollarCount = oneDollarCount })

quarterCount :: Lens' USDWallet Int
quarterCount =
    lens 
        (\(USDWallet bag) → bag.quarterCount) 
        (\(USDWallet bag) quarterCount → USDWallet $ bag { quarterCount = quarterCount })

tenCentCount :: Lens' USDWallet Int
tenCentCount =
    lens 
        (\(USDWallet bag) → bag.tenCentCount) 
        (\(USDWallet bag) tenCentCount → USDWallet $ bag { tenCentCount = tenCentCount })

oneCentCount :: Lens' USDWallet Int
oneCentCount =
    lens 
        (\(USDWallet bag) → bag.oneCentCount) 
        (\(USDWallet bag) oneCentCount → USDWallet $ bag { oneCentCount = oneCentCount })

derive instance eqUSDWallet ∷ Eq USDWallet

instance semiringUSDWallet ∷ Semiring USDWallet where
    add (USDWallet a) (USDWallet b) = 
        USDWallet 
            { oneCentCount: a.oneCentCount + b.oneCentCount
            , tenCentCount: a.tenCentCount + b.tenCentCount
            , quarterCount: a.quarterCount + b.quarterCount
            , oneDollarCount: a.oneDollarCount + b.oneDollarCount
            , fiveDollarCount: a.fiveDollarCount + b.fiveDollarCount
            , twentyDollarCount: a.twentyDollarCount + b.twentyDollarCount
            }

    zero = 
        USDWallet
            { oneCentCount: zero
            , tenCentCount: zero
            , quarterCount: zero
            , oneDollarCount: zero
            , fiveDollarCount: zero
            , twentyDollarCount: zero
            }

    mul (USDWallet a) (USDWallet b) = 
        USDWallet
            { oneCentCount: a.oneCentCount * b.oneCentCount
            , tenCentCount: a.tenCentCount * b.tenCentCount
            , quarterCount: a.quarterCount * b.quarterCount
            , oneDollarCount: a.oneDollarCount * b.oneDollarCount
            , fiveDollarCount: a.fiveDollarCount * b.fiveDollarCount
            , twentyDollarCount: a.twentyDollarCount * b.twentyDollarCount
            }

    one = 
        USDWallet
            { oneCentCount: one
            , tenCentCount: one
            , quarterCount: one
            , oneDollarCount: one
            , fiveDollarCount: one
            , twentyDollarCount: one
            }

instance ringUSDWallet ∷ Ring USDWallet where
    sub (USDWallet a) (USDWallet b) = 
        USDWallet
            { oneCentCount: a.oneCentCount - b.oneCentCount
            , tenCentCount: a.tenCentCount - b.tenCentCount
            , quarterCount: a.quarterCount - b.quarterCount
            , oneDollarCount: a.oneDollarCount - b.oneDollarCount
            , fiveDollarCount: a.fiveDollarCount - b.fiveDollarCount
            , twentyDollarCount: a.twentyDollarCount - b.twentyDollarCount
            }

instance arbitraryUSDWallet ∷ Arbitrary USDWallet where
    arbitrary = 
        usdWallet 
            <$> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100

instance canMakeChangeUSDWalletWithUSD ∷ CanMakeChange USDWallet USD where
    makeChange bag amount = 
          swapRightAndLeft result
        where
            swapRightAndLeft = 
                either Right Left

            result =
                Right { change: (zero ∷ USDWallet), amountInCents: (view cents amount) }
                    >>= makeChangeForDenomination 2000 twentyDollarCount
                    >>= makeChangeForDenomination 500 fiveDollarCount
                    >>= makeChangeForDenomination 100 oneDollarCount
                    >>= makeChangeForDenomination 25 quarterCount
                    >>= makeChangeForDenomination 10 tenCentCount
                    >>= makeChangeForDenomination 1 oneCentCount
                    <#> const CannotMakeChange

            makeChangeForDenomination 
                ∷ Int 
                → Lens' USDWallet Int 
                → {change ∷ USDWallet, amountInCents ∷ Int} 
                → Either USDWallet {change ∷ USDWallet, amountInCents ∷ Int}
            makeChangeForDenomination denominationInCents unitLens {change, amountInCents} =
                let 
                    maxUnitsToConsume = amountInCents / denominationInCents
                    unitsToConsume = min maxUnitsToConsume (view unitLens bag)
                    remainder = amountInCents - (unitsToConsume * denominationInCents)
                in
                    if remainder == 0 then
                        Left $ set unitLens unitsToConsume change 
                    else
                        Right $ 
                            { change: set unitLens unitsToConsume change
                            , amountInCents: remainder
                            }