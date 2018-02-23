module Money.USD
    ( USD
    , cents
    , USDWallet
    , usdWallet
    , oneCentCount
    , tenCentCount
    , quarterCount
    , oneDollarCount
    , fiveDollarCount
    , twentyDollarCount
    ) where

import Control.Monad.Gen.Class (chooseInt)
import Data.Lens (Lens', lens, set, view)
import Data.Ring (class Ring, sub)
import Data.Semiring (class Semiring, zero, one)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Data.Either (Either(..), either)
import Money.CanMakeChange (class CanMakeChange, MakeChangeError(..))
import Prelude (class Eq, const, min, ($), (*), (+), (-), (/), (<#>), (<$>), (<*>), (==), (>>=))

newtype USD = USD Int

cents ∷ Lens' USD Int
cents =
    lens
        (\(USD c) → c)
        (\(USD _) c → USD c)

derive instance eqUSD ∷ Eq USD

instance semiringUSD ∷ Semiring USD where
    add (USD a) (USD b) = USD (a + b)
    zero = USD zero
    mul (USD a) (USD b) = USD (a * b)
    one = USD one

instance ringUSD ∷ Ring USD where
    sub (USD a) (USD b) = USD (sub a b)

instance arbitraryUSD ∷ Arbitrary USD where
    arbitrary = USD <$> chooseInt 0 10000

newtype USDWallet =
    USDWallet
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
        (\(USDWallet bag) c → USDWallet $ bag { twentyDollarCount = c })

fiveDollarCount :: Lens' USDWallet Int
fiveDollarCount =
    lens
        (\(USDWallet bag) → bag.fiveDollarCount)
        (\(USDWallet bag) c → USDWallet $ bag { fiveDollarCount = c })

oneDollarCount :: Lens' USDWallet Int
oneDollarCount =
    lens
        (\(USDWallet bag) → bag.oneDollarCount)
        (\(USDWallet bag) c → USDWallet $ bag { oneDollarCount = c })

quarterCount :: Lens' USDWallet Int
quarterCount =
    lens
        (\(USDWallet bag) → bag.quarterCount)
        (\(USDWallet bag) c → USDWallet $ bag { quarterCount = c })

tenCentCount :: Lens' USDWallet Int
tenCentCount =
    lens
        (\(USDWallet bag) → bag.tenCentCount)
        (\(USDWallet bag) c → USDWallet $ bag { tenCentCount = c })

oneCentCount :: Lens' USDWallet Int
oneCentCount =
    lens
        (\(USDWallet bag) → bag.oneCentCount)
        (\(USDWallet bag) c → USDWallet $ bag { oneCentCount = c })

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