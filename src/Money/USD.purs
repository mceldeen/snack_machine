module Money.USD
    ( USD
    , cents
    , USDWallet
    , oneCentCount
    , tenCentCount
    , quarterCount
    , oneDollarCount
    , fiveDollarCount
    , twentyDollarCount
    ) where

import Control.Monad.Gen.Class (chooseInt)
import Data.Lens (Lens', lens)
import Data.Ring (class Ring, sub)
import Data.Semiring (class Semiring, zero, one)
import Prelude (class Eq, ($), (*), (+), (-), (<$>), (<*>))
import Test.QuickCheck.Arbitrary (class Arbitrary)

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
usdWallet pennies dimes quarters oneDollarBills fiveDollarBills twentyDollarBills =
    USDWallet
        { oneCentCount: pennies
        , tenCentCount: dimes
        , quarterCount: quarters
        , oneDollarCount: oneDollarBills
        , fiveDollarCount: fiveDollarBills
        , twentyDollarCount: twentyDollarBills
        }

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
    add  = mapOp (+)
    zero = usdWallet 0 0 0 0 0 0
    mul  = mapOp (*)
    one  = usdWallet 1 1 1 1 1 1

instance ringUSDWallet ∷ Ring USDWallet where
    sub = mapOp (-)

instance arbitraryUSDWallet ∷ Arbitrary USDWallet where
    arbitrary =
        usdWallet
            <$> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100

mapOp ∷ (Int -> Int -> Int) -> USDWallet -> USDWallet -> USDWallet
mapOp op (USDWallet a) (USDWallet b) =
    USDWallet
        { oneCentCount: a.oneCentCount `op` b.oneCentCount
        , tenCentCount: a.tenCentCount `op` b.tenCentCount
        , quarterCount: a.quarterCount `op` b.quarterCount
        , oneDollarCount: a.oneDollarCount `op` b.oneDollarCount
        , fiveDollarCount: a.fiveDollarCount `op` b.fiveDollarCount
        , twentyDollarCount: a.twentyDollarCount `op` b.twentyDollarCount
        }
