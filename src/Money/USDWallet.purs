module Money.USDWallet
    ( USDWallet
    , oneCentCount
    , tenCentCount
    , quarterCount
    , oneDollarCount
    , fiveDollarCount
    , twentyDollarCount
    ) where

import Control.Applicative ((<*>))
import Control.Monad.Gen.Class (chooseInt)
import Data.Eq (class Eq)
import Data.Functor ((<$>))
import Data.Lens (Lens', lens)
import Data.Ord (class Ord)
import Data.Ring (class Ring, (*), (+), (-))
import Data.Semiring (class Semiring)
import Prelude (($))
import Test.QuickCheck.Arbitrary (class Arbitrary)

newtype USDWallet =
    USDWallet
        { oneCentCount ∷ Int
        , tenCentCount ∷ Int
        , quarterCount ∷ Int
        , oneDollarCount ∷ Int
        , fiveDollarCount ∷ Int
        , twentyDollarCount ∷ Int
        }

derive instance eqUSDWallet ∷ Eq USDWallet

derive instance ordUSDWallet ∷ Ord USDWallet

instance semiringUSDWallet ∷ Semiring USDWallet where
    add  = liftOp (+)
    zero = usdWallet 0 0 0 0 0 0
    mul  = liftOp (*)
    one  = usdWallet 1 1 1 1 1 1

instance ringUSDWallet ∷ Ring USDWallet where
    sub = liftOp (-)

instance arbitraryUSDWallet ∷ Arbitrary USDWallet where
    arbitrary =
        usdWallet
            <$> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100

liftOp ∷ (Int -> Int -> Int) -> USDWallet -> USDWallet -> USDWallet
liftOp op (USDWallet a) (USDWallet b) =
    USDWallet
        { oneCentCount: a.oneCentCount `op` b.oneCentCount
        , tenCentCount: a.tenCentCount `op` b.tenCentCount
        , quarterCount: a.quarterCount `op` b.quarterCount
        , oneDollarCount: a.oneDollarCount `op` b.oneDollarCount
        , fiveDollarCount: a.fiveDollarCount `op` b.fiveDollarCount
        , twentyDollarCount: a.twentyDollarCount `op` b.twentyDollarCount
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