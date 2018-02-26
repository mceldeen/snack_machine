module Money.USDWallet
    ( USDWallet
    , pennies
    , dimes
    , quarters
    , oneDollarBills
    , fiveDollarBills
    , twentyDollarBills
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
        { pennies ∷ Int
        , dimes ∷ Int
        , quarters ∷ Int
        , oneDollarBills ∷ Int
        , fiveDollarBills ∷ Int
        , twentyDollarBills ∷ Int
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
        { pennies: a.pennies `op` b.pennies
        , dimes: a.dimes `op` b.dimes
        , quarters: a.quarters `op` b.quarters
        , oneDollarBills: a.oneDollarBills `op` b.oneDollarBills
        , fiveDollarBills: a.fiveDollarBills `op` b.fiveDollarBills
        , twentyDollarBills: a.twentyDollarBills `op` b.twentyDollarBills
        }

usdWallet ∷ Int → Int → Int → Int → Int → Int → USDWallet
usdWallet pennies dimes quarters oneDollarBills fiveDollarBills twentyDollarBills =
    USDWallet
        { pennies: pennies
        , dimes: dimes
        , quarters: quarters
        , oneDollarBills: oneDollarBills
        , fiveDollarBills: fiveDollarBills
        , twentyDollarBills: twentyDollarBills
        }

twentyDollarBills :: Lens' USDWallet Int
twentyDollarBills =
    lens
        (\(USDWallet bag) → bag.twentyDollarBills)
        (\(USDWallet bag) c → USDWallet $ bag { twentyDollarBills = c })

fiveDollarBills :: Lens' USDWallet Int
fiveDollarBills =
    lens
        (\(USDWallet bag) → bag.fiveDollarBills)
        (\(USDWallet bag) c → USDWallet $ bag { fiveDollarBills = c })

oneDollarBills :: Lens' USDWallet Int
oneDollarBills =
    lens
        (\(USDWallet bag) → bag.oneDollarBills)
        (\(USDWallet bag) c → USDWallet $ bag { oneDollarBills = c })

quarters :: Lens' USDWallet Int
quarters =
    lens
        (\(USDWallet bag) → bag.quarters)
        (\(USDWallet bag) c → USDWallet $ bag { quarters = c })

dimes :: Lens' USDWallet Int
dimes =
    lens
        (\(USDWallet bag) → bag.dimes)
        (\(USDWallet bag) c → USDWallet $ bag { dimes = c })

pennies :: Lens' USDWallet Int
pennies =
    lens
        (\(USDWallet bag) → bag.pennies)
        (\(USDWallet bag) c → USDWallet $ bag { pennies = c })