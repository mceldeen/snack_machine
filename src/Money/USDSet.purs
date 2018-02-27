module Money.USDSet
    ( USDSet
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

newtype USDSet =
    USDSet
        { pennies ∷ Int
        , dimes ∷ Int
        , quarters ∷ Int
        , oneDollarBills ∷ Int
        , fiveDollarBills ∷ Int
        , twentyDollarBills ∷ Int
        }

derive instance eqUSDSet ∷ Eq USDSet

derive instance ordUSDSet ∷ Ord USDSet

instance semiringUSDSet ∷ Semiring USDSet where
    add  = liftOp (+)
    zero = usdSet 0 0 0 0 0 0
    mul  = liftOp (*)
    one  = usdSet 1 1 1 1 1 1

instance ringUSDSet ∷ Ring USDSet where
    sub = liftOp (-)

instance arbitraryUSDSet ∷ Arbitrary USDSet where
    arbitrary =
        usdSet
            <$> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100

liftOp ∷ (Int -> Int -> Int) -> USDSet -> USDSet -> USDSet
liftOp op (USDSet a) (USDSet b) =
    USDSet
        { pennies: a.pennies `op` b.pennies
        , dimes: a.dimes `op` b.dimes
        , quarters: a.quarters `op` b.quarters
        , oneDollarBills: a.oneDollarBills `op` b.oneDollarBills
        , fiveDollarBills: a.fiveDollarBills `op` b.fiveDollarBills
        , twentyDollarBills: a.twentyDollarBills `op` b.twentyDollarBills
        }

usdSet ∷ Int → Int → Int → Int → Int → Int → USDSet
usdSet pennies dimes quarters oneDollarBills fiveDollarBills twentyDollarBills =
    USDSet
        { pennies: pennies
        , dimes: dimes
        , quarters: quarters
        , oneDollarBills: oneDollarBills
        , fiveDollarBills: fiveDollarBills
        , twentyDollarBills: twentyDollarBills
        }

twentyDollarBills :: Lens' USDSet Int
twentyDollarBills =
    lens
        (\(USDSet bag) → bag.twentyDollarBills)
        (\(USDSet bag) c → USDSet $ bag { twentyDollarBills = c })

fiveDollarBills :: Lens' USDSet Int
fiveDollarBills =
    lens
        (\(USDSet bag) → bag.fiveDollarBills)
        (\(USDSet bag) c → USDSet $ bag { fiveDollarBills = c })

oneDollarBills :: Lens' USDSet Int
oneDollarBills =
    lens
        (\(USDSet bag) → bag.oneDollarBills)
        (\(USDSet bag) c → USDSet $ bag { oneDollarBills = c })

quarters :: Lens' USDSet Int
quarters =
    lens
        (\(USDSet bag) → bag.quarters)
        (\(USDSet bag) c → USDSet $ bag { quarters = c })

dimes :: Lens' USDSet Int
dimes =
    lens
        (\(USDSet bag) → bag.dimes)
        (\(USDSet bag) c → USDSet $ bag { dimes = c })

pennies :: Lens' USDSet Int
pennies =
    lens
        (\(USDSet bag) → bag.pennies)
        (\(USDSet bag) c → USDSet $ bag { pennies = c })