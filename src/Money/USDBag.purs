module Money.USDBag (USDBag) where

import Prelude (class Eq, (<$>), (<*>))
import Data.Semiring (class Semiring, add, zero, mul, one)
import Data.Ring (class Ring, sub)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Control.Monad.Gen.Class (chooseInt)

newtype USDBag = USDBag 
    { oneCentCount ∷ Int
    , tenCentCount ∷ Int
    , quarterCount ∷ Int
    , oneDollarCount ∷ Int
    , fiveDollarCount ∷ Int
    , twentyDollarCount ∷ Int
    }

usdBag ∷ Int → Int → Int → Int → Int → Int → USDBag
usdBag oneCentCount tenCentCount quarterCount oneDollarCount fiveDollarCount twentyDollarCount =
    USDBag 
        {  oneCentCount, tenCentCount, quarterCount, oneDollarCount, fiveDollarCount, twentyDollarCount }

derive instance eqUSDBag ∷ Eq USDBag

instance semiringUSDBag ∷ Semiring USDBag where
    add (USDBag a) (USDBag b) = 
        USDBag 
            { oneCentCount: a.oneCentCount `add` b.oneCentCount
            , tenCentCount: a.tenCentCount `add` b.tenCentCount
            , quarterCount: a.quarterCount `add` b.quarterCount
            , oneDollarCount: a.oneDollarCount `add` b.oneDollarCount
            , fiveDollarCount: a.fiveDollarCount `add` b.fiveDollarCount
            , twentyDollarCount: a.twentyDollarCount `add` b.twentyDollarCount
            }

    zero = 
        USDBag
            { oneCentCount: zero
            , tenCentCount: zero
            , quarterCount: zero
            , oneDollarCount: zero
            , fiveDollarCount: zero
            , twentyDollarCount: zero
            }

    mul (USDBag a) (USDBag b) = 
        USDBag
            { oneCentCount: a.oneCentCount `mul` b.oneCentCount
            , tenCentCount: a.tenCentCount `mul` b.tenCentCount
            , quarterCount: a.quarterCount `mul` b.quarterCount
            , oneDollarCount: a.oneDollarCount `mul` b.oneDollarCount
            , fiveDollarCount: a.fiveDollarCount `mul` b.fiveDollarCount
            , twentyDollarCount: a.twentyDollarCount `mul` b.twentyDollarCount
            }

    one = 
        USDBag
            { oneCentCount: one
            , tenCentCount: one
            , quarterCount: one
            , oneDollarCount: one
            , fiveDollarCount: one
            , twentyDollarCount: one
            }

instance ringUSDBag ∷ Ring USDBag where
    sub (USDBag a) (USDBag b) = 
        USDBag
            { oneCentCount: a.oneCentCount `sub` b.oneCentCount
            , tenCentCount: a.tenCentCount `sub` b.tenCentCount
            , quarterCount: a.quarterCount `sub` b.quarterCount
            , oneDollarCount: a.oneDollarCount `sub` b.oneDollarCount
            , fiveDollarCount: a.fiveDollarCount `sub` b.fiveDollarCount
            , twentyDollarCount: a.twentyDollarCount `sub` b.twentyDollarCount
            }

instance arbitraryUSDBag ∷ Arbitrary USDBag where
    arbitrary = 
        usdBag 
            <$> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100
            <*> chooseInt 0 100