module Test.Money.CoinSet where

import Prelude

import CoinConversion (convertToValue)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Money.CoinSet (MakeChangeError(CannotMakeChange), isSingleCoin, makeChange)
import Money.USD (cents, fromCents)
import Money.USDSet (USDSet, dimes, fiveDollarBills, oneDollarBills, pennies, quarters, twentyDollarBills)
import Test.ExpectationHelpers (expectFail, expectSucceed)
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)

canMakeChangeUSDSet ∷ USDSet → USDSet → Gen Result
canMakeChangeUSDSet a b = do
  c ← fromCents <$> chooseInt 1 (convertToValue b ^. cents)
  case convertToValue <$> makeChange (a + b) c == Right c of
    true → pure Success
    false → pure $ Failed $ "a = " <> show a <> ", b = " <> show b <> ", c = " <> show c

main ∷ ∀ e. TestSuite ( random ∷ RANDOM | e)
main =
  suite "Money.CoinSet" do 
    suite "CoinSet USDSet" do
      suite "makeChange" do
        test "can always make change under the right circumstances" $ quickCheck canMakeChangeUSDSet

        test "allocates coins from smallest to largest"
          let
            result = makeChange moneySet (fromCents 3466)
            moneySet = zero # pennies           .~ 26
                            # dimes             .~ 5
                            # quarters          .~ 4
                            # oneDollarBills    .~ 3
                            # fiveDollarBills   .~ 2
                            # twentyDollarBills .~ 1
          in
            expectSucceed result \change → do
                Assert.equal 16 (change ^. pennies)
                Assert.equal 5  (change ^. dimes)
                Assert.equal 4  (change ^. quarters)
                Assert.equal 3  (change ^. oneDollarBills)
                Assert.equal 2  (change ^. fiveDollarBills)
                Assert.equal 1  (change ^. twentyDollarBills)

        test "fails when it cannot make exact change"
          let
            result = makeChange moneySet (fromCents 1000)
            moneySet = zero # twentyDollarBills .~ 1
          in
            expectFail result \err → Assert.equal (CannotMakeChange 1000) err

      suite "isSingleCoin" do
        test "returns true when there is only a single instance of a single denomination" do
          Assert.equal true $ isSingleCoin (zero # pennies .~ 1)
        test "returns false when there are multiple instances of a single denomination" do
          Assert.equal false $ isSingleCoin (zero # pennies .~ 2)
        test "returns false when there are multiple denominations" do
          Assert.equal false $ isSingleCoin (zero # pennies .~ 1 # quarters .~ 1)
