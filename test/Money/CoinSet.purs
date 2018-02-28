module Test.Money.CoinSet where

import Prelude

import Data.Lens ((.~), (^.))
import Money.CoinSet (MakeChangeError(..), isSingleCoin, makeChange)
import Money.USD (cents)
import Money.USDSet (fiveDollarBills, pennies, oneDollarBills, quarters, dimes, twentyDollarBills)
import Test.ExpectationHelpers (expectFail, expectSucceed)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

main :: ∀ e. TestSuite e
main =
  suite "Money.CoinSet" do 
    suite "CoinSet USDSet" do
      suite "makeChange" do
        test "allocates coins from smallest to largest"
          let
            result = makeChange moneySet (zero # cents .~ 3466)
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
            result = makeChange moneySet (zero # cents .~ 1000)
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
