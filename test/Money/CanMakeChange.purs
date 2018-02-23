module Test.Money.CanMakeChange where

import Prelude

import Data.Either (Either(..))
import Data.Lens (set, view)
import Money.CanMakeChange (makeChange)
import Money.USD (cents, fiveDollarCount, oneCentCount, oneDollarCount, quarterCount, tenCentCount, twentyDollarCount, usdWallet)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert

main :: ∀ e. TestSuite e
main =
  suite "Money.CanMakeChange" do 
    suite "CanMakeChange USDWallet USD" do
      test "allocates bills from smallest to largest" do
        let inventory = usdWallet 26 5 4 3 2 1
        let result = makeChange inventory (set cents 3466 zero)
        case result of
          Left err → failure $ "unexpected error " <> show err
          Right change → do
            Assert.equal 16 (view oneCentCount change)
            Assert.equal 5 (view tenCentCount change)
            Assert.equal 4 (view quarterCount change)
            Assert.equal 3 (view oneDollarCount change)
            Assert.equal 2 (view fiveDollarCount change)
            Assert.equal 1 (view twentyDollarCount change)
