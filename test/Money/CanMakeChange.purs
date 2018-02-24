module Test.Money.CanMakeChange where

import Prelude

import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Money.CanMakeChange (makeChange)
import Money.USD (cents, fiveDollarCount, oneCentCount, oneDollarCount, quarterCount, tenCentCount, twentyDollarCount)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert

main :: ∀ e. TestSuite e
main =
  suite "Money.CanMakeChange" do 
    suite "CanMakeChange USDWallet USD" do
      test "allocates bills from smallest to largest" do
        case result of
          Left err → failure $ "unexpected error " <> show err
          Right change → do
            Assert.equal 16 (change ^. oneCentCount)
            Assert.equal 5  (change ^. tenCentCount)
            Assert.equal 4  (change ^. quarterCount)
            Assert.equal 3  (change ^. oneDollarCount)
            Assert.equal 2  (change ^. fiveDollarCount)
            Assert.equal 1  (change ^. twentyDollarCount)
        where
          result = makeChange wallet (cents .~ 3466 $ zero)

          wallet = zero # oneCentCount      .~ 26
                        # tenCentCount      .~ 5
                        # quarterCount      .~ 4
                        # oneDollarCount    .~ 3
                        # fiveDollarCount   .~ 2
                        # twentyDollarCount .~ 1
