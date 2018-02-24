module Test.Money.CanMakeChange where

import Prelude

import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Money.CanMakeChange (MakeChangeError(..), makeChange)
import Money.USD (cents)
import Money.USDWallet
  ( fiveDollarCount
  , oneCentCount
  , oneDollarCount
  , quarterCount
  , tenCentCount
  , twentyDollarCount
  )
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert

main :: ∀ e. TestSuite e
main =
  suite "Money.CanMakeChange" do 
    suite "CanMakeChange USDWallet USD" do
      test "allocates bills from smallest to largest"
        let
          result = makeChange moneySet (zero # cents .~ 3466)

          moneySet = zero # oneCentCount      .~ 26
                        # tenCentCount      .~ 5
                        # quarterCount      .~ 4
                        # oneDollarCount    .~ 3
                        # fiveDollarCount   .~ 2
                        # twentyDollarCount .~ 1
        in do
          case result of
            Left err → failure $ "unexpected error " <> show err
            Right change → do
              Assert.equal 16 (change ^. oneCentCount)
              Assert.equal 5  (change ^. tenCentCount)
              Assert.equal 4  (change ^. quarterCount)
              Assert.equal 3  (change ^. oneDollarCount)
              Assert.equal 2  (change ^. fiveDollarCount)
              Assert.equal 1  (change ^. twentyDollarCount)

      test "fails when it cannot make exact change"
        let
          result = makeChange moneySet (zero # cents .~ 1000)
          moneySet = zero # twentyDollarCount .~ 1
        in do
          case result of
            Left err → Assert.equal CannotMakeChange err
            Right _ → failure "unexpectedly able to make change"
