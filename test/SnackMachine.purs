module Test.SnackMachine where

import Prelude

import Data.Either (Either(..))
import SnackMachine (SnackMachine, SnackMachineError(..), coinsInTransaction, coinsInMachine, empty, insertMoney)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert

main :: ∀ e. TestSuite e
main =
  suite "SnackMachine" do
    let testSnackMachine = empty ∷ SnackMachine Int Int
    suite "insertMoney" do
      test "fails when you do not insert a single coin"
        let
          invalidCoins = 2
          insertionResult = insertMoney testSnackMachine invalidCoins
        in do
          case insertionResult of
            Right _ → failure "unexpected success"
            Left err → Assert.equal CannotInsertMultipleCoinsAtOnce err

      suite "when you insert a single coin" do
        test "shows the value in the transaction"
          let
            singleCoin = 1
            insertionResult = insertMoney testSnackMachine singleCoin
          in do
            case insertionResult of
              Left err → failure $ "unexpected error " <> show err
              Right snackMachine → Assert.equal 1 (coinsInTransaction snackMachine)

        test "shows the value inside the machine"
          let
            singleCoin = 1
            insertionResult = insertMoney testSnackMachine singleCoin
          in do
            case insertionResult of
              Left err → failure $ "unexpected error " <> show err
              Right snackMachine → Assert.equal 1 (coinsInMachine snackMachine)
