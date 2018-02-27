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
      test "fails when you do not insert a single coin" do
        case insertMoney testSnackMachine 2 of
          Right _ → failure "unexpected success"
          Left err → Assert.equal CannotInsertMultipleCoinsAtOnce err

      suite "when you insert a single coin" do
        test "shows the value in the transaction" do
          case insertMoney testSnackMachine 1 of
            Left err → failure $ "unexpected error " <> show err
            Right snackMachine → Assert.equal 1 (coinsInTransaction snackMachine)

        test "shows the value inside the machine" do
          case insertMoney testSnackMachine 1 of
            Left err → failure $ "unexpected error " <> show err
            Right snackMachine → Assert.equal 1 (coinsInMachine snackMachine)
