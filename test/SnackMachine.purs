module Test.SnackMachine where

import Prelude

import SnackMachine (SnackMachine, SnackMachineError(..), coinsInTransaction, coinsInMachine, empty, insertMoney)
import Test.ExpectationHelpers (expectFail, expectSucceed)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

testSnackMachine = empty ∷ SnackMachine Int Int

main ∷ ∀ e. TestSuite e
main =
  suite "SnackMachine" do
    suite "insertMoney" do
      test "fails when you do not insert a single coin" do
        expectFail (insertMoney testSnackMachine 2) \err →
          Assert.equal CannotInsertMultipleCoinsAtOnce err

      suite "when you insert a single coin" do
        test "shows the value in the transaction" do
          expectSucceed (insertMoney testSnackMachine 1) \snackMachine →
            Assert.equal 1 (coinsInTransaction snackMachine)

        test "shows the value inside the machine" do
          expectSucceed (insertMoney testSnackMachine 1) \snackMachine →
            Assert.equal 1 (coinsInMachine snackMachine)
