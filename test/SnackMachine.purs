module Test.SnackMachine where

import Prelude

import Data.Either (Either(..))
import SnackMachine (SnackMachine, SnackMachineError(..), coinsInTransaction, coinsInMachine, empty, insertMoney)
import Test.Unit (TestSuite, Test, failure, suite, test)
import Test.Unit.Assert as Assert

testSnackMachine = empty ∷ SnackMachine Int Int

expectLeft ∷ ∀ v err e. Either err v → (err → Test e) → Test e
expectLeft r f =
  case r of
    Right _ → failure "unexpected success"
    Left l → f l

expectRight ∷ ∀ v err e. Show err ⇒ Either err v → (v → Test e) → Test e
expectRight r f =
  case r of
    Left err → failure $ "unexpected error " <> show err
    Right v → f v

main ∷ ∀ e. TestSuite e
main =
  suite "SnackMachine" do
    suite "insertMoney" do
      test "fails when you do not insert a single coin" do
        expectLeft (insertMoney testSnackMachine 2) \err →
          Assert.equal CannotInsertMultipleCoinsAtOnce err

      suite "when you insert a single coin" do
        test "shows the value in the transaction" do
          expectRight (insertMoney testSnackMachine 1) \snackMachine →
            Assert.equal 1 (coinsInTransaction snackMachine)

        test "shows the value inside the machine" do
          expectRight (insertMoney testSnackMachine 1) \snackMachine →
            Assert.equal 1 (coinsInMachine snackMachine)
