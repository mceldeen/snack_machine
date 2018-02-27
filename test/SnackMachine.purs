module Test.SnackMachine where

import Data.Either (Either(..))
import SnackMachine (SnackMachineError(..))
import SnackMachine as SnackMachine
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert

main :: ∀ e. TestSuite e
main =
  suite "SnackMachine" do 
    suite "insertMoney" do
      test "fails when you do not insert "
        let
          invalidCoins = 2
          insertionResult = SnackMachine.insertMoney SnackMachine.empty invalidCoins
        in do
          case insertionResult of
            Right _ → failure "unexpected success"
            Left err → Assert.equal CannotInsertMultipleCoinsAtOnce err


