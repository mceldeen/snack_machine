module Test.SnackMachine where

import Prelude

import CoinConversion (class CoinConversion)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (Either(..), either, fromRight)
import Data.Tuple (Tuple(..))
import Money.CoinSet (class CoinSet)
import Money.USD (USD)
import Money.USDSet (USDSet)
import Partial.Unsafe (unsafePartial)
import SnackMachine
import Test.QuickCheck (Result(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)

testSnackMachine ∷ SnackMachine Int Int
testSnackMachine =
  unsafePartial $ fromRight (snackMachine 0 0)

snackMachineWithCoinsInTransaction ∷ SnackMachine Int Int
snackMachineWithCoinsInTransaction =
  insertMoney 2 testSnackMachine

checkCanAlwaysReturnMoney :: ∀ c v. Show c ⇒ Show v ⇒ CoinSet c ⇒ CoinConversion v c ⇒ c → v → c → Result
checkCanAlwaysReturnMoney inside transaction insertedMoney =
  let
    reportFailure e =
      Failed $ show e <> " " <> show inside <> " " <> show transaction <> " " <> show insertedMoney
  in
    case insertMoney insertedMoney <$> snackMachine inside transaction of
      Right sm →
        either reportFailure (const Success) $ safeReturnMoney sm
      Left _ →
        Success

main ∷ ∀ e. TestSuite ( random ∷ RANDOM | e)
main =
  suite "SnackMachine" do
    test "cannot ever get into a state where it cannot return the money with USD" do
      quickCheck $ (checkCanAlwaysReturnMoney ∷ USDSet → USD → USDSet → Result)

    test "cannot ever get into a state where it cannot return the money with Int" do
      quickCheck $ (checkCanAlwaysReturnMoney ∷ Int → Int → Int → Result)

    suite "insertMoney" do
      test "shows the value in the transaction" do
        Assert.equal 1 (coinsInTransaction $ insertMoney 1 testSnackMachine)

      test "shows the value inside the machine" do
        Assert.equal 1 (coinsInMachine $ insertMoney 1 testSnackMachine)

    suite "returnCoins" do
      test "returns proper change" do
        case (returnMoney snackMachineWithCoinsInTransaction) of
          Tuple _ change → do
            Assert.equal 2 change
      test "removes the coins from the transaction" do
        case (returnMoney snackMachineWithCoinsInTransaction) of
          Tuple machine _ → do
            Assert.equal zero (coinsInTransaction machine)
      test "removes the coins from the inside" do
        case (returnMoney snackMachineWithCoinsInTransaction) of
          Tuple machine _ → do
            Assert.equal zero (coinsInMachine machine)

    suite "stockSnack" do
      test "shows the snack as in stock"
        let
          snack =
            Snack {name: "candy bar", unitPrice: 150}
          machineWithSnack =
            stockSnack snack 10 testSnackMachine
        in do
          Assert.equal (listSnacks machineWithSnack) [snack]

      test "adds to existing snacks"
          let
            snack =
              Snack {name: "candy bar", unitPrice: 150}
            machineWithSnack =
              stockSnack snack 10 $ stockSnack snack 10 testSnackMachine
          in do
            Assert.equal (listSnacks machineWithSnack) [snack]
