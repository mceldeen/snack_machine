module SnackMachine 
  ( SnackMachine
  , SnackMachineError(..)
  , empty
  , insertMoney
  ) where

import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Show (class Show)
import Money.CoinSet (class CoinSet, isSingleCoin)

newtype SnackMachine = SnackMachine { }

data SnackMachineError = CannotInsertMultipleCoinsAtOnce

derive instance snackMachineErrorEq ∷ Eq SnackMachineError

instance snackMachineErrorShow ∷ Show SnackMachineError where
  show CannotInsertMultipleCoinsAtOnce = "CannotInsertMultipleCoinsAtOnce"


empty ∷ SnackMachine
empty = SnackMachine { }

insertMoney ∷ ∀ c. CoinSet c ⇒ SnackMachine → c → Either SnackMachineError SnackMachine
insertMoney machine coin =
  if isSingleCoin coin then
    Right machine
  else
    Left CannotInsertMultipleCoinsAtOnce
    
