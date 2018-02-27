module SnackMachine 
  ( SnackMachine
  , SnackMachineError(..)
  , empty
  , insertMoney
  , coinsInTransaction
  , coinsInMachine
  ) where

import CoinConversion (class CoinConversion, convertToValue)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Ring (class Ring, zero, (+))
import Data.Show (class Show)
import Money.CoinSet (class CoinSet, isSingleCoin)
import Prelude (($))

newtype SnackMachine c v = SnackMachine { inside ∷ c, transaction ∷ v }

data SnackMachineError = CannotInsertMultipleCoinsAtOnce

derive instance snackMachineErrorEq ∷ Eq SnackMachineError

instance snackMachineErrorShow ∷ Show SnackMachineError where
  show CannotInsertMultipleCoinsAtOnce = "CannotInsertMultipleCoinsAtOnce"


empty ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ SnackMachine c v
empty = SnackMachine { inside: zero, transaction: zero }

insertMoney ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ SnackMachine c v → c → Either SnackMachineError (SnackMachine c v)
insertMoney (SnackMachine state) coin =
  if isSingleCoin coin then
    Right $ SnackMachine $ state
      { inside = state.inside + coin
      , transaction = state.transaction + convertToValue coin }
  else
    Left CannotInsertMultipleCoinsAtOnce

coinsInTransaction ∷ ∀ c v. CoinSet c ⇒ Ring v ⇒ SnackMachine c v → v
coinsInTransaction (SnackMachine { transaction }) = transaction

coinsInMachine ∷ ∀ c v. CoinSet c ⇒ Ring v ⇒ SnackMachine c v → c
coinsInMachine (SnackMachine { inside }) = inside
