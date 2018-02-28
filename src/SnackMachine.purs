module SnackMachine 
  ( SnackMachine
  , SnackMachineError(..)
  , snackMachine
  , insertMoney
  , coinsInTransaction
  , coinsInMachine
  , returnMoney
  , safeReturnMoney
  ) where

import CoinConversion (class CoinConversion, convertToValue)
import Data.Bifunctor (bimap)
import Data.Either (Either, fromRight)
import Data.Eq (class Eq)
import Data.Monoid ((<>))
import Data.Ring (class Ring, zero, (+), (-))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Money.CoinSet (class CoinSet, MakeChangeError(..), makeChange)
import Partial.Unsafe (unsafePartial)
import Prelude (($), const)

newtype SnackMachine c v = SnackMachine { inside ∷ c, transaction ∷ v }

data SnackMachineError
  = CannotInsertMultipleCoinsAtOnce
  | ImpossibleToMakeChange Int

derive instance snackMachineErrorEq ∷ Eq SnackMachineError

instance snackMachineErrorShow ∷ Show SnackMachineError where
  show CannotInsertMultipleCoinsAtOnce = "CannotInsertMultipleCoinsAtOnce"
  show (ImpossibleToMakeChange r) = "ImpossibleToMakeChange " <> show r


snackMachine ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ c → v → Either SnackMachineError (SnackMachine c v)
snackMachine inside transaction =
  bimap
    convertMakeChangeToSnackMachineError
    (const $SnackMachine { inside, transaction })
    (makeChange inside transaction)
  where
    convertMakeChangeToSnackMachineError (CannotMakeChange r) = ImpossibleToMakeChange r

insertMoney ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ c → SnackMachine c v → SnackMachine c v
insertMoney coin (SnackMachine state) =
  SnackMachine $ state
      { inside = state.inside + coin
      , transaction = state.transaction + convertToValue coin }

coinsInTransaction ∷ ∀ c v. CoinSet c ⇒ Ring v ⇒ SnackMachine c v → v
coinsInTransaction (SnackMachine { transaction }) = transaction

coinsInMachine ∷ ∀ c v. CoinSet c ⇒ Ring v ⇒ SnackMachine c v → c
coinsInMachine (SnackMachine { inside }) = inside

returnMoney ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ SnackMachine c v → Tuple (SnackMachine c v) c
returnMoney machine =
    unsafePartial $ fromRight $ safeReturnMoney machine

safeReturnMoney ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ SnackMachine c v → Either SnackMachineError (Tuple (SnackMachine c v) c)
safeReturnMoney machine@(SnackMachine state@{ inside, transaction }) =
  let
    handleChange change =
      Tuple
        (SnackMachine $ state { inside = inside - change, transaction = zero })
        change
  in
    bimap (\(CannotMakeChange r) → ImpossibleToMakeChange r) handleChange $ makeChange inside transaction