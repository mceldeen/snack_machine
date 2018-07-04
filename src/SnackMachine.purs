module SnackMachine 
  ( SnackMachine
  , SnackMachineError(..)
  , snackMachine
  , insertMoney
  , coinsInTransaction
  , coinsInMachine
  , returnMoney
  , safeReturnMoney
  , stockSnack
  , listSnacks
  , Snack(..)
  ) where

import CoinConversion (class CoinConversion, convertToValue)
import Data.Bifunctor (bimap)
import Data.Either (Either, fromRight)
import Data.Eq (class Eq, (==))
import Data.Foldable (elem)
import Data.Monoid ((<>))
import Data.Ring (class Ring, zero, (+), (-))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..), fst)
import Money.CoinSet (class CoinSet, MakeChangeError(..), makeChange)
import Partial.Unsafe (unsafePartial)
import Prelude (const, map, ($))

newtype SnackMachine c v =
  SnackMachine
    { inside ∷ c
    , transaction ∷ v
    , snacks ∷ (Array (Tuple (Snack v) Int))
    }

data SnackMachineError
  = CannotInsertMultipleCoinsAtOnce
  | ImpossibleToMakeChange Int

derive instance snackMachineErrorEq ∷ Eq SnackMachineError

instance snackMachineErrorShow ∷ Show SnackMachineError where
  show CannotInsertMultipleCoinsAtOnce = "CannotInsertMultipleCoinsAtOnce"
  show (ImpossibleToMakeChange r) = "ImpossibleToMakeChange " <> show r

snackMachine ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ c → v → Either SnackMachineError (SnackMachine c v)
snackMachine inside transaction =
  let
    newSnackMachine = const $ SnackMachine { inside, transaction, snacks: [] }

    convertMakeChangeToSnackMachineError (CannotMakeChange r) = ImpossibleToMakeChange r
  in
    bimap convertMakeChangeToSnackMachineError newSnackMachine (makeChange inside transaction)

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

newtype Snack v = Snack { name ∷ String, unitPrice ∷ v }

derive instance snackEq ∷ Eq v ⇒ Eq (Snack v)

instance snackShow ∷ Show v ⇒ Show (Snack v) where
  show (Snack s) =
    "{ name: \"" <> show s.name <> "\", unitPrice: " <> show s.unitPrice <> " }"

stockSnack ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ Eq v ⇒ Snack v → Int → SnackMachine c v → SnackMachine c v
stockSnack snack quantity (SnackMachine state) =
  let
    existingSnacks = state.snacks

    updateExistingSnack (Tuple existingSnack existingQuantity) =
      if existingSnack == snack then
        Tuple existingSnack (existingQuantity + quantity)
      else
        Tuple existingSnack existingQuantity

    newSnacks =
      if elem (Tuple snack quantity) existingSnacks then
        map updateExistingSnack existingSnacks
      else
        existingSnacks <> [Tuple snack quantity]
  in
    SnackMachine $ state { snacks = newSnacks }

listSnacks ∷ ∀ c v. CoinSet c ⇒ CoinConversion v c ⇒ SnackMachine c v → Array (Snack v)
listSnacks (SnackMachine state) = map fst state.snacks