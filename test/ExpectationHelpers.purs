module Test.ExpectationHelpers where

import Data.Either (Either(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Prelude (($))
import Test.Unit (Test, failure)

expectFail ∷ ∀ v err e. Either err v → (err → Test e) → Test e
expectFail r f =
  case r of
    Right _ → failure "unexpected success"
    Left l → f l

expectSucceed ∷ ∀ v err e. Show err ⇒ Either err v → (v → Test e) → Test e
expectSucceed r f =
  case r of
    Left err → failure $ "unexpected error " <> show err
    Right v → f v