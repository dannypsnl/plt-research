module ScopedLabel () where

import Control.Monad.Exception.Synchronous
import qualified Data.Map.Strict as M

data Type
  = TyRecord (M.Map String Type)
  | TyTuple [Type]
  | TyString
  | TyInt

data Term
  = TmRecord (M.Map String Term)
  | TmTuple [Term]
  | TmString String
  | TmInt Integer

data TypeError = TypeMismatched Type Type

check :: Type -> Term -> Exceptional TypeError ()
check (TyRecord s) (TmRecord ss) = Success ()
check (TyTuple types) (TmTuple terms) = zipWith check types terms
check TyString (TmString _) = Success ()
check TyInt (TmInt _) = Success ()
check ty tm = throw $ TypeMismatched ty (typeof tm)

typeof :: Term -> Type
typeof (TmRecord terms) = TyRecord (M.map typeof terms)
typeof (TmTuple terms) = TyTuple (map typeof terms)
typeof (TmString _) = TyString
typeof (TmInt _) = TyInt
