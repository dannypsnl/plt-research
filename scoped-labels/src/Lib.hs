module Lib () where

import Control.Monad.Exception.Synchronous

data Type
  = TyRecord [(String, Type)]
  | TyTuple [Type]
  | TyString
  | TyInt

data Term
  = TmRecord [(String, Term)]
  | TmTuple [Term]
  | TmString String
  | TmInt Integer

data TypeError = TypeMismatched Type Type

check :: Type -> Term -> Exceptional TypeError ()
check (TyRecord s) (TmRecord ss) = Success ()
-- FIXME: check tuple
check (TyTuple types) (TmTuple terms) = Success ()
check TyString (TmString _) = Success ()
check TyInt (TmInt _) = Success ()
check ty tm = throw $ TypeMismatched ty (typeof tm)

typeof :: Term -> Type
typeof (TmRecord terms) = TyRecord (map (\(s, term) -> (s, typeof term)) terms)
typeof (TmTuple terms) = TyTuple (map typeof terms)
typeof (TmString _) = TyString
typeof (TmInt _) = TyInt
