module ScopedLabel () where

import Control.Monad
import Control.Monad.Catch
import qualified Data.Map.Strict as M

data Type
  = TyRecord (M.Map String Type)
  | TyTuple [Type]
  | TyString
  | TyInt
  deriving (Show)

data Term
  = TmRecord (M.Map String Term)
  | TmTuple [Term]
  | TmString String
  | TmInt Integer

data TypeError = TypeMismatched Type Type
  deriving (Show)

instance Exception TypeError

check :: MonadThrow m => Type -> Term -> m ()
check (TyRecord types) (TmRecord terms) = do
  -- TODO: check record content
  return ()
check (TyTuple types) (TmTuple terms) = do
  es <- zipWithM check types terms
  return ()
check TyString (TmString _) = return ()
check TyInt (TmInt _) = return ()
check ty tm = throwM $ TypeMismatched ty (typeof tm)

typeof :: Term -> Type
typeof (TmRecord terms) = TyRecord (M.map typeof terms)
typeof (TmTuple terms) = TyTuple (map typeof terms)
typeof (TmString _) = TyString
typeof (TmInt _) = TyInt
