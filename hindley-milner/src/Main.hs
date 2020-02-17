module Main where
import Lang (Expr(..), Op(..))
import Parser (expr)

import Control.Monad.State (State, evalState, get, put)
import Data.Map (Map, map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Parsec (parse)

main :: IO ()
main = do
  (input:_) <- getArgs
  case parse expr "hm-lang" input of
    Left err -> print err
    Right val -> print $ runInfer $ infer emptyTypeEnv val

instance Show Type where
  show (TPrimitive name) = name
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (TypeVar name) = name

data Type =
  TPrimitive String
  | TArrow Type Type
  | TypeVar String
  deriving (Eq)

newtype TypeError = TypeError String deriving (Show)
-- Unique is standing for storing new type variable in environment
newtype Unique = Unique {count :: Int}

type SubstMap = [Type -> Type]

type Env a = (State Unique) a

runInfer :: Env (Type, SubstMap) -> Type
runInfer m = let (typ, _) = evalState m (Unique {count = 0}) in typ

-- consistent renaming type variables
fresh :: Env Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  return $ TypeVar (supply !! count s)
  where
    supply :: [String]
    supply = [replicate cnt v | cnt <- [1..], v <- ['a'..'z']]

newtype TypeEnv = TypeEnv (Map String Type)
  deriving (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv Map.empty

bindType :: String -> Type -> TypeEnv -> TypeEnv
bindType name typ (TypeEnv env) = TypeEnv (Map.insert name typ env)

getType :: String -> TypeEnv -> Type
getType name (TypeEnv env) = fromMaybe raiseError $ Map.lookup name env
  where raiseError = error $ name ++ " is not defined"

emptySubst :: SubstMap
emptySubst = []

addSubst :: Type -> Type -> SubstMap -> SubstMap
addSubst fromType toType substs = sub : substs
  where sub t = if t == fromType then toType else fromType

substType :: SubstMap -> Type -> Type
substType substs fromType = foldr (\sub t -> sub t) fromType substs

substEnv :: SubstMap -> TypeEnv -> TypeEnv
substEnv substs (TypeEnv env) = TypeEnv (foldr Data.Map.map env substs)

subst :: SubstMap -> Type -> Type
subst _ t@(TPrimitive _) = t
subst sub (TArrow t1 t2) = TArrow (subst sub t1) (subst sub t2)
subst sub v@(TypeVar _) = substType sub v

unify :: Type -> Type -> Env SubstMap
unify t@(TPrimitive t1) (TPrimitive t2) =
  if t1 == t2
  then return emptySubst
  else error $ "unable to unify type: " ++ show t1 ++ " and " ++ show t2 ++ ""
unify (TArrow t1 t2) (TArrow t1' t2') = do
  subst1 <- unify t1 t1'
  subst2 <- unify (substType subst1 t2) (substType subst1 t2')
  return $ subst2 ++ subst1
unify t v@(TypeVar _) =
  if v == t || not (v `occurs` t)
  then return $ addSubst v t emptySubst
  else return emptySubst
unify v@(TypeVar _) t = unify t v
unify t1 t2 = error $ "unable to unify type: " ++ show t1 ++ " and " ++ show t2 ++ ""

occurs :: Type -> Type -> Bool
occurs _ (TPrimitive _) = False
occurs v (TArrow t1 t2) = occurs v t1 || occurs v t2
occurs v t@(TypeVar _) = v == t

-- Now, let's infer!
infer :: TypeEnv -> Expr -> Env (Type, SubstMap)
infer _ (Int _) = return (TPrimitive "int", emptySubst)
infer _ (Bool _) = return (TPrimitive "bool", emptySubst)
infer _ (String _) = return (TPrimitive "string", emptySubst)
infer env (Var x) = return (getType x env, emptySubst)
infer env (Func x expr) = do
  freeVar <- fresh
  -- bind parameter with freeVar
  -- then infer expression of function
  (t, subst) <- infer (bindType x freeVar env) expr
  return (TArrow (substType subst freeVar) t, subst)
infer env (Application lambda argument) = do
  (arrowT, substOfLambda) <- infer env lambda
  (argT, substOfArg) <- infer (substEnv substOfLambda env) argument
  freeVar <- fresh
  subst' <- unify (substType substOfArg arrowT) (TArrow argT freeVar)
  return (substType subst' freeVar, subst' ++ substOfArg ++ substOfLambda)
infer env (Let x e1 e2) = do
  (t1, s1) <- infer env e1
  (t2, s2) <- infer (bindType x t1 env) e2
  return (t2, s2 ++ s1)
infer env (Binary op e1 e2) = do
  (t1, s1) <- infer env e1
  (t2, s2) <- infer (substEnv s1 env) e2
  s3       <- unify (substType s2 t1) (leftOperandType op)
  s4       <- unify (substType s3 t2) (rightOperandType op)
  return (opType op, s4 ++ s3 ++ s2 ++ s1)

opType :: Op -> Type
opType op | op `elem` [Equal, NotEqual] = TPrimitive "bool"
          | op `elem` [Add, Sub, Div, Mul] = TPrimitive "int"
          | otherwise = error $ "Unknown operator" ++ show op

leftOperandType :: t -> Type
leftOperandType _ = TPrimitive "int"

rightOperandType :: t -> Type
rightOperandType _ = TPrimitive "int"

inferExpr :: TypeEnv -> Expr -> Type
inferExpr env = runInfer . infer env
