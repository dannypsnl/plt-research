module Main (main) where

import Control.Monad
import Data.Maybe
import Parser
import Syntax
import System.Environment
import Text.Megaparsec
import Text.Printf

-- type checking
--------------------------------------------------------------------------------

data Val
  = VVar Name
  | VApp Val ~Val
  | VLam Name (Val -> Val)
  | VPi Name Val (Val -> Val)
  | VU

type Env = [(Name, Val)]

-- NOTE: a [Name] suffices for getting fresh names, but it's more convenient
--   here to use an Env, because this way we don't have to constantly unzip the
--   names from the values. Alternatively, we could store the names and types in
--   unzipped form to begin with, but that'd make lookup less neat.
fresh :: Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookup x env of
  Just _ -> fresh env (x ++ "'")
  _ -> x

eval :: Env -> Tm -> Val
eval env = \case
  Var x -> fromJust $ lookup x env
  App t u -> case (eval env t, eval env u) of
    (VLam _ t, u) -> t u
    (t, u) -> VApp t u
  Lam x t -> VLam x (\u -> eval ((x, u) : env) t)
  Pi x a b -> VPi x (eval env a) (\u -> eval ((x, u) : env) b)
  Let x _ t u -> eval ((x, eval env t) : env) u
  Postulate x _ u -> eval ((x, VVar x) : env) u
  U -> VU
  SrcPos _ t -> eval env t

quote :: Env -> Val -> Tm
quote env = \case
  VVar x -> Var x
  VApp t u -> App (quote env t) (quote env u)
  VLam (fresh env -> x) t -> Lam x (quote ((x, VVar x) : env) (t (VVar x)))
  VPi (fresh env -> x) a b -> Pi x (quote env a) (quote ((x, VVar x) : env) (b (VVar x)))
  VU -> U

nf :: Env -> Tm -> Tm
nf env = quote env . eval env

nf0 :: Tm -> Tm
nf0 = nf []

-- | Beta-eta conversion checking
conv :: Env -> Val -> Val -> Bool
conv env t u = case (t, u) of
  (VU, VU) -> True
  (VPi (fresh env -> x) a b, VPi x' a' b') ->
    conv env a a' && conv ((x, VVar x) : env) (b (VVar x)) (b' (VVar x))
  (VLam (fresh env -> x) t, VLam x' t') ->
    conv ((x, VVar x) : env) (t (VVar x)) (t' (VVar x))
  -- checking eta conversion for Lam
  (VLam (fresh env -> x) t, u) ->
    conv ((x, VVar x) : env) (t (VVar x)) (VApp u (VVar x))
  (u, VLam (fresh env -> x) t) ->
    conv ((x, VVar x) : env) (VApp u (VVar x)) (t (VVar x))
  (VVar x, VVar x') -> x == x'
  (VApp t u, VApp t' u') -> conv env t t' && conv env u u'
  _ -> False

type VTy = Val

-- | Typing context.
type Cxt = [(Name, VTy)]

-- | Typechecking monad. After we throw an error, we annotate it at the innermost
--   point in the syntax where source position information is available from
--   a 'SrcPos' 'Tm' constructor.
type M = Either (String, Maybe SourcePos)

report :: String -> M a
report str = Left (str, Nothing)

quoteShow :: Env -> Val -> String
quoteShow env = show . quote env

addPos :: SourcePos -> M a -> M a
addPos pos ma = case ma of
  Left (msg, Nothing) -> Left (msg, Just pos)
  ma' -> ma'

check :: Env -> Cxt -> Raw -> VTy -> M ()
check env cxt t a = case (t, a) of
  (SrcPos pos t, _) -> addPos pos (check env cxt t a)
  (Lam x t, VPi (fresh env -> x') a b) ->
    check ((x, VVar x') : env) ((x, a) : cxt) t (b (VVar x'))
  (Let x a' t' u, _) -> do
    check env cxt a' VU
    let a'' = eval env a'
    check env cxt t' a''
    check ((x, eval env t') : env) ((x, a'') : cxt) u a
  _ -> do
    tty <- infer env cxt t
    unless (conv env tty a) $
      report
        ( printf
            "type mismatch\n\nexpected type:\n\n  %s\n\ninferred type:\n\n  %s\n"
            (quoteShow env a)
            (quoteShow env tty)
        )

infer :: Env -> Cxt -> Raw -> M VTy
infer env cxt = \case
  SrcPos pos t -> addPos pos (infer env cxt t)
  Var x -> case lookup x cxt of
    Nothing -> report $ "Name not in scope: " ++ x
    Just a -> pure a
  U -> pure VU
  App t u -> do
    tty <- infer env cxt t
    case tty of
      VPi _ a b -> do
        check env cxt u a
        pure (b (eval env u))
      v ->
        report $
          "Expected a function type, instead inferred:\n\n  "
            ++ quoteShow env v
  Lam {} -> report "Can't infer type for lambda expresion"
  Pi x a b -> do
    check env cxt a VU
    check ((x, VVar x) : env) ((x, eval env a) : cxt) b VU
    pure VU
  Postulate x a u -> do
    check env cxt a VU
    let a' = eval env a
    infer ((x, VVar x) : env) ((x, a') : cxt) u
  Let x a t u -> do
    check env cxt a VU
    let a' = eval env a
    check env cxt t a'
    infer ((x, eval env t) : env) ((x, a') : cxt) u

-- main
--------------------------------------------------------------------------------

displayError :: String -> (String, Maybe SourcePos) -> IO ()
displayError file (msg, Just (SourcePos path (unPos -> linum) (unPos -> colnum))) = do
  let lnum = show linum
      lpad = map (const ' ') lnum
  printf "%s:%d:%d:\n" path linum colnum
  printf "%s |\n" lpad
  printf "%s | %s\n" lnum (lines file !! (linum - 1))
  printf "%s | %s\n" lpad (replicate (colnum - 1) ' ' ++ "^")
  printf "%s\n" msg
displayError _ _ = error "displayError: impossible: no available source position"

main :: IO ()
main = do
  [file] <- getArgs
  content <- readFile file
  case parse pSrc file content of
    Left bundle -> putStrLn $ errorBundlePretty bundle
    Right tm -> do
      case infer [] [] tm of
        Left err -> displayError file err
        Right a -> do
          print $ nf0 tm
          putStrLn "  :"
          print $ quote [] a
