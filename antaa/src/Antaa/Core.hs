module Antaa.Core
  ( infer,
    showTm0,
    displayMetas,
    displayError,
    emptyCtx,
    showVal,
  )
where

import Antaa.Raw
import Antaa.Term
import Control.Exception hiding (try)
import Control.Monad
import Data.IORef
import Data.IntMap qualified as IM
import Text.Megaparsec
import Text.Printf

infixl 4 :>

pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- x : xs
  where
    xs :> x = x : xs

{-# COMPLETE (:>), [] #-}

pattern VVar :: Lvl -> Val
pattern VVar x = VRigid x []

pattern VMeta :: MetaVar -> Val
pattern VMeta m = VFlex m []

infixl 8 $$

-- 簡化套用 closure 的語法
($$) :: Closure -> Val -> Val
(Closure env t) $$ u = eval (env :> u) t

vApp :: Val -> Val -> Val
vApp t ~u = case t of
  VLam _ t' -> t' $$ u
  VFlex m sp -> VFlex m (sp :> u)
  VRigid x sp -> VRigid x (sp :> u)
  _ -> error "impossible"

vAppSp :: Val -> Spine -> Val
vAppSp t = \case
  [] -> t
  sp :> u -> vAppSp t sp `vApp` u

vMeta :: MetaVar -> Val
vMeta m = case lookupMeta m of
  Solved v -> v
  Unsolved -> VMeta m

vAppBDs :: Env -> Val -> [BD] -> Val
vAppBDs env ~v bds = case (env, bds) of
  ([], []) -> v
  (env' :> t, bds' :> Bound) -> vAppBDs env' v bds' `vApp` t
  (env' :> _, bds' :> Defined) -> vAppBDs env' v bds'
  _ -> error "impossible"

eval :: Env -> Tm -> Val
eval env = \case
  Var x -> env !! unIx x
  App t u -> vApp (eval env t) (eval env u)
  Lam x t -> VLam x (Closure env t)
  Pi x a b -> VPi x (eval env a) (Closure env b)
  -- 用 indicies 就不用去綁定 x := t 了
  -- 因為 index 0 就是 x
  Let _ _ t u -> eval (env :> eval env t) u
  U -> VU
  Meta m -> vMeta m
  InsertedMeta m bds -> vAppBDs env (vMeta m) bds

force :: Val -> Val
force = \case
  VFlex m sp | Solved t <- lookupMeta m -> force (vAppSp t sp)
  t -> t

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl x) = Ix (l - x - 1)

quoteSp :: Lvl -> Tm -> Spine -> Tm
quoteSp l t = \case
  [] -> t
  sp :> u -> App (quoteSp l t sp) (quote l u)

quote :: Lvl -> Val -> Tm
quote l t = case force t of
  VFlex m sp -> quoteSp l (Meta m) sp
  VRigid x sp -> quoteSp l (Var (lvl2Ix l x)) sp
  VLam x t -> Lam x (quote (l + 1) (t $$ VVar l))
  VPi x a b -> Pi x (quote l a) (quote (l + 1) (b $$ VVar l))
  VU -> U

nf :: Env -> Tm -> Tm
nf env t = quote (Lvl (length env)) (eval env t)

-- Metavariables and pattern unification
--------------------------------------------------------------------------------

freshMeta :: Ctx -> IO Tm
freshMeta ctx = do
  m <- readIORef nextMeta
  writeIORef nextMeta $! (m + 1) -- increment nextMeta
  modifyIORef' mctx $ IM.insert m Unsolved -- add a new unsolved metaEntry
  pure $ InsertedMeta (MetaVar m) (bds ctx) -- as I previously mentioned, we just drop the [BD] here
  -- Ctx has a (bds :: Ctx -> [BD]) field

-- partial renaming from Γ to Δ
data PartialRenaming = PRen
  { -- domain
    dom :: Lvl, -- size of Γ
    -- codomain
    cod :: Lvl, -- size of Δ
    ren :: IM.IntMap Lvl -- mapping from Δ vars to Γ vars
  }

-- Lifting a partial renaming over an extra bound variable.
-- Given (σ : PRen Γ Δ), (lift σ : PRen (Γ, x : A[σ]) (Δ, x : A))
lift :: PartialRenaming -> PartialRenaming
lift (PRen dom cod ren) =
  PRen (dom + 1) (cod + 1) (IM.insert (unLvl cod) dom ren)

--  invert : (Γ : Ctx) → (spine : Sub Δ Γ) → PRen Γ Δ
invert :: Lvl -> Spine -> IO PartialRenaming
invert gamma sp = do
  let go :: Spine -> IO (Lvl, IM.IntMap Lvl)
      go [] = pure (0, mempty)
      go (sp :> t) = do
        (dom, ren) <- go sp
        case force t of
          VVar (Lvl x) | IM.notMember x ren -> pure (dom + 1, IM.insert x dom ren)
          _ -> throwIO UnifyError

  (dom, ren) <- go sp
  pure $ PRen dom gamma ren

-- perform the partial renaming on rhs, while also checking for "m" occurrences.
rename :: MetaVar -> PartialRenaming -> Val -> IO Tm
rename m pren v = go pren v
  where
    goSp :: PartialRenaming -> Tm -> Spine -> IO Tm
    goSp pren t [] = pure t
    goSp pren t (sp :> u) = App <$> goSp pren t sp <*> go pren u

    go :: PartialRenaming -> Val -> IO Tm
    go pren t = case force t of
      -- flex: 靈活的
      VFlex m' sp
        | m == m' -> throwIO UnifyError -- occurs check
        | otherwise -> goSp pren (Meta m') sp
      -- rigid: 死板的
      VRigid (Lvl x) sp -> case IM.lookup x (ren pren) of
        Nothing -> throwIO UnifyError -- scope error ("escaping variable" error)
        Just x' -> goSp pren (Var $ lvl2Ix (dom pren) x') sp
      VLam x t -> Lam x <$> go (lift pren) (t $$ VVar (cod pren))
      VPi x a b -> Pi x <$> go pren a <*> go (lift pren) (b $$ VVar (cod pren))
      VU -> pure U

{-
Wrap a term in lambdas.
-}
lams :: Lvl -> Tm -> Tm
lams l = go 0
  where
    go x t | x == l = t
    go x t = Lam ("x" ++ show (x + 1)) $ go (x + 1) t

--       Γ      ?α         sp       rhs
solve :: Lvl -> MetaVar -> Spine -> Val -> IO ()
solve gamma m sp rhs = do
  pren <- invert gamma sp
  rhs <- rename m pren rhs
  let solution = eval [] $ lams (dom pren) rhs
  modifyIORef' mctx $ IM.insert (unMetaVar m) (Solved solution)

{-
The actual unification algorithm is fairly simple, it works in a structural
fashion, like conversion checking before.  The main complication, as we've seen,
is the pattern unification case.
-}

unifySp :: Lvl -> Spine -> Spine -> IO ()
unifySp l sp sp' = case (sp, sp') of
  ([], []) -> pure ()
  (sp :> t, sp' :> t') -> unifySp l sp sp' >> unify l t t'
  _ -> throwIO UnifyError -- rigid mismatch error

unify :: Lvl -> Val -> Val -> IO ()
unify l t u = case (force t, force u) of
  (VLam _ t, VLam _ t') -> unify (l + 1) (t $$ VVar l) (t' $$ VVar l)
  (t, VLam _ t') -> unify (l + 1) (t `vApp` VVar l) (t' $$ VVar l)
  (VLam _ t, t') -> unify (l + 1) (t $$ VVar l) (t' `vApp` VVar l)
  (VU, VU) -> pure ()
  (VPi x a b, VPi x' a' b') -> unify l a a' >> unify (l + 1) (b $$ VVar l) (b' $$ VVar l)
  (VRigid x sp, VRigid x' sp') | x == x' -> unifySp l sp sp'
  (VFlex m sp, VFlex m' sp') | m == m' -> unifySp l sp sp'
  (VFlex m sp, t') -> solve l m sp t'
  (t, VFlex m' sp') -> solve l m' sp' t
  _ -> throwIO UnifyError -- rigid mismatch error

-- Elaboration
--------------------------------------------------------------------------------

type Types = [(String, VTy)]

data Ctx = Ctx
  { -- used for:
    -----------------------------------
    env :: Env, -- evaluation
    lvl :: Lvl, -- unification
    types :: Types, -- raw name lookup, pretty printing
    bds :: [BD], -- fresh meta creation
    pos :: SourcePos -- error reporting
  }

instance Show Ctx where
  show = show . ctxNames

emptyCtx :: SourcePos -> Ctx
emptyCtx = Ctx [] 0 [] []

-- | Extend Ctx with a bound variable.
bind :: Ctx -> Name -> VTy -> Ctx
bind (Ctx env l types bds pos) x ~a =
  Ctx (env :> VVar l) (l + 1) (types :> (x, a)) (bds :> Bound) pos

-- | Extend Ctx with a definition.
define :: Ctx -> Name -> Val -> VTy -> Ctx
define (Ctx env l types bds pos) x ~t ~a =
  Ctx (env :> t) (l + 1) (types :> (x, a)) (bds :> Defined) pos

-- | closeVal : (Γ : Con) → Val (Γ, x : A) B → Closure Γ A B
closeVal :: Ctx -> Val -> Closure
closeVal ctx t = Closure (env ctx) (quote (lvl ctx + 1) t)

unifyCatch :: Ctx -> Val -> Val -> IO ()
unifyCatch ctx t t' =
  unify (lvl ctx) t t'
    `catch` \UnifyError ->
      throwIO $ Error ctx $ CantUnify (quote (lvl ctx) t) (quote (lvl ctx) t')

check :: Ctx -> Raw -> VTy -> IO Tm
check ctx t a = case (t, force a) of
  (RSrcPos pos t, a) ->
    check (ctx {pos = pos}) t a
  (RLam x t, VPi _ a b) ->
    Lam x <$> check (bind ctx x a) t (b $$ VVar (lvl ctx))
  (RLet x a t u, a') -> do
    a <- check ctx a VU
    let ~va = eval (env ctx) a
    t <- check ctx t va
    let ~vt = eval (env ctx) t
    u <- check (define ctx x vt va) u a'
    pure (Let x a t u)
  (RHole, a) ->
    freshMeta ctx
  (t, expected) -> do
    (t, inferred) <- infer ctx t
    unifyCatch ctx expected inferred
    pure t

infer :: Ctx -> Raw -> IO (Tm, VTy)
infer ctx = \case
  RSrcPos pos t ->
    infer (ctx {pos = pos}) t
  RVar x -> do
    let go ix (types :> (x', a))
          | x == x' = pure (Var ix, a)
          | otherwise = go (ix + 1) types
        go ix [] =
          throwIO $ Error ctx $ NameNotInScope x

    go 0 (types ctx)
  RLam x t -> do
    a <- eval (env ctx) <$> freshMeta ctx
    (t, b) <- infer (bind ctx x a) t
    pure (Lam x t, VPi x a $ closeVal ctx b)
  RApp t u -> do
    (t, tty) <- infer ctx t

    -- ensure that tty is Pi
    (a, b) <- case force tty of
      VPi x a b ->
        pure (a, b)
      tty -> do
        a <- eval (env ctx) <$> freshMeta ctx
        b <- Closure (env ctx) <$> freshMeta (bind ctx "x" a)
        unifyCatch ctx (VPi "x" a b) tty
        pure (a, b)

    u <- check ctx u a
    pure (App t u, b $$ eval (env ctx) u)
  RU ->
    pure (U, VU)
  RPi x a b -> do
    a <- check ctx a VU
    b <- check (bind ctx x (eval (env ctx) a)) b VU
    pure (Pi x a b, VU)
  RLet x a t u -> do
    a <- check ctx a VU
    let ~va = eval (env ctx) a
    t <- check ctx t va
    let ~vt = eval (env ctx) t
    (u, b) <- infer (define ctx x vt va) u
    pure (Let x a t u, b)
  RHole -> do
    a <- eval (env ctx) <$> freshMeta ctx
    t <- freshMeta ctx
    pure (t, a)

-- Printing
--------------------------------------------------------------------------------
ctxNames :: Ctx -> [Name]
ctxNames = fmap fst . types

showVal :: Ctx -> Val -> String
showVal ctx v =
  prettyTm 0 (ctxNames ctx) (quote (lvl ctx) v) []

fresh :: [Name] -> Name -> Name
fresh ns "_" = "_"
fresh ns x
  | elem x ns = fresh ns (x ++ "'")
  | otherwise = x

-- printing precedences
atomp = 3 :: Int -- U, var

appp = 2 :: Int -- application

pip = 1 :: Int -- pi

letp = 0 :: Int -- let, lambda

-- Wrap in parens if expression precedence is lower than
-- enclosing expression precedence.
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

prettyTm :: Int -> [Name] -> Tm -> ShowS
prettyTm = go
  where
    piBind ns x a =
      showParen True ((x ++) . (" : " ++) . go letp ns a)

    goBDS :: Int -> [Name] -> MetaVar -> [BD] -> ShowS
    goBDS p ns m bds = case (ns, bds) of
      ([], []) -> (("?" ++ show m) ++)
      (ns :> n, bds :> Bound) -> par p appp $ goBDS appp ns m bds . (' ' :) . (n ++)
      (ns :> n, bds :> Defined) -> goBDS appp ns m bds
      _ -> error "impossible"

    go :: Int -> [Name] -> Tm -> ShowS
    go p ns = \case
      Var (Ix x) -> ((ns !! x) ++)
      App t u -> par p appp $ go appp ns t . (' ' :) . go atomp ns u
      Lam (fresh ns -> x) t -> par p letp $ ("λ " ++) . (x ++) . goLam (ns :> x) t
        where
          goLam ns (Lam (fresh ns -> x) t) =
            (' ' :) . (x ++) . goLam (ns :> x) t
          goLam ns t = (". " ++) . go letp ns t
      U -> ("U" ++)
      Pi "_" a b -> par p pip $ go appp ns a . (" → " ++) . go pip (ns :> "_") b
      Pi (fresh ns -> x) a b -> par p pip $ piBind ns x a . goPi (ns :> x) b
        where
          goPi ns (Pi (fresh ns -> x) a b)
            | x /= "_" = piBind ns x a . goPi (ns :> x) b
          goPi ns b = (" → " ++) . go pip ns b
      Let (fresh ns -> x) a t u ->
        par p letp $
          ("let " ++)
            . (x ++)
            . (" : " ++)
            . go letp ns a
            . ("\n  = " ++)
            . go letp ns t
            . (";\n\n" ++)
            . go letp (ns :> x) u
      Meta m -> (("?" ++ show m) ++)
      InsertedMeta m bds -> goBDS p ns m bds

showTm0 :: Tm -> String
showTm0 t = prettyTm 0 [] t []

showTm :: Ctx -> Tm -> String
showTm ctx t = prettyTm 0 (ctxNames ctx) t []

displayMetas :: IO ()
displayMetas = do
  ms <- readIORef mctx
  forM_ (IM.toList ms) $ \(m, e) -> case e of
    Unsolved -> printf "let ?%s = ?;\n" (show m)
    Solved v -> printf "let ?%s = %s;\n" (show m) (showTm0 $ quote 0 v)
  putStrLn ""

-- Error

data UnifyError = UnifyError
  deriving (Show, Exception)

data ElabError = NameNotInScope Name | CantUnify Tm Tm
  deriving (Show, Exception)

data Error = Error Ctx ElabError
  deriving (Show, Exception)

displayError :: String -> Error -> IO ()
displayError file (Error ctx e) = do
  let SourcePos path (unPos -> linum) (unPos -> colnum) = pos ctx
      lnum = show linum
      lpad = map (const ' ') lnum
      msg = case e of
        NameNotInScope x ->
          "Name not in scope: " ++ x
        CantUnify t t' ->
          "Cannot unify expected type\n\n"
            ++ "  "
            ++ showTm ctx t
            ++ "\n\n"
            ++ "with inferred type\n\n"
            ++ "  "
            ++ showTm ctx t'

  printf "%s:%d:%d:\n" path linum colnum
  printf "%s |\n" lpad
  printf "%s | %s\n" lnum (lines file !! (linum - 1))
  printf "%s | %s\n" lpad (replicate (colnum - 1) ' ' ++ "^")
  printf "%s\n" msg
