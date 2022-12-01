module Antaa.Term
  ( Ix (..),
    Lvl (..),
    Raw (..),
    Ty,
    Tm (..),
    VTy,
    Val (..),
    Env,
    Spine,
    BD (..),
    Closure (..),
    MetaVar (..),
    MetaEntry (..),
    nextMeta,
    mctx,
    lookupMeta,
    reset,
  )
where

import Antaa.Raw
import Data.IntMap qualified as IM
import GHC.IO
import GHC.IORef

newtype Lvl = Lvl {unLvl :: Int} deriving (Eq, Ord, Show, Num) via Int

type Env = [Val]

type Spine = [Val]

data Closure = Closure Env Tm

type VTy = Val

data Val
  = VFlex MetaVar Spine
  | VRigid Lvl Spine
  | -- host lambda
    VLam Name {-# UNPACK #-} Closure
  | -- host pi
    VPi Name ~VTy {-# UNPACK #-} Closure
  | -- universe
    VU

newtype MetaVar = MetaVar {unMetaVar :: Int} deriving (Eq, Show, Num) via Int

data MetaEntry = Solved Val | Unsolved

nextMeta :: IORef Int
nextMeta = unsafeDupablePerformIO $ newIORef 0
{-# NOINLINE nextMeta #-}

mctx :: IORef (IM.IntMap MetaEntry)
mctx = unsafeDupablePerformIO $ newIORef mempty
{-# NOINLINE mctx #-}

lookupMeta :: MetaVar -> MetaEntry
lookupMeta (MetaVar m) = unsafeDupablePerformIO $ do
  ms <- readIORef mctx
  case IM.lookup m ms of
    Just e -> pure e
    Nothing -> error "impossible"

reset :: IO ()
reset = do
  writeIORef nextMeta 0
  writeIORef mctx mempty

newtype Ix = Ix {unIx :: Int} deriving (Eq, Show, Num) via Int

data BD = Bound | Defined
  deriving (Show)

type Ty = Tm

data Tm
  = Var Ix
  | Lam Name Tm
  | App Tm Tm
  | U
  | Pi Name Ty Ty
  | Let Name Ty Tm Tm
  | Meta MetaVar
  | InsertedMeta MetaVar [BD]
  deriving (Show)
