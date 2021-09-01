{-# OPTIONS_GHC -fplugin=ViaFields #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
#if __GLASGOW_HASKELL__ < 920
{-# LANGUAGE PatternSynonyms #-}
#endif
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (unless)
import Data.Monoid (Sum(..), Product(..))
import Data.Ord (Down(..))
import GHC.Generics (Generic(..))
import System.Exit (die)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  unless (T 1 "!" () 4 <> T 2 "?" () 5 == T 3 "!?" () (20 :: Int)) $
    die "T 3 \"!?\" () 20"
  unless (XY { x = (5 :: Int), y = 4 } < XY { x = 3, y = 2 }) $
    die "5 > 3"

-- | A custom monoid with type variables
data T a b = T (Int via Sum Int) String a (b via Product b)
  deriving stock (Eq, Generic)
  deriving (Semigroup, Monoid) via Generically (T a b)

-- | Coodinates ordered right-to-left, top-to-bottom
data XY = XY
  { x :: {-# UNPACK #-} !(Int via Down Int) -- ^ The X component
  , y :: {-# UNPACK #-} !Int -- ^ The Y component
  }
  deriving stock (Eq, Ord)

--------------------------------------------------------------------------------

-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5726

newtype Generically a = Generically a

instance (Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
  (<>) :: Generically a -> Generically a -> Generically a
  Generically a <> Generically b = Generically (to (from a <> from b :: Rep a ()))

instance (Semigroup a, Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
  mempty :: Generically a
  mempty = Generically (to (mempty :: Rep a ()))

  mappend :: Generically a -> Generically a -> Generically a
  mappend = (<>)
