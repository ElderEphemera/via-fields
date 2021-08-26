{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module ViaFields.Util (pattern Coerced) where

import Data.Coerce

pattern Coerced :: Coercible a b => a -> b
pattern Coerced x <- (coerce -> x)
  where Coerced x = coerce x
