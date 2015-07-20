----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Algebraic.Semiring
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module contains the semiring type class,
-- as well as the associated type classes for additive and multiplicative monoids
-- (with and without the possibility to distinguish the constants).
-- Additionally, this module provides some (standard) implementations of semirings.
-- There is some overlap with
-- 
--   *  [FHW]    Fischer, Huch, Wilke: A play on Regular Expressions
--
--   *  [FHWLib] Fischer, Huch, Wilke: 
--              <http://hackage.haskell.org/package/weighted-regexp weighted-regexp>
-- 
-- and with the deprecated package <http://hackage.haskell.org/package/monoids monoids>.

module Algebraic.Semiring (
    -- * Additive Monoids
    MonoidA ( .. )

    ) where

-- | A type class for additive monoids.
--   Instances of this type class are required to satisfy the rules
--   for all @a, b, c :: m@
--   
--   [/associativity/]
--      @a '.+.' (b '.+.' c) = (a '.+.' b) '.+.' c@
--   
--   [/neutrality/]
--      @a '.+.' 'zero' = a = 'zero' '.+.' a@

class MonoidA a where
    {-# MINIMAL (.+.), zero #-}

    -- | A binary associative operation.

    infixr 4 .+.
    (.+.) :: a -> a -> a

    -- | A neutral element with respect to '(.+.)'.
    --   This element is automatically unique.

    zero  :: a
    
    -- | The sum of a list of elements.
    msum :: [a] -> a
    msum = foldr (.+.) zero