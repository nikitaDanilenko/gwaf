----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Algebraic.Vector
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides functions that are specific for algebraic vectors.
-- In the context of this library,
-- these functions are mostly applications of the more general approach of 'Mapping's to
-- the concrete, algebraic setting.
-- We do not provide actual instances of algebraic type classes.
-- There are two main reasons for this decision:
-- 
-- * The functions should be parametric and wrapping them in an additional @__newtype__@ comes
--   with additional clutter.
-- * Algebraic instances require vectors of a fixed size and homogeneous operations usually require
--   that both arguments have the same size.
--   The provided functions are applicable for the more general case of different sizes.

module Algebraic.Vector (
  
  -- * Auxiliary functions

  (.@.),
  removeZeroes,

  -- * Algebraic functions

  (<+++>),
  (<++>),
  (<--->),
  (<-->),
  additiveInverse,
  (*>>),
  (*>),
  unitVector,
  unitVector2,
  vectorSum,

  -- * Maximum functions

  maxIndicesWithMax,
  maxIndices,
  maxVector,
  restrictToMax

  ) where

import Data.Foldable                ( Foldable )
import qualified Data.Foldable as F ( foldr )

import Algebraic.Structures         ( SemigroupA, MonoidA, (.+.), zero, MonoidM, (.*.),
                                      one, GroupA, inverseA, FindZero, isZero, isNotZero, FindOne,
                                      isOne, Number )
import Auxiliary.General            ( Key, (<.>), scaleLeft )
import Auxiliary.KeyedClasses       ( Lookup, maybeAt, KeyMaybeFunctor, ffilter )
import Auxiliary.Mapping            ( Mapping, singleton, MappingV, empty, keys, values )
import Auxiliary.SetOps             ( UnionableHom, unionWith )

import Prelude hiding               ( (*>) )

-- | A safe version of a lookup operation, because non-existent values
--   are mapped to the 'zero' of the underlying monoid.

infixr 7 .@.

(.@.) :: (MonoidA s, Lookup vec) => vec s -> Key -> s
(.@.) = maybeAt zero

-- | Removes all occurrences of an abstract 'zero' from the vector.

removeZeroes :: (KeyMaybeFunctor vec, FindZero a) => vec a -> vec a
removeZeroes = ffilter isNotZero

-- | The component-wise sum of two vectors. The result may contain zeroes.

infixr 4 <+++>

(<+++>) :: (UnionableHom vec, SemigroupA asg) => vec asg -> vec asg -> vec asg
(<+++>) = unionWith (.+.)

-- | The component-wise sum of two vectors followed by a removal of all zeroes.

infixr 4 <++>

(<++>) :: (KeyMaybeFunctor vec, UnionableHom vec, MonoidA a, FindZero a) => vec a -> vec a -> vec a
(<++>) = removeZeroes <.> (<+++>)

-- | The component-wise subtraction of two vectors.
-- The result may contain zeroes.

(<--->) :: (UnionableHom vec, Functor vec, GroupA ag) => vec ag -> vec ag -> vec ag
x <---> y = x <+++> additiveInverse y

-- | The component-wise subtraction of two vectors followed by a removal of all zeroes.

(<-->) :: (KeyMaybeFunctor vec, UnionableHom vec, GroupA ag, FindZero ag) => 
  vec ag -> vec ag -> vec ag
(<-->) = removeZeroes <.> (<--->)

-- | The additive inverse of a vector.
-- If the input vector contains zeroes,
-- then the result vector also contains zeroes.

additiveInverse :: (Functor vec, GroupA ag) => vec ag -> vec ag
additiveInverse = fmap inverseA

-- | A canonic implementation of scalar multiplication over multiplicative monoids.
-- The result vector may contain newly introduced zeroes.

infixl 5 *>>

(*>>) :: (Functor vec, MonoidM mm) => mm -> vec mm -> vec mm
(*>>) = scaleLeft (.*.)

-- | Optimised scalar multiplication of a vector with a scalar.
-- The contexts 'FindZero' and 'FindOne' allow constant time multiplications with constants.
-- Also, the result of this function does not contains zeroes.

infixl 5 *>

(*>) :: (MappingV vec, FindZero mm, FindOne mm, MonoidM mm) => mm -> vec mm -> vec mm
x *> v | isZero x  = empty
       | isOne  x  = v
       | otherwise = removeZeroes (x *>> v)

-- | Yields the standard unit vector over a multiplicative monoid.

unitVector :: (Mapping vec, MonoidM m) => Key -> vec m
unitVector v = singleton v one

-- | Yields the standard unit vector in the relational sense (a concrete, relation algebraic point).

unitVector2 :: Mapping vec => Int -> vec ()
unitVector2 x = singleton x ()

-- | Adds all elements in a vector.

vectorSum :: (Foldable v, MonoidA am) => v am -> am
vectorSum = F.foldr (.+.) zero

-- | Restricts the given vector to those indices that have maximum values and returns a pair
-- consisting of this vector and the maximum value.
-- If the vector is empty, the maximum is 0.

maxIndicesWithMax :: (Mapping vec, Num n, Ord n) => vec (Number n) -> (vec (Number n), Number n)
maxIndicesWithMax v = (ffilter (m ==) v, m)
  where m = foldr max 0 (values v)

-- | Returns those keys in the vector that have maximum values.

maxIndices :: (Mapping vec, Num n, Ord n) => vec (Number n) -> [Key]
maxIndices = keys . restrictToMax

-- | Returns the maximum value in the vector.

maxVector :: (Mapping vec, Num n, Ord n) => vec (Number n) -> Number n
maxVector = snd . maxIndicesWithMax

-- | Restricts the given vector to those keys that have maximum values.

restrictToMax :: (Mapping vec, Num n, Ord n) => vec (Number n) -> vec (Number n)
restrictToMax = fst . maxIndicesWithMax