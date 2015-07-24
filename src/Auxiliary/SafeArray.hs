----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.SafeArray
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides a dense representation of 'Key'-to-something maps using
-- immutable arrays.
-- The query operations on this data type are efficient,
-- but modification is expensive.

{-# Language MultiParamTypeClasses #-}

module Auxiliary.SafeArray ( 

  SafeArray,
  newSafeArray,
  (!),
  bounds,
  fullSize,
  toFullList,
  
  -- * Combination functions
  
  zipWithOpKey,
  zipWithOp,
  zipWithDownKey,
  zipWithDown,
  zipWithUp

  ) where

import Control.Applicative       ( liftA2 )
import Control.Monad             ( mplus )
import Data.Array                ( Array, elems, listArray, assocs, array,
                                   range, inRange )
import qualified Data.Array as A ( (!), bounds )
import Data.Maybe                ( catMaybes, mapMaybe )
import Test.QuickCheck           ( Arbitrary, arbitrary )

import Auxiliary.General         ( Key, wrap, mkArbitrary, unionByFstWith )
import Auxiliary.KeyedClasses    ( KeyFunctor, fmapWithKey, KeyMaybeFunctor, fmapMaybeWithKey,
                                   Lookup, clookup )
import Auxiliary.Mapping         ( Mapping, values, size, toRow, fromRow )
import Auxiliary.SetOps          ( Intersectable, intersectionWithKey, intersectionWith,
                                   Unionable, unionWith, Complementable, differenceWith,
                                   differenceWith2, SetOps )

-- | A safe array variant in the sense that queries behave rather as a lookup and not
-- as a pure indexing operation.
-- Also, a 'Maybe' result is used for queries, so that accessing unfilled positions (even if
-- they are in the array range) does not result in an error.

newtype SafeArray a = Safe { safeArray :: Array Key (Maybe a) }
  deriving Eq

-- | A safe variant of a query operation on arrays.
-- If the index is out of bounds, 'Nothing' is returned.

(!) :: SafeArray a -> Int -> Maybe a
arr ! i | inRange (bounds arr) i = safeArray arr A.! i
        | otherwise              = Nothing

instance Functor SafeArray where

    fmap f  = Safe . fmap (fmap f) . safeArray

instance KeyFunctor SafeArray where

    fmapWithKey f a = Safe (listArray (A.bounds arr) (map (uncurry (fmap . f)) (assocs arr)))
        where arr = safeArray a

instance KeyMaybeFunctor SafeArray where

    fmapMaybeWithKey f a = Safe (listArray (A.bounds arr) (map (uncurry g) (assocs arr)))
        where arr = safeArray a
              g i x = x >>= f i

instance Lookup SafeArray where

    clookup = flip (!)

-- | Shows the underlying array.

instance Show a => Show (SafeArray a) where
    
    show = show . safeArray

instance Arbitrary a => Arbitrary (SafeArray a) where

  arbitrary = fmap fromRow mkArbitrary

-- | Creates a new 'SafeArray' of a fixed size where every element is 'Nothing'.
-- Since modification of arrays is linear in the size of the array,
-- this operation should be used sparsely.
-- For arrays that are already filled with values rather use 'fromRow' from the
-- 'Mapping' instance.

newSafeArray :: Int -> SafeArray a
newSafeArray n = Safe (listArray (0, n - 1) (replicate n Nothing))

-- | Transforms the array into a \"full\" association list,
-- which is a list representation of the complete array.

toFullList :: SafeArray a -> [(Key, Maybe a)]
toFullList = assocs . safeArray

-- | Returns the bounds of the 'SafeArray'.

bounds :: SafeArray a -> (Key, Key)
bounds = A.bounds . safeArray

-- | Returns the number of positions that are filled in the array including those that are
-- filled with 'Nothing'.

fullSize :: SafeArray a -> Int
fullSize = snd . bounds

instance Mapping SafeArray where

  toRow       = mapMaybe wrap . toFullList
  fromRow ivs = Safe (array (0, n) 
    (unionByFstWith mplus (zip [0 .. ] (replicate n Nothing)) (map (fmap Just) ivs)))
      where n = foldr (max . fst) (-1) ivs

  values      = catMaybes . elems . safeArray
  
-- | This function takes a combination operator, two arrays and zips the two arrays
-- applying the combination operator to each key, its value in the first array and its
-- value in the second array.
-- The values from the two arrays are obtained through queries.
-- The result array is as large as the larger of the two arrays.

zipWithOpKey :: (Key -> Maybe a -> Maybe b -> Maybe c) -> SafeArray a -> SafeArray b -> SafeArray c
zipWithOpKey op x y = Safe (listArray bnds [op i (x ! i) (y ! i) | i <- range bnds])
    where bnds = (0, max (size x) (size y) - 1)

-- | A variant of 'zipWithOpKey' that ignores the key.

zipWithOp :: (Maybe a -> Maybe b -> Maybe c) -> SafeArray a -> SafeArray b -> SafeArray c
zipWithOp = zipWithOpKey . const

-- | This function takes a binary function and lifts it to a function of 'Maybe's,
-- which is then passed to 'zipWithOp'.
-- If one of the arguments is 'Nothing', so is the lifted function's result.
-- The \"down\" in the name hints at this fact, because the result array is a restriction of
-- both arguments.

zipWithDown :: (a -> b -> c) -> SafeArray a -> SafeArray b -> SafeArray c
zipWithDown = zipWithOp . liftA2

-- | A variant of 'zipWithDown' that uses the key values.

zipWithDownKey :: (Key -> a -> b -> c) -> SafeArray a -> SafeArray b -> SafeArray c
zipWithDownKey op = zipWithOpKey (liftA2 . op)

-- | This function lifts a binary operation to 'Maybe' values and then applies
-- 'zipWithOp' to the result.
-- If @op@ is the binary operation, then
-- its lifted version returns @'Just' (v `op` w)@ if the first argument if @'Just' v@ and the
-- second one is @'Just' w@ and the respective other value in case one of the arguments is
-- 'Nothing'.
-- The \"up\" in the name hints at the fact that the result array is an extension of both
-- arguments.

zipWithUp :: (a -> a -> a) -> SafeArray a -> SafeArray a -> SafeArray a
zipWithUp op = zipWithOp op'
    where op' (Just x) (Just y) = Just (x `op` y)
          op' x        y        = x `mplus` y

-- | The intersection operation has an /O/@('max' ('fullSize' left) ('fullSize' right))@ complexity.

instance Intersectable SafeArray SafeArray where

  intersectionWithKey = zipWithDownKey
  intersectionWith    = zipWithDown

-- | The union operation has an /O/@('max' ('fullSize' left) ('fullSize' right))@ complexity.

instance Unionable SafeArray SafeArray where

  unionWith = zipWithUp

-- | The relative complement operation has 
-- an /O/@('max' ('fullSize' left) ('fullSize' right))@ complexity.

instance Complementable SafeArray SafeArray where

  differenceWith op = zipWithOp op' where
    op' Nothing  _        = Nothing
    op' (Just x) Nothing  = Just x
    op' (Just x) (Just y) = x `op` y

  differenceWith2   = differenceWith

-- | All set operations have an /O/@('max' ('fullSize' left) ('fullSize' right))@ complexity.

instance SetOps SafeArray SafeArray