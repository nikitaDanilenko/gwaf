----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.AList
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- This module contains a simple implementation of association lists and
-- some general purpose functions on these lists.
-- Association lists are represented by a wrapped list of key-value pairs
-- with the additional condition that these lists are sorted in ascending order of their indices.

{-# Language MultiParamTypeClasses #-}

module Auxiliary.AList (
    
    AList,
    asList,
    mkAList
    
    ) where

import Control.Arrow                ( (&&&) )
import Data.Foldable                ( Foldable )
import qualified Data.Foldable as F ( foldr )
import Data.Function                ( on )
import Data.List                    ( groupBy, sortBy, subsequences )
import Data.Maybe                   ( mapMaybe )
import Data.Ord                     ( comparing )
import Test.QuickCheck              ( Arbitrary, arbitrary, shrink )

import Auxiliary.General            ( Row, wrap, orderedLookup, unionByFstWith,
                                      intersectionByWith, intersectionByFstWith, compareFirsts,
                                      differenceByFstWith, differenceByFst, 
                                      symmetricDifferenceByFst, mkArbitrary )
import Auxiliary.KeyedClasses       ( KeyFunctor, fmapWithKey, KeyMaybeFunctor, fmapMaybe,
                                      fmapMaybeWithKey, Lookup, clookup )
import Auxiliary.Mapping            ( Mapping, toRow, fromRow, MappingV, empty, insertWith, delete )
import Auxiliary.SetOps             ( Unionable, unionWith, Intersectable, intersectionWithKey,
                                      intersectionWith, Complementable, differenceWith,
                                      differenceWith2, SetOps, symDifference,
                                      UnionableHom, IntersectableHom, ComplementableHom, SetOpsHom )

-- | A wrapper around a 'Row'.

newtype AList a = AL { asList :: Row a }

-- | Association lists are printed as 'Row's.

instance Show a => Show (AList a) where

    show = show . asList

-- | The equality on 'AList's is a special instance of list equality.

instance Eq a => Eq (AList a) where

    (==) = (==) `on` asList

instance Functor AList where
    
    fmap = mapInner . map . fmap

instance Foldable AList where

    foldr f e = foldr (f . snd) e . asList

-- | Creates a new association list from a 'Row'.
-- Duplicate entries in the 'Row' are ignored and only the first value in the 'Row'
-- is added to the result.

mkAList :: Row a -> AList a
mkAList = AL . map head . groupBy ((==) `on` fst) . sortBy (comparing fst)

-- | This function applies a monotonic function to the underlying association list of the 'AList'.
--   It is assumed to be monotonic, but this condition is not checked.
--   Since it is not safe to use this function outside this module, it is not exported. 

mapInner :: (Row a -> Row b) -> AList a -> AList b
mapInner f = AL . f . asList

-- | This instance maps every key-value-pair @(i, v)@ to @(i, f i v)@,
-- where @f@ is the supplied function.

instance KeyFunctor AList where

    fmapWithKey f = mapInner (map (fst &&& uncurry f))

instance KeyMaybeFunctor AList where

    fmapMaybeWithKey f = mapInner (mapMaybe (wrap . (fst &&& uncurry f)))
    fmapMaybe        f = mapInner (mapMaybe (wrap . fmap f))

-- | A slightly better version than 'lookup' because the search can be aborted before the end
-- of the list.

instance Lookup AList where

    clookup k = orderedLookup k . asList

instance Mapping AList where
    
    toRow   = asList 
    fromRow = mkAList

instance MappingV AList where

    empty             = AL []
    insertWith op i x = mapInner (unionByFstWith op [(i, x)])
    delete k m        = AL (differenceByFst (asList m) [(k, ())])

-- | Intersection operations have a structural complexity /O/(@'size' left@ + @'size' right@)
-- (the computation of the /values/ is not taken into account).

instance Intersectable AList AList where

    intersectionWithKey op = mapInner . intersectionByWith compareFirsts (fmap . uncurry op) . asList
    intersectionWith op    = mapInner . intersectionByFstWith op . asList

instance IntersectableHom AList

-- | Union operations have a structural complexity /O/(@'size' left@ + @'size' right@).

instance Unionable AList AList where

    unionWith op   = mapInner . unionByFstWith op . asList

instance UnionableHom AList

-- | Difference operations have a structural complexity /O/(@'size' left@ + @'size' right@).

instance Complementable AList AList where

    differenceWith op = mapInner . differenceByFstWith op . asList
    differenceWith2   = differenceWith

instance ComplementableHom AList

-- | All set operations have a structural complexity of /O/(@'size' left@ + @'size' right@).

instance SetOps AList AList where

    symDifference = mapInner . symmetricDifferenceByFst . asList

instance SetOpsHom AList

instance Arbitrary a => Arbitrary (AList a) where

    arbitrary = fmap fromRow mkArbitrary
    shrink    = map AL . subsequences . asList