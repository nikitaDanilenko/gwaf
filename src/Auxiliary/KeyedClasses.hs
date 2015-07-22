----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.KeyedClasses
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides some type classes that deal with indexed functors.
-- Several of these classes are similar to the ones given in the 
-- <http://hackage.haskell.org/package/keys keys> package by E. Kmett,
-- but these are less overloaded since we deal with integer ('Int')
-- keys only and do not employ type families to gain generality.

module Auxiliary.KeyedClasses (
    
    Key,

    -- * Functors with implicit key values

    KeyFunctor ( .. ),

    addKeys,
    foldrWithKey,
    minKeyValue,
    minKeyValueBy,

    -- * Functors with a generalised mapping function

    KeyMaybeFunctor ( .. ),

    -- * Structures with @lookup@

    Lookup ( .. ),

    maybeAt,
    contained

    ) where

import Data.Foldable                    ( Foldable )
import qualified Data.Foldable as F     ( foldr1, foldr )
import Data.Function                    ( on )
import Data.IntMap.Lazy                 ( IntMap )
import qualified Data.IntMap.Lazy as IM ( mapWithKey, mapMaybeWithKey, mapMaybe, lookup,
                                          filterWithKey, filter )
import Data.Maybe                       ( fromMaybe, isJust )

import Auxiliary.General                ( (<.>), Arc, minBy )

type Key = Int

-- |
-- For all kind of indexed containers. The function 'fmap' applies its function
-- parameter to the values at all positions, while 'fmapWithKey' also considers
-- the key of the value. The function 'fmapWithKey' is required to satisfy the following:
--
-- [/composition/]
--   @'fmapWithKey' g . 'fmapWithKey' f = 'fmapWithKey' (\\k -> g k . f k)@
--
-- [/identity/]
--   @'fmapWithKey' ('const' 'id') = 'id'@
--
-- [/compatibility/]
--   @'fmap' f = 'fmapWithKey' ('const' f)@
-- 
-- These three laws imply that the underlying 'Functor' instance satisfies the functor laws.

class Functor f => KeyFunctor f where
    {-# Minimal fmapWithKey #-}

    -- | Map a function over a structure that takes an additional key argument.

    fmapWithKey :: (Key -> a -> b) -> f a -> f b

-- | The 'addKeys' function adds the corresponding key to every value in the structure.

addKeys :: KeyFunctor f => f a -> f (Arc a)
addKeys = fmapWithKey (,)

-- | 'IntMap's provide a function 'mapWithKey' which satisfies the above laws, so that the
--   'KeyFunctor' instance uses this function.

instance KeyFunctor IntMap where

    fmapWithKey = IM.mapWithKey

-- | Instances of 'Foldable' and 'KeyFunctor' allow folding the structure according to a key-
--   depending function. This is achieved by first adding keys to every value in the structure
--   and then using the abstract 'foldr' function to fold the structure.

foldrWithKey :: (Foldable f, KeyFunctor f) => (Key -> a -> b -> b) -> b -> f a -> b
foldrWithKey phi e = F.foldr (uncurry phi) e . addKeys

-- | Computes the minimum value in a non-empty structure with respect to 'compare' and returns
--   this value and the \"leftmost\" key it appeared at. If the structure is empty, this function
--   returns an error.

minKeyValue :: (KeyFunctor f, Foldable f, Ord a) => f a -> Arc a
minKeyValue = minKeyValueBy compare

-- | Non-overloaded version of 'minKeyValue' which takes a comparing function as and additional
--   argument and returns a pair consisting of the minimum value according to the comparison
--   function in the second component and its leftmost occurrence in the first component. If the
--   structure is empty, an error occurs.

minKeyValueBy :: (KeyFunctor f, Foldable f) => (a -> a -> Ordering) -> f a -> Arc a
minKeyValueBy cmp = F.foldr1 (minBy (cmp `on` snd)) . addKeys

-- |
-- The general idea is to combine a function that maps container values to a 'Maybe'-type and a
-- function that filters out all occurrences of 'Nothing'.
-- 
-- Instances of this type class have to satisfy the following laws:
-- 
-- [/ composition /]
--   @'fmapMaybeWithKey' g . 'fmapMaybeWithKey' f = 'fmapMaybeWithKey' (\\ k v -> f k v '>>=' g k) @
-- 
-- [/ identity /]
--   @'fmapMaybeWithKey' (\\_ v -> 'Just' v) = 'id'@
-- 
-- [/ compatibility (keys) /]
--   @'fmapWithKey' f = 'fmapMaybeWithKey' ('Just' '<.>' f)@
-- 
-- [/ compatibility (no keys) /]
--   @ 'fmapMaybe' g = 'fmapMaybeWithKey' . 'const' @
-- 
-- These laws imply that the underlying 'KeyFunctor' instance satisfies the respective composition
-- and identity laws. They additionally imply:
-- 
-- [/ composition (no keys) /]
--   @ 'fmapMaybe' g . 'fmapMaybe' f = 'fmapMaybe' (\\x -> f x '>>=' g) @
--   
-- [/ identity (no keys) /]
--   @ 'fmapMaybe' 'Just' = 'id'@
--   
-- [/ compatibility (keys\/no keys) /]
--   @ 'fmapMaybeWithKey' f = 'fmapMaybe' ('uncurry' f) . 'fmapWithKey' (,) @
--   
-- [/ wrapping /]
--   @ 'fmapWithKey' (,) . 'fmapMaybe' ('uncurry' f) . 'fmapWithKey' (,)@
--   @ = 'fmapMaybeWithKey' (\\k v -> 'wrap' (k, f k v)) @
--    
-- A minimal complete definition can be obtained in one of the following ways:
-- 
-- [Option 1] Define a function 'fmapMaybeWithKey' satisfying /composition/, /identity/ and
--   /compatibility (keys)/ and then define
--    
--   > fmapMaybe = fmapMaybeWithKey . const
-- 
-- [Option 2] Define a function @fmapMaybe@ satisfying /composition (no keys)/, /identity (no keys)/,
--    /wrapping/ and
--    
--    [/ pseudo-identity /]
--      @ 'fmapMaybe' ('uncurry' ('const' 'Just')) . 'fmapWithKey' (,) = 'id' @
--      
--    Then proceed to define
--    
--    > fmapMaybeWithKey f = fmapMaybe (uncurry f) . fmapWithKey (,)
--    

class KeyFunctor f => KeyMaybeFunctor f where
    {-# Minimal fmapMaybe | fmapMaybeWithKey #-}

    -- | The function 'fmapMaybeWithKey' applies the supplied function to every element of the
    --   structure, removing all occurrences of 'Nothing'. This is a combination of
    --   'fmapWithKey' and 'mapMaybe'.

    fmapMaybeWithKey :: (Key -> a -> Maybe b) -> f a -> f b
    fmapMaybeWithKey f = fmapMaybe (uncurry f) . fmapWithKey (,)

    -- | This function applies a given function parameter that is independent of the keys to
    --   every value in the structure and removes possible occurrences of 'Nothing'.

    fmapMaybe :: (a -> Maybe b) -> f a -> f b
    fmapMaybe = fmapMaybeWithKey . const

    -- | The function 'ffilterWithKey' generalises 'Prelude.filter' by taking a predicate that
    --   is allowed to take keys into account. This function is required to satisfy
    --   
    --   > ffilterWithKey p = fmapMaybeWithKey (\i x -> if p i x then Just x else Nothing)
    --   
    --   which is the default definition. It can be overridden for better performance.

    ffilterWithKey :: (Key -> a -> Bool) -> f a -> f a
    ffilterWithKey p = fmapMaybeWithKey fil where
        fil i x | p i x     = Just x
                | otherwise = Nothing

    -- | The function 'ffilter' is the analogon of 'Prelude.filter' and filters values according
    --   to a given predicate that does not depend on the keys. It is required to satisfy
    --   
    --   > ffilter p c = ffilterWithKey (\_ x -> p x) c
    --    
    --   or, equivalently:
    --   
    --   > ffilter = ffilterWithKey . const
    --    
    --   The latter is the default definition, which can be overridden for better performance.

    ffilter :: (a -> Bool) -> f a -> f a
    ffilter = ffilterWithKey . const

-- | The 'IntMap' structure provides functions with the desired properties and near-identical
--   names (except for the @f@ in the beginning). These functions are mapped one-to-one to their
--   respective abstractions.

instance KeyMaybeFunctor IntMap where

    fmapMaybeWithKey = IM.mapMaybeWithKey
    fmapMaybe        = IM.mapMaybe
    ffilterWithKey   = IM.filterWithKey
    ffilter          = IM.filter

-- | This class supplies a function that generalises the 'lookup' function
-- on lists to indexed containers.

class Lookup l where
    {-# Minimal clookup | at #-}

    -- | This function looks up a key in the structure. If the key is present, its value is returned
    --   wrapped in a 'Just', otherwise 'Nothing' is returned.

    clookup :: Key -> l a -> Maybe a
    clookup = flip at

    -- | This function is 'clookup' with its arguments interchanged (and is required to be that).

    at      :: l a -> Key -> Maybe a
    at = flip clookup

-- | This function takes a default value, a structure and a key and returns the value at the key if
--   one is present or the default value otherwise.

maybeAt :: Lookup l => a -> l a -> Key -> a
maybeAt x = fromMaybe x <.> at

-- | This function checks, whether a key is contained in the structure.

contained :: Lookup l => Key -> l a -> Bool
contained = isJust <.> clookup

-- | 'IntMap's provide a lookup function that is used as definition for 'clookup'.

instance Lookup IntMap where

    clookup = IM.lookup