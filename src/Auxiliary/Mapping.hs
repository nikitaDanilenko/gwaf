----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.Mapping
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides a simple abstraction of 'Key'-to-something maps.
-- The abstraction uses our type classes 'Lookup' and 'KeyMaybeFunctor' and
-- extends a combination of these two by transformations to and from association lists.
-- Additionally, we provide two variations of the resulting type class,
-- where the first one provides an empty map and the possibility to insert values into
-- maps,
-- while the second one allows the creation of a map with a fixed size.

module Auxiliary.Mapping (

    -- * Mapping abstraction

    Mapping ( .. ),
    transform,
    toMappingFrom,
    toMappingWith,
    toMapping,
    fromMapping,

    -- * Mappings with variable sizes
    
    MappingV ( .. ),
    cupWith,
    bigcupWith,
    cupLeft,
    bigcupLeft

    ) where

import Control.Arrow                      ( (&&&) )
import Data.Foldable                      ( Foldable )
import Data.IntMap.Lazy                   ( IntMap )
import qualified Data.IntMap.Lazy as IM   ( toAscList, fromList, empty, keys, elems, null, findMin,
                                            size, insertWith, insert, delete, singleton )
import Data.Maybe                         ( listToMaybe )

import Auxiliary.General                  ( Key, Arc, Row, (<.>) )
import Auxiliary.KeyedClasses             ( Lookup (..), KeyMaybeFunctor (..),
                                            KeyFunctor (..), foldrWithKey )

-- | The 'Mapping' type class is an extension of the type classes 'KeyMaybeFunctor' and
-- 'Lookup'.
-- The additional feature of 'Key'-to-something mappings is that they can be transformed to
-- lists of key-value pairs, which we call 'Row's, and created from such lists.
-- These conversions allow a variety of applications, which have to satisfy the following
-- laws:
-- 
-- * @'fromRow' . 'toRow' = 'id'@
-- * @'size' = 'length' . 'toRow'@
-- * @'keys' = 'map' 'fst' . 'toRow'@
-- * @'values' = 'map' 'snd' . 'toRow'@
-- 
-- Note that we do not require the law @'toRow' . 'fromRow' = 'id'@, 
-- because association lists are not necessarily returned in the same key order.

class (KeyMaybeFunctor m, Lookup m) => Mapping m where
  {-# Minimal toRow, fromRow #-}

  -- | Returns the list of key-value pairs in ascending key order.

  toRow :: m a -> Row a
  
  -- | Transforms a row into a mapping.
  -- This function should be independent of the key order in the supplied row.

  fromRow :: Row a -> m a

  -- | The size of a mapping, which is the number of keys that have values.

  size :: m a -> Int
  size = length . toRow
  
  -- | Returns a key-value pair in the mapping, if the mapping is not empty and 'Nothing'
  -- otherwise.
  
  some :: m a -> Maybe (Arc a)
  some = listToMaybe . toRow
  
  -- | Returns the list of keys in the mapping in ascending order.

  keys :: m a -> [Key]
  keys = map fst . toRow
  
  -- | Returns the list of values in the mapping in ascending order of their keys.

  values :: m a -> [a]
  values = map snd . toRow
  
  -- | Checks whether a mapping is empty.
  
  isEmpty :: m a -> Bool
  isEmpty = null . toRow

  -- | Creates a singleton mapping that has a value at exactly one key.

  singleton :: Key -> a -> m a
  singleton = fromRow . return <.> (,)

-- | This function transforms one mapping representation to another using
-- 'fromRow' and 'toRow' from different instances.
-- It should be used sparsely,
-- because its complexity is linear in the size of the mapping.

transform :: (Mapping m, Mapping m') => m a -> m' a
transform = fromRow . toRow

-- | Creates a new mapping from a list of keys and a function that
-- maps keys to values.
-- Essentially, the resulting mapping is a restriction of the first argument
-- to the second one.

toMappingFrom :: Mapping m => (Key -> a) -> [Key] -> m a
toMappingFrom f = fromRow . map (id &&& f)

-- | Creates a new mapping from a key list labelling every key with the same value.

toMappingWith :: Mapping m => a -> [Key] -> m a
toMappingWith = toMappingFrom . const

-- | Creates a new mapping from a list of keys labelling every key with '()'.

toMapping :: Mapping m => [Key] -> m ()
toMapping = toMappingWith ()

-- | The same as 'keys'.

fromMapping :: Mapping m => m a -> [Key]
fromMapping = keys

-- | 'IntMap's provide all required functions directly.
    
instance Mapping IntMap where
     
  toRow     = IM.toAscList
  fromRow   = IM.fromList
  size      = IM.size
  some m    | IM.null m = Nothing
            | otherwise = Just (IM.findMin m)
  values    = IM.elems
  keys      = IM.keys
  isEmpty   = IM.null
  singleton = IM.singleton

-- | The type class 'Mapping' does not provide functions for the modification of particular
-- keys directly.
-- Clearly, this is possible by transforming a mapping into an association list,
-- modifying said list and transforming it back into a mapping.
-- However, this approach comes with a lot of overhead,
-- because each transformation is (likely) linear in the size of the mapping.
-- The type class 'MappingV' provides an abstraction of an efficient key modification.
-- Instances of this type class are required to satisfy the following laws:
-- 
-- * @'size' m = 0 \<===\> m = 'empty'@
-- * @'insert' = 'insertWith' 'const'@
-- * @'singleton' k v = 'fromRow' [(k, v)] = 'insert' k v 'empty'@
-- * @'clookup' k ('singleton' l v) = 'Just' v \<===\> k = l@
-- * @'clookup' k ('singleton' l v) = 'Nothing' \<===\> k /= l@
-- * @'clookup' k ('insert' k v m)' = 'Just' v@
-- * @'clookup' k ('delete' k m) = 'Nothing'@
-- * @'delete' k . 'delete' k = 'delete' k@
-- * @'insert' k v . 'insert' k v = 'insert' k v@

class Mapping m => MappingV m where
  {-# Minimal empty, insertWith, delete #-}
  
  -- | The empty mapping. Should be computatable in constant time.
  
  empty :: m a
  
  -- | An insertion operation that applies a combination function in case the inserted
  -- key is already present in the map.
  -- To be more precise: if the key @k@ is present in the mapping with associated value
  -- @v@, then @'insertWith' op k w'@ updates the value at @k@ to @v `op` w@.
  
  insertWith :: (a -> a -> a)  -- ^ operation applied to the new value and the original one
                                --   in case of key equality
              -> Key            -- ^ the key the value is inserted at
              -> a              -- ^ the inserted value
              -> m a            -- ^ the mappping that is modified
              -> m a

  -- | A variant of 'insertWith' that overwrites keys that are present in the mapping.

  insert :: Key -> a -> m a -> m a
  insert = insertWith const

  -- | Deletes a key-value pair at a given key.
  -- If the key is not present in the mapping, the mapping is returned unchanged.

  delete :: Key -> m a -> m a

-- | This function incorporates one particular insertion strategy. 
-- The first structure is traversed and each of its entries is added to the second container, 
-- where the supplied function is applied
-- in the case that the key is already present in the mapping.
-- While this function is fully parametric in its containers, 
-- it implements exactly /one/ strategy and may thus not be
-- the optimal solution for all container types.
-- The mnemonic is \"a cup is a union, but not every union is a cup\".

cupWith :: (Foldable f, KeyFunctor f, MappingV m) => (a -> a -> a) -> f a -> m a -> m a
cupWith op c m = foldrWithKey (insertWith op) m c

-- | A left-biased variant of 'cupWith'.

cupLeft :: (Foldable f, KeyFunctor f, MappingV m) => f a -> m a -> m a
cupLeft = cupWith const

-- | A folded version of 'cupWith'.
-- Note that the accumulation parameter needs to be supplied and is not necessarily 'empty'.

bigcupWith :: (Foldable f, KeyFunctor f, MappingV m) => (a -> a -> a) -> m a -> [f a] -> m a
bigcupWith op = foldr (cupWith op)

-- | A folded version of 'cupLeft'.
-- The first argument is the variable mapping the values in the list are inserted into.

bigcupLeft :: (Foldable f, KeyFunctor f, MappingV m) => m a -> [f a] -> m a
bigcupLeft = bigcupWith const

-- | 'IntMap's provide the required functions (and laws) directly.

instance MappingV IntMap where

  empty      = IM.empty
  insertWith = IM.insertWith
  insert     = IM.insert
  delete     = IM.delete  