----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.MappingSpec
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides tests for the properties that should hold for mappings.

{-# Language ScopedTypeVariables #-}

module Auxiliary.MappingSpec where

import Data.IntMap               ( IntMap )
import Data.Maybe                ( isNothing )
import Test.Hspec                ( Spec )
import Test.QuickCheck           ( Arbitrary, property )

import Auxiliary.AList           ( AList )
import Auxiliary.General         ( Key )
import Auxiliary.KeyedClasses    ( clookup )
import Auxiliary.IntMapArbitrary ()
import Auxiliary.Mapping         ( Mapping, fromRow, toRow, keys, values, size,
                                   MappingV, isEmpty, empty, insert, insertWith,
                                   singleton, fromRow, delete )
import Auxiliary.SafeArray       ( SafeArray )
import Helpers                   ( LabProperties, Proxy ( Proxy ), mkSuite )


-- | Tests the law @'fromRow' . 'toRow' = 'id'@.

prop_fromRowToRow :: (Mapping m, Eq (m a)) => m a -> Bool
prop_fromRowToRow m = fromRow (toRow m) == m

-- | Tests the rule @'size' = 'length' . 'toRow'@.

prop_sizeLengthToRow :: Mapping m => m a -> Bool
prop_sizeLengthToRow m = size m == length (toRow m)

-- | Tests the rule @'keys' = 'map' 'fst' . 'toRow'@.

prop_keysMapFstToRow :: Mapping m => m a -> Bool
prop_keysMapFstToRow m = keys m == map fst (toRow m)

-- | Tests the rule @'values' = 'map' 'snd' . 'toRow'@.

prop_valuesMapSndToRow :: (Mapping m, Eq a) => m a -> Bool
prop_valuesMapSndToRow m = values m == map snd (toRow m)

-- | All tests for the laws of 'Mapping'.

propsMapping :: forall m a . (Mapping m, Eq (m a), Eq a, Arbitrary (m a), Show (m a)) => 
  Proxy (m a) -> LabProperties
propsMapping _ = zip lawsMapping
  [ property (prop_fromRowToRow      :: m a -> Bool),
    property (prop_sizeLengthToRow   :: m a -> Bool),
    property (prop_keysMapFstToRow   :: m a -> Bool),
    property (prop_valuesMapSndToRow :: m a -> Bool)]

-- | Tests the law that 'isEmpty' returns 'True' if and only if the mapping is equal to 'empty'.

prop_emptyMapping :: (MappingV m, Eq (m a)) => m a -> Bool
prop_emptyMapping m = isEmpty m == (m == empty)

-- | Tests the law @'insert' = 'insertWith' 'const'@.

prop_insertConst :: (MappingV m, Eq (m a)) => Key -> a -> m a -> Bool
prop_insertConst k v m = insert k v m == insertWith const k v m

-- | Tests the law @'singleton' k v = 'fromRow' [(k, v)] = 'insert' k v 'empty'@

prop_singletonFromRowInsert :: forall m a . (MappingV m, Eq (m a)) => Key -> a -> m a -> Bool
prop_singletonFromRowInsert k v _ = singleton k v == fr && fr == insert k v empty where
  fr = fromRow [(k, v)] :: m a

-- | Tests the law @'clookup' k ('singleton' l v) = 'Just' v \<===\> k = l@.

prop_singletonLookup :: forall m a . (MappingV m, Eq a) => Key -> Key -> a -> m a -> Bool
prop_singletonLookup k l v _ = case clookup k (singleton l v :: m a) of
                                  Just w  -> k == l && v == w
                                  Nothing -> k /= l

-- | Tests the law @'clookup' k ('insert' k v m)' = 'Just' v@.

prop_lookupInsert :: (MappingV m, Eq a) => Key -> a -> m a -> Bool
prop_lookupInsert k v m = maybe False (v ==) (clookup k (insert k v m))

-- | Tests the law @'clookup' k ('delete' k m)' = 'Nothing'@.

prop_lookupDelete :: MappingV m => Key -> m a -> Bool
prop_lookupDelete k m = isNothing (clookup k (delete k m))

-- | Tests the law @'delete' k . 'delete' k = 'delete' k@.

prop_deleteDelete :: (MappingV m, Eq (m a)) => Key -> m a -> Bool
prop_deleteDelete k m = delete k del == del where
  del = delete k m

-- | Tests the law @'insert' k v . 'insert' k v = 'insert' k v@.

prop_insertInsert :: (MappingV m, Eq (m a)) => Key -> a -> m a -> Bool
prop_insertInsert k v m = insert k v ins == ins where
  ins = insert k v m

propsMappingV :: 
  forall m a . (MappingV m, Eq a, Show a, Arbitrary a, Eq (m a), Arbitrary (m a), Show (m a)) => 
  Proxy (m a) -> LabProperties
propsMappingV _ = zip lawsMappingV [
    property (prop_emptyMapping           :: m a -> Bool),
    property (prop_insertConst            :: Key -> a -> m a -> Bool),
    property (prop_singletonFromRowInsert :: Key -> a -> m a -> Bool),
    property (prop_singletonLookup        :: Key -> Key -> a -> m a -> Bool),
    property (prop_lookupInsert           :: Key -> a -> m a -> Bool),
    property (prop_lookupDelete           :: Key -> m a -> Bool),
    property (prop_deleteDelete           :: Key -> m a -> Bool),
    property (prop_insertInsert           :: Key -> a -> m a -> Bool)
  ]

-- | A list of tested laws for 'Mapping'.

lawsMapping :: [String]
lawsMapping = [  
    "fromRow . toRow = id"     
  , "size = length . toRow"    
  , "keys = map fst . toRow"  
  , "values = map snd . toRow"
  ]

-- | A list of tested laws for 'MappingV'.

lawsMappingV :: [String]
lawsMappingV = [
    "isEmpty m = True <===> m == empty"
  , "insert = insertWith const"
  , "singleton k v = fromRow [(k, v)] = insert k v empty"
  , "lookup k (singleton l v) = Just w <===> k = l && v == w (and Nothing otherwise)"
  , "lookup k (insert k v m) = Just v"
  , "lookup k (delete k m) = Nothing"
  , "delete k . delete k = delete k"
  , "insert k v . insert k v = insert k v" ]

-- | A list of structures the laws are tested for.

structures :: [String]
structures = ["AList", "IntMap", "SafeArray"]

-- | A list of types the laws are tested for.

types :: [String]
types = ["Integer", "(Double, String)", "[Bool]"]

spec :: Spec
spec = mkSuite $
    zip [unwords ["Mapping:", s, t] | s <- structures, t <- types]
        [
          propsMapping (Proxy :: Proxy (AList Integer)),
          propsMapping (Proxy :: Proxy (AList (Double, String))),
          propsMapping (Proxy :: Proxy (AList [Bool])),
          propsMapping (Proxy :: Proxy (IntMap Integer)),
          propsMapping (Proxy :: Proxy (IntMap (Double, String))),
          propsMapping (Proxy :: Proxy (IntMap [Bool])),
          propsMapping (Proxy :: Proxy (SafeArray Integer)),
          propsMapping (Proxy :: Proxy (SafeArray (Double, String))),
          propsMapping (Proxy :: Proxy (SafeArray [Bool]))
        ]
  ++
    zip [unwords [s, t] | s <- structures, t <- types, s /= "SafeArray"]
        [
          propsMappingV (Proxy :: Proxy (AList Integer)),
          propsMappingV (Proxy :: Proxy (AList (Double, String))),
          propsMappingV (Proxy :: Proxy (AList [Bool])),
          propsMappingV (Proxy :: Proxy (IntMap Integer)),
          propsMappingV (Proxy :: Proxy (IntMap (Double, String))),
          propsMappingV (Proxy :: Proxy (IntMap [Bool]))
        ]