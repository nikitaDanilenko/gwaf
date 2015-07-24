----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.SetOpsInstances
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides instances for several subclasses of 'SetOps' exclusively,
-- there are no function or data type definitions in this module.
-- This structure is due to the fact that the heterogeneous set operations require two
-- imports per operation and this creates a cyclic dependency.
-- The following combinations are defined.
-- 
-- * 'Intersectable'  'AList'  'IntMap'
-- * 'Intersectable'  'IntMap' 'IntMap'
-- * 'Intersectable'  'AList'  'SafeArray'
-- * 'Intersectable'  'IntMap' 'SafeArray'
-- * 'Unionable'      'AList'  'IntMap'
-- * 'Unionable'      'IntMap' 'IntMap'
-- * 'Complementable' 'AList'  'IntMap'
-- * 'Complementable' 'IntMap' 'IntMap'
-- * 'SetOps'         'AList'  'IntMap'
-- * 'SetOps'         'IntMap' 'IntMap'

{-# Language MultiParamTypeClasses #-}

module Auxiliary.SetOpsInstances where

import Data.IntMap.Lazy                 ( IntMap )
import qualified Data.IntMap.Lazy as IM ( intersectionWithKey, update, delete,
                                          insert, insertWith, differenceWith,
                                          unionWith )
import Data.Maybe                       ( isNothing )

import Auxiliary.KeyedClasses           ( Lookup ( clookup ) )
import Auxiliary.AList                  ( AList, asList )
import Auxiliary.SetOps                 ( Intersectable ( intersectionWithKey ), 
                                          Unionable ( unionWith ), 
                                          Complementable ( differenceWith, differenceWith2 ),
                                          SetOps ( symDifference ), capWithKey, difWith )
import Auxiliary.SafeArray              ( SafeArray )

-- | Intersection with a complexity of /O/((@'size' left@) * log(@'size' right@)).

instance Intersectable AList IntMap where

    intersectionWithKey = capWithKey

-- | Intersection with a complexity of /O/(@'size' left@ + @'size' right@).

instance Intersectable IntMap IntMap where

    intersectionWithKey = IM.intersectionWithKey

-- | Intersection with a complexity of /O/(@size@ left).

instance Intersectable AList SafeArray where

    intersectionWithKey = capWithKey

-- | Intersection with a complexity of /O/(@size@ left).

instance Intersectable IntMap SafeArray where

    intersectionWithKey = capWithKey

-- | Union with a complexity of /O/((@'size' left@) * log(@'size' right@)).

instance Unionable AList IntMap where

    unionWith op r m = foldr (uncurry (IM.insertWith op)) m (asList r)

-- | Union with a complexity of /O/(@'size' left@ + @'size' right@).

instance Unionable IntMap IntMap where

    unionWith        = IM.unionWith

-- | Relative complement with a complexity of /O/((@'size' al@) * log(@'size' im@)),
-- where @al@ is the 'AList' and @im@ is the 'IntMap'.

instance Complementable AList IntMap where

    differenceWith       = difWith
    differenceWith2 op m = foldr (\(i, y) -> IM.update (`op` y) i) m . asList

-- | Relative complement with a complexity of /O/(@'size' left@ + @'size' right@).

instance Complementable IntMap IntMap where

    differenceWith    = IM.differenceWith
    differenceWith2   = differenceWith

-- | All set operations have a complexity of /O/((@'size' al@) * log(@'size' im@)),
-- where @al@ is the 'AList' and @im@ is the 'IntMap'.

instance SetOps AList IntMap where

    symDifference r m  = foldr f m (asList r) where
        f (i, y) im | isNothing (clookup i im) = IM.insert i y im
                    | otherwise                = IM.delete i im

-- | All set operations have a complexity of /O/(@'size' left@ + @'size' right@).

instance SetOps IntMap IntMap