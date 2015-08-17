----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.MaximumMatching
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides functions for the computations of maximum matchings.

module Graph.MaximumMatching (

	) where

import Algebraic.Matrix  ( HasVMM )
import Auxiliary.Mapping ( Mapping, some )
import Auxiliary.SetOps  ( Intersectable, Complementable )
import Graph.Graph       ( Graph, withoutSuccessorsVec, withoutSuccessorsOuter )
import Graph.Path        ( emptyPath, stepRight )
import Graph.Paths       ( VPath, shortestOneMultiplicationWith, (.*~=) )

-- | Computes an augmenting path if such a path exists.
-- The first argument is the matching and the second one is its complement.	

augmentingPath :: 
	(HasVMM vec q, Mapping q, Intersectable vec q,  Complementable vec q, Intersectable vec vec) => 
	Graph q vec a -> Graph q vec a -> Maybe VPath
augmentingPath m eNotM = fmap (uncurry (flip stepRight)) (some result) where
  result = shortestOneMultiplicationWith (.*~=) (fmap (const emptyPath) u .*~= eNotM) [m, eNotM] u'
  u      = withoutSuccessorsVec m
  u'     = withoutSuccessorsOuter m -- outer type for more efficient intersections.