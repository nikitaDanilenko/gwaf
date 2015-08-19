----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.MaximumMatching
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides functions for the computations of maximum matchings in bipartite graphs.

module Graph.MaximumMatching (

	maximumMatchingBoth,
	maximumMatching,
	maximumMatchingHKBoth,
	maximumMatchingHK

	) where

import Control.Arrow     ( (***) )
import Data.Functor      ( void )
import Data.Traversable  ( Traversable )

import Algebraic.Matrix  ( HasVMM )
import Auxiliary.General ( stepwise2 )
import Auxiliary.Mapping ( Mapping, MappingV, some )
import Auxiliary.SetOps  ( Intersectable, Complementable, Unionable, SetOps, symDifference )
import Graph.Graph       ( Graph, withoutSuccessorsVec, withoutSuccessorsOuter, emptyGraph2,
	                         pathToSymGraph, numberOfVertices )
import Graph.Path        ( emptyPath, stepRight )
import Graph.Paths       ( VPath, shortestOneMultiplicationWith, (.*~), shortestDisjointWithSize,
                           (.*++) )

-- | Applies the symmetric difference with the first argument in both components of the
-- second argument.

xorInBoth :: (SetOps q q, SetOps vec vec) => 
	Graph q vec a -> (Graph q vec a, Graph q vec a) -> (Graph q vec a, Graph q vec a)
xorInBoth g = f *** f where
  f = symDifference g

-- | Computes an augmenting path if such a path exists.
-- The first argument is the matching and the second one is its complement.	

augmentingPath :: 
	(HasVMM vec q, Mapping q, Complementable vec q, Intersectable vec vec) => 
		Graph q vec a 
		-> Graph q vec a 
		-> Maybe VPath
augmentingPath m cm = fmap (uncurry (flip stepRight)) (some result) where
  result = shortestOneMultiplicationWith (.*~) (fmap (const emptyPath) u .*~ cm) [m, cm] u'
  u      = withoutSuccessorsVec m
  u'     = withoutSuccessorsOuter m -- outer type for more efficient intersections.

-- | Augments a current matching given a graph to insert the augmenting path into
-- (typically the empty graph), the current matching, and the complement of the current matching.

augmentMatching ::
	(HasVMM vec q, Mapping q, Complementable vec q, SetOps vec vec, SetOps q q) => 
		Graph q vec () 
		-> Graph q vec () 
		-> Graph q vec ()
		-> Maybe (Graph q vec (), Graph q vec ())
augmentMatching aux m cm = fmap f (augmentingPath m cm) where
  f path    = xorInBoth augPath (m, cm) where
  	augPath = pathToSymGraph path aux

-- | This function computes a maximum matching and its complement in a symmetric, bipartite graph.
-- Both preconditions are not checked.

maximumMatchingBoth :: 
	(Mapping q, HasVMM vec q, Complementable vec q, SetOps vec vec, SetOps q q) =>
		Graph q vec a -> (Graph q vec (), Graph q vec ())
maximumMatchingBoth graph = go empty graph' where

	go     = stepwise2 (augmentMatching empty)
	-- this is the same as go m cm = maybe (m, cm) (uncurry go) (augmentMatching empty m cm)

	empty  = emptyGraph2 graph'
	graph' = void graph

-- | This function computes a maximum matching in a symmetric, bipartite graph.
-- Neither of these two preconditions is checked.	

maximumMatching :: 
	(Mapping q, MappingV vec, HasVMM vec q, Complementable vec q, SetOps vec vec, SetOps q q) =>
		Graph q vec a -> Graph q vec ()
maximumMatching = fst . maximumMatchingBoth

-- | Computes a list of pairwise disjoint shortest augmenting paths.
-- The list is maximal with respect to inclusion.

augmentingPaths :: 
	(HasVMM vec q, Mapping q, Complementable vec q, Intersectable vec vec, Traversable vec) => 
		Int 
		-> Graph q vec a 
		-> Graph q vec a 
		-> [VPath]
augmentingPaths n m cm = shortestDisjointWithSize n (u .*++ cm) [m, cm] u where
  u      = fmap (const []) (withoutSuccessorsVec m)
  u'     = withoutSuccessorsOuter m -- outer type for more efficient intersections.

-- | Augments the matching according to the Hopcroft-Karp strategy using a set of pairwise
-- disjoint shortest augmenting paths.

augmentMatchingHK :: 
	(HasVMM vec q, Mapping q, Complementable vec q, SetOps vec vec, SetOps q q, Traversable vec) =>
     Int 
     -> Graph q vec () 
     -> Graph q vec () 
     -> Graph q vec ()
     -> Maybe (Graph q vec (), Graph q vec ())
augmentMatchingHK n aux m cm 
	| null paths = Nothing
  | otherwise  = Just (augPaths `xorInBoth` (m, cm))
  where paths    = augmentingPaths n m cm
        augPaths = foldr pathToSymGraph aux paths

-- | Computes a maximum matching and its complement in a symmetric bipartite graph using the
-- strategy of Hopcroft and Karp.
-- Neither of the preconditions is checked.

maximumMatchingHKBoth :: 
	(HasVMM vec q, Mapping q, Complementable vec q, SetOps vec vec, SetOps q q, Traversable vec) =>
  	Graph q vec a -> (Graph q vec (), Graph q vec ())
maximumMatchingHKBoth graph = go empty graph' where

	go     = stepwise2 (augmentMatchingHK n empty)
	empty  = emptyGraph2 graph'
	graph' = void graph
	n      = numberOfVertices graph

-- | Computes a maximum matching and in a symmetric bipartite graph using the
-- strategy of Hopcroft and Karp.
-- Neither of the preconditions is checked.

maximumMatchingHK :: 
	(HasVMM vec q, Mapping q, Complementable vec q, SetOps vec vec, SetOps q q, Traversable vec) =>
  	Graph q vec a -> Graph q vec ()
maximumMatchingHK = fst . maximumMatchingHKBoth