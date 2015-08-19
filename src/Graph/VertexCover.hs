----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.VertexCover
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides functions for the computations of minimum vertex covers in bipartite graphs
-- and an implementation of the approximation algorithm of Gavril and Yannakakis for the general
-- case.

module Graph.VertexCover (

	minimumVertexCover,
	minimumVertexCoverApprox

  ) where

import Data.Traversable      ( Traversable )

import Algebraic.Matrix      ( (.*+-), HasVMM )
import Auxiliary.Mapping     ( Mapping, isEmpty, empty, toMappingAs, toRow )
import Auxiliary.SetOps      ( (\\/), (//\), (\\), Intersectable, Complementable, SetOps )
import Graph.Bipartite       ( findBipartition )
import Graph.Graph           ( Graph, withoutSuccessorsVec, verticesVec )
import Graph.MaximumMatching ( maximumMatchingHKBoth )
import Graph.Paths           ( reachableOneMultiplicationWithEmpty, (.*<-) )

-- | Given two graphs @m@, @cm@ and a vector @start@ this function computes
-- those vertices that are reachable from @'withoutSuccessorsVec' m '//\' start@ via
-- @cm@-@m@-alternating paths.

alternatingReachable :: (HasVMM vec q, Mapping q, Intersectable vec vec, Complementable vec q) =>
	Graph q vec a -> Graph q vec a -> vec b -> vec ()
alternatingReachable m cm start = s \\/ s .*+- cm where
	s = reachableOneMultiplicationWithEmpty (.*+-) (withoutSuccessorsVec m //\ start) [cm, m]

-- | Computes a minimum vertex cover in a bipartite graphs according to the construction given
-- in the König theorem.
-- If the graph is not bipartite, the result is 'Nothing'.

minimumVertexCover :: 
	(HasVMM vec q, Mapping q, Complementable vec q, SetOps q q, SetOps vec vec, Traversable vec) =>
	Graph q vec a -> Maybe (vec ())
minimumVertexCover graph = fmap makeCover (findBipartition graph) where
	makeCover (a, b) = (a \\ r) \\/ (b //\ r) where
		r = uncurry alternatingReachable (maximumMatchingHKBoth graph) a

-- | Computes a vertex cover in an arbitrary graph that has at most twice the size of a minimum
-- vertex cover.
-- The implemented strategy is the one of Gavril and Yannakakis, also known as 
-- \"Greedy Vertex Cover\".

minimumVertexCoverApprox ::
	(HasVMM vec q, Mapping q, Intersectable vec vec, Complementable vec vec) => 
		Graph q vec a -> vec ()
minimumVertexCoverApprox graph = approximate (verticesVec graph) where
	approximate notC | isEmpty cands = empty
	                 | otherwise     = new \\/ approximate (notC \\ new)
	                 where cands  = notC .*<- graph //\ notC
	                       new    = toMappingAs cands [x, y]
	                       (x, y) = head (toRow cands)