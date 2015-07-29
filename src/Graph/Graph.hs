----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.Graph
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides functions that are specific for graphs.
-- In the presented implementation graphs are the same as (square) matrices,
-- which is why many matrix operations apply.
-- To avoid duplication,
-- we do not provide graph-theoretic names for all general purpose matrix functions,
-- but restrict this renaming to a minimum.

module Graph.Graph ( 

  -- * Graph data type and basic operations

  Graph,
  Vertex,
  verticesList,
  fmapAdjacencies,
  vertices,
  verticesVec,
  filterAdjacencies,
  withSuccessors,
  withoutSuccessorsList,
  withoutSuccessorsVec,
  successors,
  successorsWith,

  -- * Algebraic graph operations

  (/|\),

  ) where

import Algebraic.Matrix       ( Matrix, rowNumbers, matrix, (!!!), addValue, emptyMatrix, HasVMM,
                                mkVMMWith )
import Auxiliary.General      ( Key, Arc )
import Auxiliary.KeyedClasses ( KeyFunctor, fmapWithKey, ffilterWithKey, Lookup )
import Auxiliary.Mapping      ( Mapping, toMapping, isEmpty, keys, MappingV )
import Auxiliary.SetOps       ( IntersectableHom, intersectionWith )

-- | Graphs are a type synonym for (square) matrices.
-- The names of the parameters are a mnemonic for __q__uery and __vec__tor.

type Graph q vec a = Matrix q vec a

-- | Vertices are 'Key's.

type Vertex = Key

-- | Returns the vertices of a graph as a list.

verticesList :: Mapping q => Graph q vec a -> [Vertex]
verticesList = rowNumbers

-- | Returns the vertices of a graph as the outer graph layer,
-- where the labels of the vertices are computed using the supplied function.

fmapAdjacencies :: KeyFunctor q => (Vertex -> vec a -> b) -> Graph q vec a -> q b
fmapAdjacencies f = fmapWithKey f . matrix

-- | Returns the vertices of the graph as the outer graph layer without any
-- additional information.

vertices :: KeyFunctor q => Graph q vec a -> q ()
vertices = fmapAdjacencies (\_ _ -> ())

-- | Returns the vertices of the graph as the inner graph layer without any
-- additional information.

verticesVec :: (Mapping q, Mapping vec) => Graph q vec a -> vec ()
verticesVec = toMapping . verticesList

-- | Returns the vertices of

filterAdjacencies :: Mapping q => (Vertex -> vec a -> Bool) -> Graph q vec a -> q (vec a)
filterAdjacencies p = ffilterWithKey p . matrix

-- | Returns those vertices of a graph that have a non-empty adjacency list.
-- In symmetric graphs these are the non-isolated vertices.

withSuccessors :: (Mapping q, Mapping vec) => Graph q vec a -> [Vertex]
withSuccessors = keys . filterAdjacencies (const (not . isEmpty))

-- | Removes all vertices from the outer layer that have a non-empty adjacency list.

withoutSuccs :: (Mapping q, Mapping vec) => Graph q vec a -> q (vec a)
withoutSuccs = filterAdjacencies (const isEmpty)

-- | Returns the list of all vertices in the graph that have no successors.
-- In symmetric graphs these are the isolated vertices.

withoutSuccessorsList :: (Mapping q, Mapping vec) => Graph q vec a -> [Vertex]
withoutSuccessorsList = keys . withoutSuccs

-- | Returns a vector of those vertices in the graph that have no successors.

withoutSuccessorsVec :: (Mapping q, Mapping vec) => Graph q vec a -> vec ()
withoutSuccessorsVec = toMapping . withoutSuccessorsList

-- | Returns the adjacency list of a vertex in a graph.

successors :: (Lookup q, MappingV vec) => Graph q vec a -> Vertex -> vec a
successors = (!!!)
-- same as: successors i g = markWith () [i] .*-- g

-- | Computes the successors of a vertex and then applied the supplied fuction to the result.

successorsWith :: (Lookup q, MappingV vec) => (Vertex -> a -> b) -> Graph q vec a -> Vertex -> vec b
successorsWith op g v = fmap (op v) (successors g v)

-- | Adds an edge to a graph, overwriting the possibly existing label.

addEdge :: (KeyFunctor q, MappingV vec) => Vertex -> Vertex -> a -> Graph q vec a -> Graph q vec a
addEdge = addValue

-- | Returns the empty graph of a specific inner type.

emptyGraph :: (Functor q, MappingV vec) => Graph q vec' a -> Graph q vec a
emptyGraph = emptyMatrix

-- predecessors :: 


-- | Discrete intersection of two graphs.
-- The result contains '()'s at precisely those positions,
-- at which both graphs have entries.

(/|\) :: (IntersectableHom q, IntersectableHom vec) => 
  Graph q vec a -> Graph q vec b -> Graph q vec ()
(/|\) = intersectionWith (\_ _ -> ())

