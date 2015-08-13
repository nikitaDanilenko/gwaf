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
  GraphLL,
  GraphLI,
  GraphIL,
  GraphII,
  GraphAL,
  GraphAI,
  VecL,
  VecI,
  VecA,

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
  predecessors,
  predecessorsWith,

  -- * Algebraic graph operations

  (/|\),

  ) where

import Data.IntMap            ( IntMap )

import Algebraic.Matrix       ( Matrix, rowNumbers, matrix, (!!!), addValue, emptyMatrix, HasVMM,
                                transposeSquare )
import Auxiliary.AList        ( AList )
import Auxiliary.General      ( Key, Arc )
import Auxiliary.KeyedClasses ( KeyFunctor, fmapWithKey, ffilterWithKey, Lookup )
import Auxiliary.Mapping      ( Mapping, toMapping, isEmpty, keys, MappingV )
import Auxiliary.SafeArray    ( SafeArray )
import Auxiliary.SetOps       ( IntersectableHom, intersectionWith, Unionable )

-- | Graphs are a type synonym for (square) matrices.
-- The names of the parameters are a mnemonic for __q__uery and __vec__tor.

type Graph q vec a = Matrix q vec a

type GraphLL a = Graph AList     AList  a
type GraphLI a = Graph AList     IntMap a
type GraphIL a = Graph IntMap    AList  a
type GraphII a = Graph IntMap    IntMap a
type GraphAL a = Graph SafeArray AList  a
type GraphAI a = Graph SafeArray IntMap a

type VecL a = AList     a
type VecI a = IntMap    a
type VecA a = SafeArray a

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

-- | Computes the parametrised adjacency of a vertex and applies the supplied function
-- to every value in the result vector.

onAdjacency :: Functor vec => 
  (Graph q vec a -> Vertex -> vec a) -> (Vertex -> a -> b) -> Graph q vec a -> Vertex -> vec b
onAdjacency adj op g v = fmap (op v) (adj g v)

-- | Computes the successors of a vertex and then applies the supplied fuction to the result.

successorsWith :: (Lookup q, MappingV vec) => (Vertex -> a -> b) -> Graph q vec a -> Vertex -> vec b
successorsWith = onAdjacency successors

-- | Computes the predecessors of a vertex in a graph.
-- This operation uses the transposition of a matrix,
-- which results in a higher asymptotic complexity for larger vertex values.
-- The worst-case complexity is quadratic in the number of vertices in the graph.

predecessors :: (Unionable vec q, HasVMM vec q, Mapping q) => Graph q vec a -> Vertex -> vec a
predecessors = successors . transposeSquare

-- | Computes the predecessors of a vertex and applies the given function to every
-- value in the result vector.

predecessorsWith :: (Unionable vec q, HasVMM vec q, Mapping q) 
  => (Vertex -> a -> b) -> Graph q vec a -> Vertex -> vec b
predecessorsWith = onAdjacency predecessors

-- | Adds an edge to a graph, overwriting the possibly existing label.

addEdge :: (KeyFunctor q, MappingV vec) => Vertex -> Vertex -> a -> Graph q vec a -> Graph q vec a
addEdge = addValue

-- | Returns the empty graph of a specific inner type.

emptyGraph :: (Functor q, MappingV vec) => Graph q vec' a -> Graph q vec a
emptyGraph = emptyMatrix

-- | Discrete intersection of two graphs.
-- The result contains '()'s at precisely those positions,
-- at which both graphs have entries.

(/|\) :: (IntersectableHom q, IntersectableHom vec) => 
  Graph q vec a -> Graph q vec b -> Graph q vec ()
(/|\) = intersectionWith (\_ _ -> ())