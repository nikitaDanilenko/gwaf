----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.Paths
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides several auxiliary vector-matrix multiplications as well
-- as the reachability pattern that produces an intermediate list of reachability
-- steps.
-- Additionally,
-- the reachability scheme is applied to compute shortest paths between vertex
-- sets.

{-# Language MultiParamTypeClasses #-}

module Graph.Paths (
  
  -- * Multiplications

  (.*~),
  (.*~~),
  (.*++),
  (.*+++),

  -- * Auxiliaries

  VPath,
  
  ) where

import Data.Tree         ( Forest, Tree ( Node ) )

import Algebraic.Matrix  ( HasHetVMM, vecMatMult2, allUnion, leftmostUnion, bigunionWithE )
import Algebraic.Semiring ( Number )
import Auxiliary.General ( Arc )
import Auxiliary.SetOps  ( Intersectable, UnionableHom )
import Graph.Graph       ( Graph, Vertex )
import Graph.Path        ( Path, stepRight )

-- | An abbreviation for paths that consist of vertices.

type VPath = Path Vertex

-- | Path-prolonging multiplication.
-- This multiplication extends the given path by one step.

(.*~) :: HasHetVMM vec1 q vec2 vec3 => vec1 VPath -> Graph q vec2 a -> vec3 VPath
(.*~) = vecMatMult2 leftmostUnion (\i p _ -> p `stepRight` i)

-- | This multiplication extends all given path by exactly one step.

(.*~~) :: HasHetVMM vec1 q vec2 vec3 => vec1 [VPath] -> Graph q vec2 a -> vec3 [VPath]
(.*~~) = vecMatMult2 allUnion (\i ps _ -> map (`stepRight` i) ps)

-- | Assuming that a vector is labelled with a reachability forest at every index
-- this multiplication computes the reachability forest that is obtained by walking a single step
-- through the graph.

(.*++) :: HasHetVMM vec1 q vec2 vec3 => 
  vec1 (Forest Vertex) -> Graph q vec2 a -> vec3 (Forest Vertex)
(.*++) = vecMatMult2 allUnion (\i forest _ -> [Node i forest])

-- | A relative of @('.*++')@ that also collects the values along the edges.

(.*+++) :: HasHetVMM vec1 q vec2 vec3 => 
  vec1 (Forest (Arc a)) -> Graph q vec2 a -> vec3 (Forest (Arc a))
(.*+++) = vecMatMult2 allUnion (\i forest e -> [Node (i, e) forest])

-- | The underlying multiplication of this function is the following one.
-- It maps every value that is encountered in the adjacency list of a vertex to 1
-- and then uses numerical addition to add the resulting ones.

(.*#) :: (Num a, HasHetVMM vec1 q vec2 vec3) => vec1 (Number a) -> Graph q vec2 b -> vec3 (Number a)
(.*#) = vecMatMult2 (bigunionWithE (+)) (\_ _ _ -> 1)