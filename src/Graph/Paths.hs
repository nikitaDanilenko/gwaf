----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.Paths
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
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

  (.*~°),
  (.*~),
  (.*~~°),
  (.*~~),
  (.*++°),
  (.*++),
  (.*+++°),
  (.*+++),
  (.*#°),
  (.*#),
  (.*~+°),
  (.*~+),

  -- * Reachability functions

  stepsSchemeWith,
  stepsFirstVerticesWith,
  stepsOneMultiplicationWith,
  stepsOneGraphWith,

  reachableWith,
  reachableWithEmpty,
  reachableFirstVerticesWith,
  reachableFirstVerticesWithEmpty,
  reachableOneMultiplicationWith,
  reachableOneMultiplicationWithEmpty,
  reachableOneGraphWith,
  reachableOneGraphWithEmpty,

  -- * Shortest path functions

  shortestSchemeWith,
  shortestFirstVerticesWith,
  shortestOneMultiplicationWith,
  shortestOneGraphWith,
  reachabilityForest,
  reachabilityForestSimple,
  reachabilityForestOneGraph,
  reachabilityForestOneGraphSimple,

  shortestDisjointWith,
  shortestDisjointSimpleWith,
  shortestDisjoint,
  shortestDisjointWithSize,
  shortestDisjointWithFirstSize,
  shortestDisjointUpToStart,
  shortestDisjointUpToStartWithSize,
  shortestDisjointUpToStartWithFirstSize,
  shortestDisjointUpToEnd,
  shortestDisjointUpToEndWithSize,
  shortestDisjointUpToEndWithFirstSize,
  shortestDisjointUpToStartEnd,
  shortestDisjointUpToStartEndWithSize,
  shortestDisjointUpToStartEndWithFirstSize,

  -- * Auxiliaries

  VPath,
  
  ) where

import Data.Traversable       ( Traversable )
import Data.Tree              ( Forest, Tree ( Node ) )

import Algebraic.Matrix       ( HasVMM, HasHetVMM, vecMatMult2, allUnion, leftmostUnion,
                                bigunionWithE )
import Algebraic.Semiring     ( Number, SemigroupA, (.+.) )
import Auxiliary.General      ( Arc )
import Auxiliary.KeyedClasses ( KeyFunctor, addKeys )
import Auxiliary.Mapping      ( MappingV, Mapping, empty, isEmpty, toRow )
import Auxiliary.SetOps       ( Intersectable, Unionable, UnionableHom, Complementable,
                                (\\\), (//\), bigunionLeft )
import Graph.Graph            ( Graph, Vertex, vertices, numberOfVertices )
import Graph.Path             ( Path, stepRight )
import Graph.Prune            ( pruneDisjoint, pruneDisjointUpToStart, pruneDisjointUpToEnd,
                                pruneDisjointUpToStartEnd )

-- | An abbreviation for paths that consist of vertices.

type VPath = Path Vertex

-- | Path-prolonging multiplication.
-- This multiplication extends the given path by one step.

(.*~°) :: HasHetVMM vec1 q vec2 vec3 => vec1 VPath -> Graph q vec2 a -> vec3 VPath
(.*~°) = vecMatMult2 leftmostUnion (\i p _ -> p `stepRight` i)

-- | A homogeneous variant of @('.*~°')@.

(.*~) :: HasVMM vec q => vec VPath -> Graph q vec a -> vec VPath
(.*~) = (.*~°)

-- | This multiplication extends all given path by exactly one step.

(.*~~°) :: HasHetVMM vec1 q vec2 vec3 => vec1 [VPath] -> Graph q vec2 a -> vec3 [VPath]
(.*~~°) = vecMatMult2 allUnion (\i ps _ -> map (`stepRight` i) ps)

-- | A homogeneous variant of @('.*~~°')@.

(.*~~) :: HasVMM vec q => vec [VPath] -> Graph q vec a -> vec [VPath]
(.*~~) = (.*~~°)

-- | Assuming that a vector is labelled with a reachability forest at every index
-- this multiplication computes the reachability forest that is obtained by walking a single step
-- through the graph.

(.*++°) :: HasHetVMM vec1 q vec2 vec3 => 
  vec1 (Forest Vertex) -> Graph q vec2 a -> vec3 (Forest Vertex)
(.*++°) = vecMatMult2 allUnion (\i forest _ -> [Node i forest])

-- | A homogeneous variant of @('.*++°')@.

(.*++) :: HasVMM vec q => vec (Forest Vertex) -> Graph q vec a -> vec (Forest Vertex)
(.*++) = (.*++°)

-- | A relative of @('.*++°')@ that also collects the values along the edges.

(.*+++°) :: HasHetVMM vec1 q vec2 vec3 => 
  vec1 (Forest (Arc a)) -> Graph q vec2 a -> vec3 (Forest (Arc a))
(.*+++°) = vecMatMult2 allUnion (\i forest e -> [Node (i, e) forest])

-- | A homogeneous variant of @('.*+++°')@.

(.*+++) :: HasVMM vec q => vec (Forest (Arc a)) -> Graph q vec a -> vec (Forest (Arc a))
(.*+++) = (.*+++°)

-- | The underlying multiplication of this function is the following one.
-- It maps every value that is encountered in the adjacency list of a vertex to 1
-- and then uses numerical addition to add the resulting ones.

(.*#°) :: (Num a, HasHetVMM vec1 q vec2 vec3) => 
  vec1 (Number a) -> Graph q vec2 b -> vec3 (Number a)
(.*#°) = vecMatMult2 (bigunionWithE (+)) (\_ _ _ -> 1)

-- | A homogeneous variant of @('.*#°')@.

(.*#) :: (Num a, HasVMM vec q) => vec (Number a) -> Graph q vec b -> vec (Number a)
(.*#) = (.*#°)

-- | Conceptually this is the vector matrix multiplication in the product semiring
-- of the semiring of paths and another semiring.
-- The first component extends a single path,
-- while the second component collects the sum of all values along this path.
-- In the particular case of the tropical semiring,
-- the sum of all values along the path is precisely the minimum value along the path.

(.*~+°) :: (HasHetVMM vec1 q vec2 vec3, SemigroupA asg) => 
  vec1 (VPath, asg) -> Graph q vec2 asg -> vec3 (VPath, asg)
(.*~+°) = vecMatMult2 leftmostUnion (\i (p, m) y -> (p `stepRight` i, y .+. m))

-- | A homogeneous variant of @('.*~+°')@.

(.*~+) :: (HasVMM vec q, SemigroupA asg) => vec (VPath, asg) -> Graph q vec asg -> vec (VPath, asg)
(.*~+) = (.*~+°)

-- | A fully parametric reachability scheme.
-- It takes a vector of initially unvisited vertices,
-- an initial vector, a list of multiplications, and a list of graphs,
-- and computes the reachability steps from the initial vector through the graph list
-- by applying the given multiplications in sequence.
-- The lists of multiplications and graphs are trimmed to the length of the shorter
-- list to obtain a symmetry in the sense that every reachability step is computed
-- in the exact same fashion and not with possibly different multiplications or graphs.

stepsSchemeWith :: (Complementable vec q', Intersectable vec q', Mapping vec) =>
     q' c                                 -- ^ The vector of vertex candidates.
  -> [vec a -> Graph q vec' b -> vec a]   -- ^ The list of multiplications applied in sequence.
  -> vec a                                -- ^ The start vector.
  -> [Graph q vec' b]                     -- ^ The list of graphs traversed in sequence.
  -> [vec a]
stepsSchemeWith _     _  start [] = [start]
stepsSchemeWith vs mults start gs = go start vs where

  go current unseen | isEmpty current = []
                    | otherwise       = current : go current' unseen' where
                    unseen'  = unseen \\\ current
                    current' = foldl (flip ($)) current mgs //\ unseen'
  
  mgs = zipWith flip mults gs

-- | A variant of 'stepsSchemeWith', but the vertices are no longer supplied as a parameter.
-- Instead, the vertices of the first graph are used. 

stepsFirstVerticesWith :: 
  (KeyFunctor q, Intersectable vec q, Complementable vec q, Mapping vec) =>
     [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> [vec a]
stepsFirstVerticesWith ms start gs = stepsSchemeWith (vertices (head gs)) ms start gs

-- | A variant of 'stepsFirstVerticesWith' that uses only a single multiplication.

stepsOneMultiplicationWith :: 
  (KeyFunctor q, Intersectable vec q, Complementable vec q, Mapping vec) =>
     (vec a -> Graph q vec' b -> vec a) -- ^ The multiplication applied in every step.
  -> vec a                              -- ^ The start vector.
  -> [Graph q vec' b]                   -- ^ The list of graphs traversed in sequence.
  -> [vec a]
stepsOneMultiplicationWith = stepsFirstVerticesWith . repeat

-- | Computes the reachability steps through a single graph using a single multiplication.

stepsOneGraphWith ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, Mapping vec) =>
     (vec a -> Graph q vec' b -> vec a)  -- ^ The multiplication applied in every step.
  -> vec a                               -- ^ The start vector.
  -> Graph q vec' b                      -- ^ The graph that is stepped through in every step.
  -> [vec a]
stepsOneGraphWith mult start g = stepsFirstVerticesWith [mult] start [g]

-- | This function computes all reachability steps using 'stepsSchemeWith'
-- and then performs an iterated union of these steps and the given container.

reachableWith :: 
  (Intersectable vec q', Complementable vec q', Unionable vec m, Mapping vec) =>
     m a                                 -- ^ The container all reachability steps 
                                         --   are inserted into.
  -> q' c                                -- ^ The vector of vertex candidates.
  -> [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> m a
reachableWith c vs ms s gs = bigunionLeft c (stepsSchemeWith vs ms s gs)

-- | A variant of 'reachableWith' that uses the empty container 
-- for the collection of the reachability steps.

reachableWithEmpty :: 
  (Intersectable vec q', Complementable vec q', Unionable vec m, Mapping vec, MappingV m) =>
     q' c                                -- ^ The vector of vertex candidates.
  -> [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> m a
reachableWithEmpty = reachableWith empty

-- | This function computes all reachability steps using 'stepsFirstVerticesWith'
-- and then performs an iterated union of these steps and the given container.

reachableFirstVerticesWith ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, Unionable vec m, Mapping vec) =>
     m a                                 -- ^ The container all reachability steps 
                                         --   are inserted into.
  -> [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> m a
reachableFirstVerticesWith c ms s gs = bigunionLeft c (stepsFirstVerticesWith ms s gs)

-- | A variant of 'reachableFirstVerticesWith' that uses the empty container
-- for the collection of the reachability steps.

reachableFirstVerticesWithEmpty ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, 
   Unionable vec m, Mapping vec, MappingV m) =>
     [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> m a
reachableFirstVerticesWithEmpty = reachableFirstVerticesWith empty

-- | This function computes all reachability steps using 'stepsOneMultiplicationWith'
-- and then performs an iterated union of these steps and the given container.

reachableOneMultiplicationWith ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, Mapping vec, Unionable vec m) =>
     m a                                -- ^ The container all reachability steps 
                                        --   are inserted into.
  -> (vec a -> Graph q vec' b -> vec a) -- ^ The multiplication applied in every step.
  -> vec a                              -- ^ The start vector.
  -> [Graph q vec' b]                   -- ^ The list of graphs traversed in sequence.
  -> m a
reachableOneMultiplicationWith c m s gs = bigunionLeft c (stepsOneMultiplicationWith m s gs)

-- | A variant of 'reachableOneMultiplicationWith' that uses the empty container for the
-- collection of the reachability steps.

reachableOneMultiplicationWithEmpty ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, 
   Mapping vec, Unionable vec m, MappingV m) =>
     (vec a -> Graph q vec' b -> vec a) -- ^ The multiplication applied in every step.
  -> vec a                              -- ^ The start vector.
  -> [Graph q vec' b]                   -- ^ The list of graphs traversed in sequence.
  -> m a
reachableOneMultiplicationWithEmpty = reachableOneMultiplicationWith empty

-- | This function computes all reachability steps using 'stepsOneGraphWith'
-- and then performs an iterated union of these steps and the given container.

reachableOneGraphWith ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, Unionable vec m, Mapping vec) =>
     m a                                 -- ^ The container all reachability steps 
                                         --   are inserted into.  
  -> (vec a -> Graph q vec' b -> vec a)  -- ^ The multiplication applied in every step.
  -> vec a                               -- ^ The start vector.
  -> Graph q vec' b                      -- ^ The graph that is stepped through in every step.
  -> m a
reachableOneGraphWith c m s g = bigunionLeft c (stepsOneGraphWith m s g)

-- | A variant of 'reachableOneGraphWith' that uses the empty container for the
-- collection of the reachability steps.

reachableOneGraphWithEmpty ::
  (KeyFunctor q, Intersectable vec q, Complementable vec q, 
   Unionable vec m, Mapping vec, MappingV m) =>
     (vec a -> Graph q vec' b -> vec a)  -- ^ The multiplication applied in every step.
  -> vec a                               -- ^ The start vector.
  -> Graph q vec' b                      -- ^ The graph that is stepped through in every step.
  -> m a
reachableOneGraphWithEmpty = reachableOneGraphWith empty

-- | Given a target vector and a list of vectors this function computes the intersections of the
-- target vector with every vector in the list and drops those intersections that are empty.
-- It then returns the first non-empty intersection, if it exists and the empty vector otherwise.

dropEmptyIntersections :: (Intersectable vec q, MappingV vec) => [vec a] -> q b -> vec a
dropEmptyIntersections steps target = head (dropWhile isEmpty (map (//\ target) steps) ++ [empty])

-- | Computes the first non-empty intersection of the 
-- fully parametric reachability steps returned by 'stepsSchemeWith' with the target vector,
-- if such an intersection exists and returns an empty vector otherwise.

shortestSchemeWith :: 
  (Complementable vec q', Intersectable vec q', Intersectable vec q'', MappingV vec) =>
     q' a                                -- ^ The vector of vertex candidates.
  -> [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> q'' c                               -- ^ The target vector.
  -> vec a
shortestSchemeWith vs mults start gs = dropEmptyIntersections (stepsSchemeWith vs mults start gs)

-- | Computes the first non-empty intersection of the reachability steps computed
-- by the function 'stepsFirstVerticesWith' with the target vector,
-- if such an intersection exists and returns the empty vector otherwise.

shortestFirstVerticesWith ::
  (KeyFunctor q, Complementable vec q, Intersectable vec q, Intersectable vec q', MappingV vec) =>
     [vec a -> Graph q vec' b -> vec a]  -- ^ The list of multiplications applied in sequence.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> q' c                                -- ^ The target vector.
  -> vec a
shortestFirstVerticesWith mults start gs = 
  dropEmptyIntersections (stepsFirstVerticesWith mults start gs)

-- | Computes the first non-empty intersection of the reachability steps computed
-- by the function 'stepsOneMultiplicationWith' with the target vector,
-- if such an intersection exists and returns the empty vector otherwise.

shortestOneMultiplicationWith ::
  (KeyFunctor q, Intersectable vec q, Intersectable vec q', Complementable vec q, MappingV vec) =>
     (vec a -> Graph q vec' b -> vec a)  -- ^ The multiplication applied in every step.
  -> vec a                               -- ^ The start vector.
  -> [Graph q vec' b]                    -- ^ The list of graphs traversed in sequence.
  -> q' c                                -- ^ The target vector.
  -> vec a
shortestOneMultiplicationWith mult start gs = 
  dropEmptyIntersections (stepsOneMultiplicationWith mult start gs)

-- | Computes the first non-empty intersection of the reachability steps computed
-- by the function 'stepsOneGraphWith' with the target vector,
-- if such an intersection exists and returns the empty vector otherwise.

shortestOneGraphWith ::
  (KeyFunctor q, Intersectable vec q, Intersectable vec q', Complementable vec q, MappingV vec) =>
     (vec a -> Graph q vec' b -> vec a)
  -> vec a
  -> Graph q vec' b
  -> q' c
  -> vec a
shortestOneGraphWith mult start g = dropEmptyIntersections (stepsOneGraphWith mult start g)

-- | Auxiliary function for the outermost call in shortestDisjoint*.

shortestDisjointOuter :: KeyFunctor vec => 
     (   (Vertex, Vertex) 
      -> vec (Vertex, Forest Vertex) 
      -> [VPath])
  -> Vertex
  -> Vertex
  -> vec (Forest Vertex)
  -> [VPath]
shortestDisjointOuter strategy lb ub = strategy (lb, ub) . addKeys

-- | This function takes a pruning strategy for the reachability forest and computes a list
-- of paths such that the following hold: 
-- * the paths are disjoint in the sense specified by the pruning strategy;
-- * the list of paths is maximal with respect to inclusion.

shortestDisjointWith :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q',
     Complementable vec q, KeyFunctor q) =>
     (   (Vertex, Vertex) 
      -> vec (Vertex, Forest Vertex) 
      -> [VPath])                 -- ^ The pruning strategy.
  -> Vertex                             -- ^ The lower range bound.
  -> Vertex                             -- ^ The upper range bound.
  -> vec (Forest Vertex)                -- ^ The start vector.
  -> [Graph q vec' a]                   -- ^ The list of graphs traversed in sequence.
  -> q' b                               -- ^ The target vector.
  -> [VPath]
shortestDisjointWith strategy lb ub start gs target =
  shortestDisjointOuter strategy lb ub (reachabilityForest start gs target)

-- | A variant of 'shortestDisjointWith' that replaces all values in the start vector with
-- the empty forest.

shortestDisjointSimpleWith :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q',
     Complementable vec q, KeyFunctor q) =>
     (   (Vertex, Vertex) 
      -> vec (Vertex, Forest Vertex) 
      -> [VPath])                 -- ^ The pruning strategy.
  -> Vertex                             -- ^ The lower range bound.
  -> Vertex                             -- ^ The upper range bound.
  -> vec a                              -- ^ The start vector.
  -> [Graph q vec' b]                   -- ^ The list of graphs traversed in sequence.
  -> q' c                               -- ^ The target vector.
  -> [VPath]
shortestDisjointSimpleWith strategy lb ub start gs target =
  shortestDisjointOuter strategy lb ub (reachabilityForestSimple start gs target)

-- | This function computes a list of pairwise disjoint shortest paths from the start vector to
-- the target vector.
-- The list is maximal with respect to inclusion.

shortestDisjoint :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ First considered vertex.
  -> Vertex            -- ^ Last considered vertex.
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjoint = shortestDisjointSimpleWith pruneDisjoint

-- | A variant of 'shortestDisjoint' that takes a number of vertices in the traversed graphs
-- rather than a lower and an upper bound.

shortestDisjointWithSize :: 
     (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q',
      Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ The number of vertices in the graph(s).
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointWithSize n = shortestDisjoint 0 (n - 1)

-- | A variant of 'shortestDisjointWithSize' that uses the number of vertices in the first
-- graph in the list of traversed graphs.

shortestDisjointWithFirstSize :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, Mapping q) =>
     vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointWithFirstSize start gs = 
    shortestDisjointWithSize (numberOfVertices (head gs)) start gs

-- | This function computes a list of shortest paths from the start vector to
-- the target vector that are pairwise disjoint up to their first vertex.
-- The list is maximal with respect to inclusion.

shortestDisjointUpToStart :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ First considered vertex.
  -> Vertex            -- ^ Last considered vertex.
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToStart = shortestDisjointSimpleWith pruneDisjointUpToStart

-- | A variant of 'shortestDisjointUpToStart' that takes a number of 
-- vertices in the traversed graphs rather than a lower and an upper bound.

shortestDisjointUpToStartWithSize :: 
     (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q',
      Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ The number of vertices in the graph(s).
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToStartWithSize n = shortestDisjointUpToStart 0 (n - 1)

-- | A variant of 'shortestDisjointUpToStartWithSize' that uses the number of vertices in the first
-- graph in the list of traversed graphs.

shortestDisjointUpToStartWithFirstSize :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, Mapping q) =>
     vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToStartWithFirstSize start gs = 
    shortestDisjointUpToStartWithSize (numberOfVertices (head gs)) start gs

-- | This function computes a list of shortest paths from the start vector to
-- the target vector that are pairwise disjoint up to their last vertex.
-- The list is maximal with respect to inclusion.

shortestDisjointUpToEnd :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ First considered vertex.
  -> Vertex            -- ^ Last considered vertex.
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToEnd = shortestDisjointSimpleWith pruneDisjointUpToEnd

-- | A variant of 'shortestDisjointUpToEnd' that takes a number of 
-- vertices in the traversed graphs rather than a lower and an upper bound.

shortestDisjointUpToEndWithSize :: 
     (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q',
      Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ The number of vertices in the graph(s).
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToEndWithSize n = shortestDisjointUpToEnd 0 (n - 1)

-- | A variant of 'shortestDisjointUpToEndWithSize' that uses the number of vertices in the first
-- graph in the list of traversed graphs.

shortestDisjointUpToEndWithFirstSize :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, Mapping q) =>
     vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToEndWithFirstSize start gs = 
    shortestDisjointUpToEndWithSize (numberOfVertices (head gs)) start gs

-- | This function computes a list of shortest paths from the start vector to
-- the target vector that are pairwise disjoint up to their first and last vertex.
-- The list is maximal with respect to inclusion.

shortestDisjointUpToStartEnd :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ First considered vertex.
  -> Vertex            -- ^ Last considered vertex.
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToStartEnd = shortestDisjointSimpleWith pruneDisjointUpToStartEnd

-- | A variant of 'shortestDisjointUpToStartEnd' that takes a number of 
-- vertices in the traversed graphs rather than a lower and an upper bound.

shortestDisjointUpToStartEndWithSize :: 
     (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q',
      Complementable vec q, KeyFunctor q) =>
     Vertex            -- ^ The number of vertices in the graph(s).
  -> vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToStartEndWithSize n = shortestDisjointUpToStartEnd 0 (n - 1)

-- | A variant of 'shortestDisjointUpToStartEndWithSize' that uses the number of vertices in the
-- first graph in the list of traversed graphs.

shortestDisjointUpToStartEndWithFirstSize :: 
    (Traversable vec, HasHetVMM vec q vec' vec, Intersectable vec q', 
     Complementable vec q, Mapping q) =>
     vec a             -- ^ The start vector.
  -> [Graph q vec' b]  -- ^ The list of graphs traversed in sequence.
  -> q' c              -- ^ The target vector.
  -> [VPath]
shortestDisjointUpToStartEndWithFirstSize start gs = 
    shortestDisjointUpToStartEndWithSize (numberOfVertices (head gs)) start gs

-- | Computes the shortest reachability forest through a list of graphs.

reachabilityForest :: 
  (HasHetVMM vec q vec' vec, Intersectable vec q', Complementable vec q, KeyFunctor q)
  => vec (Forest Vertex) -> [Graph q vec' b] -> q' c -> vec (Forest Vertex)
reachabilityForest = shortestOneMultiplicationWith (.*++°)

-- | Similar to 'reachabilityForest', but overwrites all values in the start vector with the empty forest.

reachabilityForestSimple ::
  (HasHetVMM vec q vec' vec, Intersectable vec q', Complementable vec q, KeyFunctor q)
  => vec a -> [Graph q vec' b] -> q' c -> vec (Forest Vertex)
reachabilityForestSimple = reachabilityForest . fmap (const [])

-- | Computes the shortest reachability forest through a single graph.

reachabilityForestOneGraph :: 
  (HasHetVMM vec q vec' vec, Intersectable vec q', Complementable vec q, KeyFunctor q)
  => vec (Forest Vertex) -> Graph q vec' b -> q' c -> vec (Forest Vertex)
reachabilityForestOneGraph = shortestOneGraphWith (.*++°)

-- | Similar to 'reachabilityForestOneGraph',
-- but overwrites all values in the start vector with the empty forest.

reachabilityForestOneGraphSimple ::
  (HasHetVMM vec q vec' vec, Intersectable vec q', Complementable vec q, KeyFunctor q)
  => vec a -> Graph q vec' b -> q' c -> vec (Forest Vertex)
reachabilityForestOneGraphSimple = reachabilityForestOneGraph . fmap (const [])