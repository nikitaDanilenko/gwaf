----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.Bipartite
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides tests for the bipartiteness of graphs.
-- Additionally, it provides a function that computes an actual bipartition in case the graph
-- is bipartite.

module Graph.Bipartite (
  isBipartite,
  isBipartiteSlow,
  isBipartiteSlow2,
  isWeakBipartition,
  findBipartition,
  componentwise
  ) where

import Control.Arrow          ( (***) )
import Data.Foldable          ( Foldable )
import Data.Maybe             ( isJust )

import Algebraic.Matrix       ( toUnitMatrix, isEmptyMatrix, (.**=.), identityMatrix, HasVMM,
                                (.*+-) )
import Algebraic.Closure      ( starClosureOC )
import Auxiliary.General      ( evensOdds )
import Auxiliary.KeyedClasses ( KeyFunctor )
import Auxiliary.Mapping      ( Mapping, MappingV, empty, isEmpty, keys, toMapping )
import Auxiliary.MonadicSet   ( runWithNewSize, ifInSet, includeAll )
import Auxiliary.SetOps       ( (//\), Intersectable, IntersectableHom, UnionableHom, Unionable,
                                (\\/), Complementable, bigunionLeft )
import Graph.Graph            ( Graph, Vertex, verticesList, numberOfVertices )
import Graph.Paths            ( stepsOneGraphWith )

-- | Maps a matrix to a Boolean unit matrix.

toBoolean :: (Functor q, Functor vec) => Graph q vec a -> Graph q vec Bool
toBoolean = toUnitMatrix

booleanIdentity :: (KeyFunctor q, Mapping vec) => Graph q vec a -> Graph q vec Bool
booleanIdentity = identityMatrix

-- | Computes the intersection of a vector with its successors.

step :: (HasVMM vec q, Intersectable vec vec) => vec a -> Graph q vec b -> vec a
step vec graph = vec .*+- graph //\ vec

-- | Checks whether a graph is bipartite by checking whether @g * star (g * g)@ is contained
-- in the complement of the identity matrix.
-- This test is \"slow\", because the star closure operation is cubic in the number of vertices.

isBipartiteSlow ::
	(Mapping q, Foldable q, UnionableHom q, Intersectable q q,
   HasVMM vec q, UnionableHom vec, Intersectable vec vec) => 
	Graph q vec a -> Bool
isBipartiteSlow a = isEmptyMatrix (i //\ (ab .**=. starClosureOC (ab .**=. ab)))
  where ab = toBoolean a
        i  = booleanIdentity a

-- | Checks whether a graph is bipartite by using the condition @g /\ star (g * g) == O@,
-- which is equivalent to the condition checked by 'isBipartiteSlow'.
-- The resulting operation is still cubic in the number of vertices,
-- because it uses the star closure operation.
-- However, there is one less multiplication necessary.

isBipartiteSlow2 :: 
	(Mapping q, Foldable q, UnionableHom q, Intersectable q q,
     HasVMM vec q, Intersectable vec vec, UnionableHom vec) =>
	Graph q vec a -> Bool
isBipartiteSlow2 a = isEmptyMatrix (ab //\ starClosureOC (ab .**=. ab))
	where ab = toBoolean a

-- | Checks whether two vectors constitute a \"weak\" bipartition in the sense that all edges
-- connect only vertices from two different sets.
-- However, there is no requirement that the two vectors are complementary to one another
-- with respect to the complete graph.

isWeakBipartition :: (HasVMM vec q, Intersectable vec vec, Unionable vec vec) => 
	Graph q vec b -> vec a -> vec a -> Bool
isWeakBipartition graph ls gs = isEmpty (step ls graph \\/ step gs graph)

-- | This function computes (lazily) the list of reachability steps for each individual vertex.
-- Then it traverses the resulting list left-to-right and removes the reachability steps of
-- those vertices that are already contained in a visited connected component.
-- The intermediate result is a list of reachability steps for each connected component,
-- where each such list is labelled with a representative of the component.
-- Finally, the supplied function is applied to each element in this intermediate result.

componentwise :: (Mapping q, HasVMM vec q, Complementable vec q) => 
	(Vertex -> [vec ()] -> c) -> Graph q vec b -> [c]
componentwise fun graph = runWithNewSize (numberOfVertices graph) (prune generated) where

  generated = map (\v -> (v, stepsOneGraphWith (.*+-) (toMapping [v]) graph)) (verticesList graph)

  prune []               = return []
  prune ((i, ls) : ilss) = ifInSet i 
  						    (prune ilss) 
  	                        (includeAll (concatMap keys ls) >> fmap (fun i ls :) (prune ilss))

-- | This function uses the even-odd-strategy to compute a special candidate for a bipartition.
-- If said candidate is a weak bipartition, it is a bipartition already;
-- otherwise the graph is not bipartite.
-- While seemingly complex, this function is at worst quadratic in the number of vertices.

findBipartition ::
	(Mapping q, HasVMM vec q, Complementable vec q, Intersectable vec vec, Unionable vec vec) => 
	Graph q vec a -> Maybe (vec (), vec ())
findBipartition graph | isWeakBipartition graph ls gs = Just (ls, gs)
                      | otherwise                     = Nothing
  where (ls, gs)   = (leftConcat *** leftConcat) (unzip (componentwise (const evensOdds) graph))
        leftConcat = bigunionLeft empty . concat

-- | Checks whether a graph is bipartite by searching for a bipartition candidate.
-- Contrary to 'isBipartiteSlow' and 'isBipartiteSlow2' this function is quadratic in the number
-- of vertices in the graph.

isBipartite :: 
	(Mapping q, HasVMM vec q, Complementable vec q, Intersectable vec vec, Unionable vec vec) => 
	Graph q vec a -> Bool
isBipartite = isJust . findBipartition