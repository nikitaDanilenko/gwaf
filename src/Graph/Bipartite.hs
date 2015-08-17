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

{-# Language ScopedTypeVariables #-}

module Graph.Bipartite (

  ) where

import Algebraic.Matrix       ( toUnitMatrix, isEmptyMatrix, (.**=.), identityMatrix, HasVMM )
import Algebraic.Closure      ( starClosureOC )
import Auxiliary.KeyedClasses ( KeyFunctor )
import Auxiliary.Mapping      ( Mapping, MappingV )
import Auxiliary.SetOps       ( (//\), Intersectable, IntersectableHom, UnionableHom )
import Data.Foldable          ( Foldable )
import Graph.Graph            ( Graph )

-- | Maps a matrix to a Boolean unit matrix.

toBoolean :: (Functor q, Functor vec) => Graph q vec a -> Graph q vec Bool
toBoolean = toUnitMatrix

booleanIdentity :: (KeyFunctor q, Mapping vec) => Graph q vec a -> Graph q vec Bool
booleanIdentity = identityMatrix

-- | Checks whether a graph is bipartite by checking whether @g * star (g * g)@ is contained
-- in the complement of the identity matrix.
-- This test is \"slow\", because the star closure operation is cubic in the number of vertices.

isBipartiteSlow :: forall q vec a .
	(Mapping q, Foldable q, UnionableHom q, Intersectable q q,
   HasVMM vec q, UnionableHom vec, Intersectable vec vec) => 
	Graph q vec a -> Bool
isBipartiteSlow a = isEmptyMatrix (i //\ (ab .**=. starClosureOC (ab .**=. ab)))
  where ab = toBoolean a
        i  = booleanIdentity a -- :: Graph q vec Bool

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