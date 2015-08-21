----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.MaximumFlow
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a function for the computation of maximum flows in oriented networks.

module Graph.MaximumFlow (

	Network ( .. ),
	maximumFlowBoth,
	maximumFlow,
	minimumCut,
	balance

	) where

import Data.Foldable                ( Foldable )
import qualified Data.Foldable as F ( sum )

import Algebraic.Semiring           ( Tropical ( Tropical ), weight, zero, FindZero, GroupA, (.-.) )
import Algebraic.Matrix             ( (.++.), (.--.), transposeSquare, HasVMM, (.*+-) )
import Auxiliary.General            ( stepwise2, Key )
import Auxiliary.KeyedClasses       ( KeyFunctor )
import Auxiliary.Mapping            ( Mapping, toMapping, toMappingAs, toMappingWith, some )
import Auxiliary.SetOps             ( (/\\), Intersectable, Complementable, Unionable )
import Graph.Graph                  ( Graph, Vertex, pathToGraphWith, emptyGraph, successors,
                                      predecessors )
import Graph.Path                   ( Path, stepRight, emptyPath )
import Graph.Paths                  ( (.*~+), shortestOneGraphWith, VPath, 
	                                    reachableOneGraphWithEmpty )

-- | A 'Network' consists of a capacity function (given as a graph),
-- a designated source vertex, and a designated target vertex.

data Network q vec a = Network { capacity :: Graph q vec a, source, sink :: Vertex }

-- | Computes an augmenting path in the current residual graph.

fAugmentingPath
  :: (HasVMM vec q, Intersectable vec vec, Complementable vec q, KeyFunctor q, Ord n) =>
     Graph q vec n                -- ^ residual capacity
     -> vec (VPath, Tropical n)   -- ^ start vector
     -> vec a                     -- ^ target vector
     -> Maybe (Path Key, n)
fAugmentingPath cf start end = fmap finish (some result) where
	result             = shortestOneGraphWith (.*~+) start (fmap Tropical cf) end
	finish (i, (p, m)) = (p `stepRight` i, weight m)

-- | Augments a flow and its residual graph by computing an augmenting path.

augmentFlow :: 
	(HasVMM vec q, Mapping q, Unionable q q, Complementable vec q, Intersectable vec vec, 
	 Unionable vec q, Intersectable q q, FindZero n, GroupA n, Ord n) =>
	vec (VPath, Tropical n)  -- ^ start vector
	-> vec a                 -- ^ target vector
	-> Graph q vec n         -- ^ capacity
	-> Graph q vec n         -- ^ empty graph
	-> Graph q vec n         -- ^ flow
	-> Graph q vec n         -- ^ residual capacity
	-> Maybe (Graph q vec n, Graph q vec n)
augmentFlow start end c empty f cf = fmap (uncurry improve) (fAugmentingPath cf start end) where

	improve p eps = (f .++. up, (cf .--. up) .++. transposeSquare up) where
		up     = c /\\ (sigmap .--. transposeSquare sigmap)
		sigmap = pathToGraphWith (\_ _ -> eps) p empty

-- | Returns a flow and the residual capacity of a flow in the given oriented network.
-- This function implements the Edmonds-Karp strategy, which is to say that in every improvement
-- iteration one searches for a shortest augmenting path.
-- The precondition that the network is oriented (i.e. asymmetric) is not checked.

maximumFlowBoth :: 
	(HasVMM vec q, Mapping q, Unionable q q, Complementable vec q, Intersectable vec vec, 
	 Unionable vec q, Intersectable q q, FindZero n, GroupA n, Ord n) =>
	Network q vec n -> (Graph q vec n, Graph q vec n)
maximumFlowBoth (Network c s t) = stepwise2 (augmentFlow start end c empty) empty c where

	start = toMappingWith (emptyPath, zero) [s]
	end   = toMappingAs start [t]

	empty = emptyGraph c

-- | Returns a maximum flow in the given oriented network.
-- The precondition that the network is oriented is not checked.

maximumFlow :: 
	(HasVMM vec q, Mapping q, Unionable q q, Complementable vec q, Intersectable vec vec, 
	 Unionable vec q, Intersectable q q, FindZero n, GroupA n, Ord n) =>
		Network q vec n -> Graph q vec n
maximumFlow = fst . maximumFlowBoth

-- | Computes a minimum cut in the given oriented network.
-- The underlying strategy is the one of the Ford-Fulkerson theorem (max-flow-min-cut).
-- The precondition that the network is oriented is not checked.

minimumCut :: 
	(HasVMM vec q, Mapping q, Unionable q q, Complementable vec q, Intersectable vec vec, 
	 Unionable vec q, Intersectable q q, FindZero n, GroupA n, Ord n) =>
		Network q vec n -> vec ()
minimumCut n = reachableOneGraphWithEmpty (.*+-) (toMapping [source n]) (snd (maximumFlowBoth n))

-- | Computes the balance of a vertex in a graph, which is the sum of all outgoing values of a 
-- vertex minus the sum of all incoming values.

balance :: (HasVMM vec q, Mapping q, Unionable vec q, GroupA n, Num n) => 
	Graph q vec n -> Vertex -> n
balance g i = F.sum (successors g i) .-. F.sum (predecessors g i)