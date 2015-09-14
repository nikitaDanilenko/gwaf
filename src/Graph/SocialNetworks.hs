----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.SocialNetworks
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides predicates to check graphs for being clustered, balanced,
-- and to compute the betweenness centrality of vertices.
-- The underlying theory is described in
--
-- * Vladimir Batagelj, Semirings for Social Network Analysis,
--   Journal of Mathematical Sociology,
--   <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.332.1968&rep=rep1&type=pdf available here>

module Graph.SocialNetworks (

	-- * Predicates

	isClustered,
	isClustered2,
	isBalanced,

	-- * Clustering

	findClusteringCandidate,

	-- * Betweenness

	betweenness,
	betweennessRow,
	
	-- * Auxiliary

	positive,
	negative,

	) where

import Control.Arrow          ( (&&&) )
import Data.Foldable          ( Foldable )
import Data.Monoid            ( Sum ( Sum ), getSum )
import Data.Ratio             ( Rational, (%) )

import Algebraic.Closure      ( starSymmetricClosureC, kleeneClosureC )
import Algebraic.Matrix       ( Matrix, toUnitMatrix, filterMatrix, isEmptyMatrix, identityMatrix,
	                              symmetricClosureC, HasVMM, (.@.) )
import Algebraic.PathAlgebra  ( Clustered ( P, N ), Balanced ( AllPositive, AllNegative ),
                                Sign ( Pos, Neg ), Geodesic ( Geodesic ), geodesicLength,
                                geodesicWeight )
import Algebraic.Structures   ( KleeneAlgebraC, addTropical, tropicalToNum, Tropical ( Tropical ),
                                Number ( Number ), number )

import Auxiliary.General      ( (<.>), Row )
import Auxiliary.KeyedClasses ( KeyMaybeFunctor )
import Auxiliary.Mapping      ( Mapping, MappingV, fromRow )
import Auxiliary.SetOps       ( (//\), UnionableHom, Unionable, Intersectable, Complementable )
import Graph.Graph            ( Graph, verticesList )
import Graph.Paths            ( weaklyConnectedComponents )


-- | Tests whether a given graph is clustered using a relational approach.

isClustered :: 
	(Unionable vec q, UnionableHom vec, Intersectable vec vec,
	 Foldable q, UnionableHom q, Intersectable q q, Mapping q, HasVMM vec q)
	 => Matrix q vec Sign -> Bool
isClustered m = isEmptyMatrix (starSymmetricClosureC (toBool (positive m)) //\ toBool (negative m))

-- | Keeps only the positive entries in a given matrix.

positive :: (Functor q, KeyMaybeFunctor vec) => Matrix q vec Sign -> Matrix q vec Sign
positive = filterMatrix (Pos ==)

-- | Keeps only the negative entries in a given matrix.

negative :: (Functor q, KeyMaybeFunctor vec) => Matrix q vec Sign -> Matrix q vec Sign
negative = filterMatrix (Neg ==)

-- | Turns a given matrix into a Boolean unit matrix, where all filled positions contain
-- the value 'True'.

toBool :: (Functor q, Functor vec) => Matrix q vec a -> Matrix q vec Bool
toBool = toUnitMatrix

-- | Tests whether a graph is clustered using an algebraic approach that computes the star closure
-- in the 'Clustered' Kleene algebra as an intermediate step.

isClustered2 :: (UnionableHom q, Mapping q, Intersectable q q, 
	 Unionable vec q, HasVMM vec q, 
	 UnionableHom vec, Intersectable vec vec) => 
			Graph q vec Sign -> Bool
isClustered2 = isStarDiagonalOne signToClustered

-- | Checks whether the star closure of the symmetric closure of a matrix contains ones at
-- every position along its main diagonal.

isStarDiagonalOne :: 
	(UnionableHom q, Mapping q, Intersectable q q, 
	 Unionable vec q, HasVMM vec q, 
	 UnionableHom vec, Intersectable vec vec, KleeneAlgebraC ka, Eq ka) => 
		(a -> ka) -> Matrix q vec a -> Bool
isStarDiagonalOne embed m = starSymmetricClosureC (fmap embed m) //\ i == i where
	i = identityMatrix m

-- | Interprets 'Sign' values as elements of the 'Clustered' Kleene algebra.

signToClustered :: Sign -> Clustered
signToClustered Pos = P
signToClustered Neg = N

-- | Interpres 'Sign' values as elements of the 'Balanced' Kleene algebra.

signToBalanced :: Sign -> Balanced
signToBalanced Pos = AllPositive
signToBalanced Neg = AllNegative

-- | Tests whether a graph is balanced.

isBalanced :: (UnionableHom q, Mapping q, Intersectable q q, 
	 Unionable vec q, HasVMM vec q, 
	 UnionableHom vec, Intersectable vec vec) => 
		Graph q vec Sign -> Bool
isBalanced = isStarDiagonalOne signToBalanced

-- | Computes the weakly connected components of the symmetric closure of the positive edges.
-- If the graph is clustered, then the resulting sets constitute a clustering
-- (positive edges connect edges from the same set, 
--  negative edges connect edges from different sets).
-- However, these sets can be computed without the graph being clustered,
-- thus the result is referred to as a candidate of a clustering only.

findClusteringCandidate :: 
	(Mapping q, HasVMM vec q, Complementable vec q, Unionable vec q, Unionable vec vec) => 
		Graph q vec Sign -> [vec ()]
findClusteringCandidate = weaklyConnectedComponents . positive

-- | Computes the vector of the betweenness values for all vertices as a vector.

betweenness :: (UnionableHom vec, Mapping q, MappingV vec) =>
	Matrix q vec a -> vec Rational
betweenness = fromRow . betweennessRow

-- | Computes the vector of the betweenness values for all vertices.
-- For simplicity of the implementation the result is a 'Row'.

betweennessRow :: (UnionableHom vec, Mapping q, MappingV vec) =>
	Graph q vec a -> Row Rational
betweennessRow m = map (id &&& between) vs where

    between t = sum [sigma v w t % gamma v w | (v, w) <- pairs, v /= t, t /= w]
    
    pairs  = [(v, w) | v <- vs, w <- vs, gamma v w /= 0]
    vs     = verticesList m

    sigma v w t  | dvt `addTropical` dtw == d v w  = gvt * gtw
                 | otherwise                       = 0
                 where  (dvt, gvt) = both v t
                        (dtw, gtw) = both t w
    
    d     = fst <.> both
    gamma = snd <.> both
    
    both v w = (geodesicLength q, number (tropicalToNum (geodesicWeight q)))
      where q = kcm .@. (v, w)

    kcm = kleeneClosureC (fmap (const label) m)

-- | A special initial label for the computation of the betweenness centrality.

label :: Geodesic (Number Integer) (Number Integer)
label = Geodesic (Tropical (Number 1)) (Tropical 1)