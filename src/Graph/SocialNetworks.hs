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

	Sign,

	-- * Predicates

	isClustered,
	isClustered2,
	
	-- * Auxiliary

	positive,
	negative,

	) where

import Data.Foldable          ( Foldable )

import Algebraic.Closure      ( starSymmetricClosureC )
import Algebraic.Matrix       ( Matrix, toUnitMatrix, filterMatrix, isEmptyMatrix, identityMatrix,
	                              symmetricClosureC, HasVMM )
import Algebraic.PathAlgebra  ( Clustered ( P, N ) )
import Algebraic.Structures   ( KleeneAlgebraC )

import Auxiliary.KeyedClasses ( KeyMaybeFunctor )
import Auxiliary.Mapping      ( Mapping )
import Auxiliary.SetOps       ( (//\), UnionableHom, Unionable, Intersectable )

-- | Data type for signs (positive and negative).

data Sign = Pos | Neg
	deriving Eq

instance Show Sign where

	show Pos = "+"
	show Neg = "-"

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

isClustered2 :: 	(UnionableHom q, Mapping q, Intersectable q q, 
	 Unionable vec q, HasVMM vec q, 
	 UnionableHom vec, Intersectable vec vec) => 
			Matrix q vec Sign -> Bool
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