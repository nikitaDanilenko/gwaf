----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Algebraic.PathAlgebra
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides semiring instances that can be used to compute different
-- measures of graphs that are relevant in the analysis of social networks.
-- All of the presented semirings are described in
--
-- * Vladimir Batagelj, Semirings for Social Network Analysis,
--   Journal of Mathematical Sociology,
--   <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.332.1968&rep=rep1&type=pdf available here>

module Algebraic.PathAlgebra (

	Sign ( .. ),
	Balanced ( .. ),
	allBalanced,
	Clustered ( .. ),
	allClustered,
	Geodesic ( .. )

	) where

import Test.QuickCheck      ( Arbitrary, arbitrary, frequency )

import Algebraic.Structures ( SemigroupA, MonoidA, FindZero, 
	                            SemigroupM, MonoidM, FindOne,
	                            Semiring, IdempotentSemiring, KleeneAlgebra, KleeneAlgebraC,
	                            (.+.), zero, mtimes, isZero, (.*.), one, (.^.), isOne, star,
	                            Tropical ( .. ), addTropical, multTropical )
import Auxiliary.General    ( allValues )

-- | Data type for the sign of a number.

data Sign = Pos | Neg
	deriving Eq

instance Show Sign where
	
	show Pos = "+"
	show Neg = "-"

-- | A data structure that can be used to check whether a graph is balanced.

data Balanced = NoWalkB | AllNegative | AllPositive | Mixed
	deriving (Eq, Enum, Bounded)

allBalanced :: [Balanced]
allBalanced = allValues

instance Show Balanced where
	
	show NoWalkB     = "__"
	show AllPositive = "++"
	show AllNegative = "--"
	show _           = "+-"

instance Arbitrary Balanced where

	arbitrary = frequency (map (\b -> (1, return b)) allBalanced)

-- | The addition is the choice between different types of walks.

instance SemigroupA Balanced where

	NoWalkB     .+. x       = x
	x           .+. NoWalkB = x
	
	AllNegative .+. AllNegative = AllNegative

	AllPositive .+. AllPositive = AllPositive
	
	_           .+. _           = Mixed

-- | 'NoWalkB' is neutral with respect to addition.

instance MonoidA Balanced where

	zero       = NoWalkB

	mtimes i b | i == 0    = zero
	           | otherwise = b

instance FindZero Balanced where

	isZero NoWalkB = True
	isZero _       = False

-- | The product of walks is their concatenation.

instance SemigroupM Balanced where

	NoWalkB .*. _       = NoWalkB
	_       .*. NoWalkB = NoWalkB

	Mixed   .*. _       = Mixed
	_       .*. Mixed   = Mixed

	x       .*. y       | x == y    = AllPositive
	                    | otherwise = AllNegative

-- | 'AllPositive' is neutral with respect to multiplication.

instance MonoidM Balanced where

	one           = AllPositive

	_           .^. 0 = AllPositive
	AllPositive .^. _ = AllPositive
	NoWalkB     .^. _ = NoWalkB
	AllNegative .^. i | even i = AllPositive
	                  | otherwise = AllNegative
	Mixed       .^. _ = Mixed

instance FindOne Balanced where

	isOne AllPositive = True
	isOne _           = False

instance Semiring Balanced
instance IdempotentSemiring Balanced

instance KleeneAlgebra Balanced where

	star NoWalkB     = AllPositive
	star AllPositive = AllPositive
	star _           = Mixed

instance KleeneAlgebraC Balanced

-- | A data type that can be used to check whether a graph is clustered.
-- The semantics are the following (as described by Batagelj):
--
-- * 'NoWalkC': There is no walk.
-- * 'N': There is no walk with only positive arcs 
--        and there is a walk with exactly one negative arc.
-- * 'P': There is a walk with only positive arcs 
--        and there is no walk with exactly one negative arc.
-- * 'A': There is a walk with only positive arcs 
--        and there a walk with exactly one negative arc.
-- * 'Q': There is a walk, but there is no walk with only positive arcs
--        and there is no walk with exactly one negative arc.
--        (Thus every walk has at least two negative arcs.)

data Clustered = NoWalkC | N | P | A | Q
	deriving (Eq, Enum, Bounded)

instance Show Clustered where

	show NoWalkC = "_"
	show N       = "n"
	show P       = "p"
	show A       = "a"
	show Q       = "q"

instance Arbitrary Clustered where

	arbitrary = frequency (map (\b -> (1, return b)) allClustered)

-- | All values of the 'Clustered' data type.

allClustered :: [Clustered]
allClustered = [minBound .. maxBound]

-- | The addition is the choice between different types of walks.

instance SemigroupA Clustered where

	NoWalkC .+. x       = x
	
	N       .+. P       = A
 	N       .+. A       = A
 	N       .+. _       = N

 	P       .+. N       = A
 	P       .+. A       = A
 	P       .+. _       = P

 	A       .+. _       = A
    
 	Q       .+. NoWalkC = Q
 	Q       .+. x       = x

-- | 'NoWalkC' is neutral with respect to addition.

instance MonoidA Clustered where

	zero = NoWalkC

	mtimes i c | i == 0    = zero
	           | otherwise = c

instance FindZero Clustered where

	isZero NoWalkC = True
	isZero _       = False

instance SemigroupM Clustered where

	NoWalkC .*. _       = NoWalkC
    
	N       .*. NoWalkC = NoWalkC
	N       .*. P       = N
	N       .*. A       = N
	N       .*. _       = Q

	P       .*. x       = x

	A       .*. NoWalkC = NoWalkC
	A       .*. N       = N
	A       .*. Q       = Q
	A       .*. _       = A

	Q       .*. NoWalkC = NoWalkC
	Q       .*. _       = Q

-- | The multiplication is the concatenation of paths.

instance MonoidM Clustered where

	one = P

	_       .^. 0 = P
	NoWalkC .^. _ = NoWalkC
	N       .^. i | i == 1    = N
	              | otherwise = Q
	x       .^. _ = x

instance FindOne Clustered where

	isOne P = True
	isOne _ = False

instance Semiring Clustered
instance IdempotentSemiring Clustered

instance KleeneAlgebra Clustered where

	star N = A
	star A = A
	star _ = P

instance KleeneAlgebraC Clustered

-- | The geodesic semiring collects information about the shortest paths between two
-- vertices.
-- The geodesic length denotes the length of a shortest path,
-- while the geodesic weight is the number of shortest paths between two vertices.

data Geodesic l w = Geodesic { geodesicLength :: Tropical l, geodesicWeight :: Tropical w }
	deriving (Show, Eq)

instance (Ord l, SemigroupA w) => SemigroupA (Geodesic l w) where

	Geodesic m i .+. Geodesic n j = Geodesic (m .+. n) k where
		k | m < n     = i
		  | m == n    = i `addTropical` j
		  | otherwise = j

instance (Ord l, SemigroupA w) => MonoidA (Geodesic l w) where

	zero = Geodesic Max Min

instance FindZero (Geodesic l w) where

	isZero (Geodesic Max Min) = True
	isZero _                  = False

instance (SemigroupA l, SemigroupM w) => SemigroupM (Geodesic l w) where

	-- The multiplication in the first component is the multiplication of Tropical values,
	-- which is the numerical addition of these values with Min = 0 and Max = Infinity.

	Geodesic m i .*. Geodesic n j = Geodesic (m .*. n) (i `multTropical` j)

instance (SemigroupA l, MonoidM w) => MonoidM (Geodesic l w) where

	one = Geodesic Min (Tropical one)

instance FindOne w => FindOne (Geodesic l w) where

	isOne (Geodesic Min (Tropical t)) = isOne t
	isOne _                           = False

instance (Ord l, SemigroupA l, SemigroupA w, MonoidM w) => Semiring (Geodesic l w)

instance (Ord l, SemigroupA l, SemigroupA w, MonoidM w) => IdempotentSemiring (Geodesic l w)

instance (Ord l, SemigroupA l, SemigroupA w, MonoidM w) => KleeneAlgebra (Geodesic l w) where

		star (Geodesic l w) = Geodesic Min (f l w) where
			f Min Min = Tropical one
			f Min _   = Max
			f _   _   = Tropical one

instance (Ord l, SemigroupA l, SemigroupA w, MonoidM w, FindOne w) => KleeneAlgebraC (Geodesic l w)