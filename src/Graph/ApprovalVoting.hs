----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.ApprovalVoting
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a simple implementation of approval votings in terms of matrices.
-- We provide functions for the computation of winners (with certain player sets)
-- and a simple generate-and-test approach to finding a minimal set of players whose removal
-- turns an arbitrary alternative into a winning alternative.

module Graph.ApprovalVoting (

	ApprovalVoting,
	Player,
	Alternative,
	players,
	votesWithPlayers,
	results,
	winners,
	generateAndTestApproval

	) where

import Control.Applicative    ( (<*>) )

import Algebraic.Matrix       ( Matrix, HasVMM )
import Algebraic.Structures   ( Number, number )
import Algebraic.Vector       ( (<-->), (.@.), maxIndices, maxVector, restrictToMax )
import Auxiliary.General      ( Key, powerlistFrom, emptyOrContained )
import Auxiliary.Mapping      ( Mapping, size, toMapping )
import Auxiliary.SetOps       ( UnionableHom )
import Graph.Graph            ( verticesVec )
import Graph.Paths            ( (.*#) )

-- | An approval voting allows only one vote per alternative.
-- We thus represent it by using the '()' value in the matrix.

type ApprovalVoting q vec = Matrix q vec ()

-- | Both players and alternatives are just 'Key's.

type Player      = Key
type Alternative = Key

-- | Returns all players in an approval voting.

players :: (Mapping q, Mapping vec) => ApprovalVoting q vec -> vec ()
players = verticesVec

-- | Computes a vector that contains the outcome of the election with the given vector
-- of players.

votesWithPlayers :: HasVMM vec q => ApprovalVoting q vec -> vec a -> vec (Number Integer)
votesWithPlayers = flip (.*#)

-- | Computes a vector that contains the outcome of the election using all players.

results :: (Mapping q, HasVMM vec q) => ApprovalVoting q vec -> vec (Number Integer)
results = votesWithPlayers <*> players -- \v -> votesWithPlayers v (players v)

-- | Computes the winning alternatives in a given election with the given vector of players.

winners :: (Mapping q, HasVMM vec q) => ApprovalVoting q vec -> vec a -> vec (Number Integer)
winners v ps = restrictToMax (votesWithPlayers v ps)

-- | Computes a minimal set of players whose removal turns an arbitrary alternative into
-- a winning alternative.
-- If the alternative is already a winning one, this function returns the empty set.
-- N.B. that this function's worst case complexity is exponential (base 2) in the number of players.

generateAndTestApproval :: (Mapping q, HasVMM vec q, UnionableHom vec) => 
	ApprovalVoting q vec -> Alternative -> (Integer, [Key])
generateAndTestApproval v alt = 
	head (dropWhile (isLosing . snd)(zip [1 ..] (findSets v alt))) where
		res        = results v
		isLosing s = not (emptyOrContained alt (maxIndices (res <--> votesWithPlayers v (toMapping s))))

-- | Computes those sets of players that need to be checked (for removal) to ensure that
-- the given alternative wins.

findSets :: (HasVMM vec q, Mapping q) => ApprovalVoting q vec -> Alternative -> [[Key]]
findSets v alt = powerlistFrom (size (players v)) (fromInteger (number (atLeast v alt)))

-- | Computes the smallest number of players that needs to be removed from the election
-- for an alternative to win.
-- The computed number is only a lower bound.

atLeast :: (HasVMM vec q, Mapping q) => ApprovalVoting q vec -> Alternative -> Number Integer
atLeast v alt = maxVector res - (res .@. alt) where
	res = results v