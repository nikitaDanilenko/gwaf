----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Graph.Prune
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides several pruning operations that are based upon the depth-first
-- strategy presented in
-- 
-- * David King, John Launchbury, Structuring Depth-First Search Algorithms in Haskell, POPL 1995,
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.6526&rep=rep1&type=pdf available here>.
-- 
-- The intention of these operations is to prune a given reachability forest in a fashion
-- that yields certain sets of paths.
-- The most important application is the computation of a maximal set of pairwise disjoint
-- shortest paths between two vertex sets.
-- However, this strategy is easily adopted to exclude the first or the last vertex.

module Graph.Prune ( 

  chop,
  chopShallow

  ) where

import Control.Monad             ( mzero, mplus )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Maybe ( MaybeT, runMaybeT )
import Data.Foldable             ( toList )
import Data.Maybe                ( catMaybes )
import Data.Traversable          ( Traversable, mapM )
import Data.Tree                 ( Forest, Tree ( Node ) )

import Prelude hiding            ( mapM )

import Auxiliary.General         ( Key, Arc, Row )
import Auxiliary.MonadicSet      ( Set, SetM, include, contains )
import Graph.Path                ( LabelledPath, labelledStepRight, initial )

-- | This function computes a single list of arcs in a reachability forest.
-- The basic idea is to apply this function to each reachability forest from
-- a given vertex in a vertex set and to obtain a set of disjoint paths.
-- The term \"disjoint\" can be relaxed through the first parameter,
-- by allowing the paths to intersect at their first vertex.

chop :: Bool               -- ^ Is the first vertex along the path unique?
     -> (a -> Key)         -- ^ Returns the 'Key' value of @a@ (e.g. 'fst' if @a = 'Arc' b@).
     -> (a -> b)           -- ^ Returns the value of @a@ (e.g. 'snd' if @a = 'Arc' b@).
     -> LabelledPath Key b -- ^ The path in which the newly found steps are inserted.
     -> Forest a           -- ^ The forest that is pruned.
     -> MaybeT (Set ()) (LabelledPath Key b)
chop firstUnique vertexOf valueOf path = chopSubforest where

  -- If the forest is empty, then the the result is Nothing.

  chopSubforest []               = mzero

  -- Otherwise we check whether the key of v is contained in the set.

  chopSubforest (Node v ts : fs) = lift (contains i) >>= check where
    
    -- If it is, then we return the chopped remaining forest.

    check True = fs'

    -- Otherwise we need to check whether we have reached the bottom of the forest,
    -- which is the case if and only if ts is empty.
    -- If it is empty, we check whether the first path vertex is supposed to be unique
    -- or not and add mark that vertex as visited or not, respectively.
    -- Otherwise we have not yet reached the bottom,
    -- thus we mark the vertex as visited and choose precisely one path,
    -- namely either the path that is obtained from chopping the subforest ts and adding
    -- the current vertex or (if the result is Nothing) chopping the subforest fs.

    check _    | null ts   = act (return (add path))
               | otherwise = do markI
                                fmap add (chopSubforest ts) `mplus` fs'

    i        = vertexOf v
    fs'      = chopSubforest fs
    add path = labelledStepRight path (valueOf v) i
    markI    = lift (include i)
    act      | firstUnique = (markI >>)
             | otherwise   = id

-- | This function takes a 'Traversable' filled with @('Arc' 'Forest' a)@ values
-- and computes possible paths through every single forest starting with the empty path.
-- The suffix \"shallow\" is a mnemonic for the fact that the pruning begins at the
-- top-most level, rather than one level below.
-- 
-- Note that the choice of 'Traversable' structures filled with 'Arc's is somewhat
-- orthogonal to the application with 'Mapping's,
-- because the keys are implicit in case of mappings.
-- However, this type is more general and thus applicable beyond the 'Mapping' context.

chopShallow :: Traversable t =>
               Bool
            -> (a -> Key)
            -> (a -> b)
            -> t (Arc (Forest a))
            -> SetM [LabelledPath Key b]
chopShallow firstUnique vertexOf valueOf = fmap (catMaybes . toList) . mapM (runMaybeT . f) where
    f (i, forest) = chop firstUnique vertexOf valueOf (initial i) forest