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
  chopShallow,
  chopDeep,
  pruneDisjoint,
  pruneDisjointUpToStart,
  pruneDisjointUpToEnd,
  pruneDisjointUpToStartEnd

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
import Auxiliary.MonadicSet      ( Set, SetM, include, contains, runWithNew )
import Graph.Path                ( Path, stepRight, emptyPath )


-- | This function computes a single list of arcs in a reachability forest.
-- The basic idea is to apply this function to each reachability forest from
-- a given vertex in a vertex set and to obtain a set of disjoint paths.
-- The term \"disjoint\" can be relaxed through the first parameter,
-- by allowing the paths to intersect at their first vertex.
-- The term \"path\" is also relaxed to the arbitrary type @p@.

chop :: Bool               -- ^ Is the first vertex along the path unique?
     -> (a -> Key)         -- ^ Returns the 'Key' value of @a@ (e.g. 'fst' if @a = 'Arc' b@).
     -> (a -> p -> p)      -- ^ Combinator function for abstract path extensions.
     -> p                  -- ^ Initial abstract path.
     -> Forest a           -- ^ The forest that is pruned.
     -> MaybeT (Set ()) p
chop firstUnique keyOf extend start = chopSubforest where

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

    check _    | null ts   = act (return (update start))
               | otherwise = do markI
                                fmap update (chopSubforest ts) `mplus` fs'

    i        = keyOf v
    fs'      = chopSubforest fs
    update   = extend v 
    markI    = lift (include i)
    act      | firstUnique = (markI >>)
             | otherwise   = id

-- | This function takes a 'Traversable' filled with @(a, 'Forest' a)@ values
-- and computes possible paths through every single forest starting with the empty path.
-- The forests should be shortest-path forests.
-- The Boolean argument answers the question, whether the first vertex of the path is allowed
-- to occur multiple times as the first vertex or not.
-- The suffix \"shallow\" is a mnemonic for the fact that the pruning begins at the
-- top-most level, rather than one level below.
-- 
-- Note that the choice of 'Traversable' structures is somewhat
-- orthogonal to the application with 'Mapping's,
-- because the keys are implicit in case of mappings.
-- However, this type is more general and thus applicable beyond the 'Mapping' context.

chopShallow :: Traversable t =>
               Bool                 -- ^ Is the first vertex unique?
            -> (a -> Key)           -- ^ The key value of @a@.
            -> (a -> p -> p)        -- ^ Abstract path extension.
            -> (a -> Forest a -> p) -- ^ Computes the initial path.
            -> t (a, Forest a)      -- ^ The structure containing all pairs in question.
            -> SetM [p]
chopShallow firstUnique keyOf ext start = fmap (catMaybes . toList) . mapM (runMaybeT . uncurry f)
  where f i forest = chop firstUnique keyOf ext (start i forest) [Node i forest]

-- | This function proceeds in a similar fashion as 'chopShallow', but instead of starting at
-- the top-most level, it allows multiple occurrences of the last path element.
-- Since the function 'chop' finds at most one path per forest,
-- for every pair @(i, forest)@ in the traversable structure,
-- the @forest@ value is decomposed into singleton forests of its trees.
-- Then each singleton forest is searched for a path and the value @i@ is used to extend
-- the path by its last step.
-- Since the outer layer is not taken into account, 
-- when checking for previously visited vertices,
-- the values at the top level may occur in the forest as well.
-- This is not the case, if the forests are shortest reachability forests,
-- because in this situation there are no shorter paths to the top-most values.

chopDeep :: Traversable t =>
            Bool 
         -> (a -> Key)
         -> (a -> p -> p)
         -> (a -> Forest a -> p)
         -> t (a, Forest a)
         -> SetM [p]
chopDeep firstUnique keyOf extend start = 
  fmap catMaybes . mapM (runMaybeT . uncurry g) . concatMap (uncurry (map . (,))) . toList
    where g i tree = fmap (extend i) (chop firstUnique keyOf extend (start i [tree]) [tree])

-- | Computes a list of shortest pairwise disjoint paths.
-- The set represented by this list is maximal with respect to inclusion (in the forest).

pruneDisjoint :: Traversable t => (Key, Key) -> t (Key, Forest Key) -> [Path Key]
pruneDisjoint = pruneWithNew (chopShallow True id (flip stepRight) (\_ _ -> emptyPath))

-- | Computes a list of shortest paths that are disjoint up to their start vertex.
-- The set of paths represented by this list is maximal with respect to inclusion (in the forest).

pruneDisjointUpToStart :: Traversable t => (Key, Key) -> t (Key, Forest Key) -> [Path Key]
pruneDisjointUpToStart = pruneWithNew (chopShallow False id (flip stepRight) (\_ _ -> emptyPath))

-- | Computes a list of shortest paths that are disjoint up to their end vertex.
-- The set of paths represented by this list is maximal with respect to inclusion (in the forest).

pruneDisjointUpToEnd :: Traversable t => (Key, Key) -> t (Key, Forest Key) -> [Path Key]
pruneDisjointUpToEnd = pruneWithNew (chopDeep True id (flip stepRight) (\_ _ -> emptyPath))

-- | Computes a list of shortest paths that are disjoint up to their start and end vertices.
-- The set of paths represented by this list is maximal with respect to inclusion (in the forest).

pruneDisjointUpToStartEnd :: Traversable t => (Key, Key) -> t (Key, Forest Key) -> [Path Key]
pruneDisjointUpToStartEnd = pruneWithNew (chopDeep False id (flip stepRight) (\_ _ -> emptyPath))

-- | Short-hand definition to avoid mentioning the bounds arguments.

pruneWithNew :: (t -> Set a b) -> (Key, Key) -> t -> b
pruneWithNew c bnds = runWithNew bnds . c