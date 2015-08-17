----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.MonadicSet
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides an extension of the monadic set definition that is given in
-- the standard library module 
-- <http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Graph.html Data.Graph>'
-- (located in the package containers-0.5.6.3).
-- Said implementation is based upon the one given in
-- 
-- * David King, John Launchbury, Structuring Depth-First Search Algorithms in Haskell, POPL 1995,
--   <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.6526&rep=rep1&type=pdf available here>.
--  
-- The implementation is extended to arbitrary values in the set and some additional functions
-- for simplified applications.
-- The separation into an 'ST' monad implementation for the GHC and a portable implementation
-- using 'IntMap's is exactly the one presented in 
-- <http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Graph.html Data.Graph>
-- and we use the same names for transparency.

{-# Language CPP #-}

#if __GLASGOW_HASKELL__
# define USE_ST_MONAD 1
#endif

#if USE_ST_MONAD
{-# Language Rank2Types, Trustworthy #-}
#endif

module Auxiliary.MonadicSet (
  Set,
  SetM,
  insert,
  insertAll,
  include,
  includeAll,
  contains,
  get,
  ifInSet,
  runWith,
  runWithNew,
  runWithNewSize
  ) where

import Control.Applicative         ( Applicative (..), liftA )
import Control.Arrow               ( (&&&) )
import Control.Monad               ( ap )

#if USE_ST_MONAD

import Control.Monad.ST.Lazy.Safe  ( ST, runST )
import Data.Array.ST               ( STArray, writeArray, readArray, newArray )

#else

import Data.IntMap                 ( IntMap )
import qualified Data.IntMap as IM ( insert, empty )

#endif

import Data.Maybe                  ( isJust )

import Auxiliary.General           ( Arc, Key )

-- | The set data type.
-- The parameter @a@ denotes the type of the values in the set,
-- while the parameter @b@ is the type of the effect of a computation.

#if USE_ST_MONAD

newtype Set a b = Set { runSet :: forall s . STArray s Key (Maybe a) -> ST s b }

instance Monad (Set a) where

    return x = Set (const (return x)) -- No eta-reduction possible due to the quantification.

    Set m >>= f = Set fun where
        fun arr = do x <- m arr
                     runSet (f x) arr
#else 

newtype Set a b = Set { runSet :: IntMap a -> (b, IntMap a) }

instance Monad (Set a) where

  return x    = Set (\im -> (x, im))
  Set m >>= f = Set (\im -> let (b, im') = m im
                            in runSet (f b) im')

#endif

instance Functor (Set a) where

    fmap = liftA

instance Applicative (Set a) where

    pure  = return
    (<*>) = ap

-- | A short-hand for monadic sets that contain only unit values.

type SetM a = Set () a

-- | Inserts a given value at the given key in the set.

#if USE_ST_MONAD

insert :: Key -> a -> Set a ()
insert i x = Set (\arr -> writeArray arr i (Just x))

#else

insert :: Key -> a -> Set a ()
insert i x = Set (\im -> ((), IM.insert i x im))

#endif

-- | Reads the value at a given key from the set.

#if USE_ST_MONAD

get :: Key -> Set a (Maybe a)
get x = Set (`readArray` x)

#else

get :: Key -> Set a (Maybe a)
get i = Set (IM.lookup i &&& id)

#endif

-- | Inserts all key-value pairs from a list into a set.

insertAll :: [Arc a] -> Set a ()
insertAll = mapM_ (uncurry insert)

-- | Inserts a key into a 'SetM'.

include :: Key -> SetM ()
include i = insert i ()

-- | Inserts all keys from a list into a 'SetM'.

includeAll :: [Key] -> SetM ()
includeAll = mapM_ include

-- | Checks whether the given key is present in the set.

contains :: Key -> Set a Bool
contains = fmap isJust . get

-- | An abbreviation of a monadic if-then-else.
-- This function allows simpler code in cases where an if-then-else branch is used
-- with a Boolean value that is obtained from a 'Set'.

ifInSet :: Key -> Set a b -> Set a b -> Set a b
ifInSet i t e = do b <- contains i
                   if b then t else e

-- | Compute the effect of a set starting with an initial set.

#if USE_ST_MONAD

runWith :: (forall s . ST s (STArray s Key (Maybe a))) -> Set a b -> b
runWith arr set = runST (arr >>= runSet set)

#else

runWith :: IntMap a -> Set a b -> b
runWith im set = fst (runSet set im)

#endif

-- | Compute the effect of a set starting with the empty set.

#if USE_ST_MONAD

runWithNew :: (Key, Key) -> Set a b -> b
runWithNew bnds = runWith (newArray bnds Nothing)

#else

runWithNew :: (Key, Key) -> Set a b -> b
runWithNew _ = runWith IM.empty

#endif

-- | Computes the effect of a set starting with the empty set indexed from /0/ to /n - 1/,
-- where /n/ is the first argument.

runWithNewSize :: Int -> Set a b -> b
runWithNewSize n = runWithNew (0, n - 1)