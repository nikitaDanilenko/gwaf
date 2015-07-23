----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.General
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- This module contains some general purpose functions and combinators as well as
-- merge-based lists operations.
-- Additionally,
-- this module provides the type synonyms that are used in graph-theoretic applications
-- in this library.

module Auxiliary.General (

    -- * Combinators, operators and higher-order functions
    
    (<.>) ,
    scaleLeft,
    wrap,

    -- * Comparison

    minBy,

    -- * Graph-related types
    
    Key,
    Arc,
    Row

    ) where

-- | 
-- This function is known as '(.:)' from the Haskell wiki and the functional
-- graph library <http://hackage.haskell.org/package/fgl fgl> by Martin Erwig.
-- It is used to compose a function
-- @f :: a -> b -> c@ with a function @g :: c -> d@ such that
-- 
-- > (g <.> f) x y = g (f x y).
-- 
-- The infix binding of '(<.>)' is exactly one unit less than the one of '(.)',
-- so that
--   
-- > f . g <.> h = (f . g) <.> h
--  
-- and
--   
-- > f <.> g . h = f <.> (g . h)
--   
-- which is convenient in many cases.
-- The pointfree function definition itself 
-- is said to have been found by the @pl@-plugin of 
-- <http://hackage.haskell.org/package/lambdabot lambdabot>.

infixr 8 <.>
(<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<.>) = (.) . (.)

-- | A non-overloaded version of 'min' that takes an ordering function as an argument
-- and returns the smaller one. 

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y = case cmp x y of
                  LT -> x
                  _  -> y

-- | Lifts a binary function to a function that operates on a structure in its second argument.
-- The idea is reminiscent of a scalar multiplication for vectors,
-- which is obtained from lifting the field multiplication to the vector level.

scaleLeft :: Functor f => (a -> b -> c) -> a -> f b -> f c
scaleLeft = (fmap .)

-- | Maps a pair whose second component is a 'Functor' to a 'Functor' containing the pair.
-- For example @wrap ('a', Just 5) = Just ('a', 5)@.

wrap :: Functor f => (a, f b) -> f (a, b)
wrap = uncurry (fmap . (,))

-- | A type for keys in key-value pairs and association lists.

type Key   = Int

-- | An arc is a key-value pair.
-- The name originates from the graph-theoretic interpretation that an arc points to its
-- key (vertex) and is labelled with its value.

type Arc a = (Key, a)

-- | A row is simply an association list.

type Row a = [Arc a]