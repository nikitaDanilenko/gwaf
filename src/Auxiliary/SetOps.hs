----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.SetOps
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides type classes for set operations which are parametric in the used containers.
-- The motivation for this abstraction is that while homogeneous unions and intersections
-- are very convenient, they usually have complexitities that are linear in the sum of the
-- sizes of their arguments.
-- It may be more efficient to 
-- 
-- * traverse one structure and to query another to obtain an intersection 
-- * traverse one structure and insert its values into another for a union.
-- 
-- This is possible with the notion of heterogeneous set operations.
-- Most of the set operations provided by this module are direct generalisations of functions
-- that are usually provided for key-value maps like 'IntMap's.
-- 
-- The instances of the heterogeneous set operations are organised as follows:
-- 
-- * the homogeneous instances for data types defined in this package are defined
--   together with the data type
-- * all other instances (heterogeneous ones and homogeneous instances of data types defined
--   elsewhere) are provided in "Auxiliary.SetOpsInstances".

{-# LANGUAGE MultiParamTypeClasses #-}

module Auxiliary.SetOps (
    
    -- * Heterogeneous intersections
    
    Intersectable ( .. ),
    capWithKey,
    capWith,
    capLeft,

    -- * Heterogeneous unions

    Unionable ( .. ),
    bigunionWith,
    bigunionLeft,

    -- * Heterogeneous differences

    Complementable ( .. ),
    difWith,
    dif,
    
    -- * Heterogeneous set operations

    SetOps ( .. )

    ) where

import Data.Foldable                    ( Foldable )
import qualified Data.Foldable as F     ( foldr )

import Auxiliary.General                ( Key, scaleLeft )
import Auxiliary.KeyedClasses           ( Lookup, clookup, KeyMaybeFunctor, fmapMaybeWithKey )

-- | The 'Intersectable' type class abstracts the heterogeneous intersection of indexed containers.
-- The idea is that the main function 'intersectWithKey' takes a function @op@ that is
-- used in case a key-value pair @(i, x)@ is present in the first structure and
-- a key-value pair @(i, y)@ is present in the second one.
-- In this case,
-- the key-value pair @(i, op i x y)@ is added to the result structure.
-- Otherwise both values are ignored and nothing is added to the result.
-- The following laws should be satisfied for instances of 'Intersectable'.
-- 
-- [/intersection without keys/]
--      @'intersectionWith' = 'intersectionWithKey' '.' 'const'@
-- 
-- [/left-biased intersection/]
--      @('//\') = 'intersectionWith' 'const'@
-- 
-- [/right-biased intersection/]
--      @('/\\') = 'intersectionWith' ('flip' 'const')@
-- 
-- The nomenclature is a mnemonic for the fact that the first argument is __t__raversed
-- and the second argument is __q__ueried.

class Intersectable t q where
    {-# Minimal intersectionWithKey #-}

    -- | A function that takes an operation that is applied in case of to key-value pairs
    -- from the two structures that have the same key.
    -- The result of this application is added to the result of 'intersectionWithKey'.

    intersectionWithKey :: (Key -> a -> b -> c) -> t a -> q b -> t c

    -- | A variant of 'intersectionWithKey' that ignores the key values.

    intersectionWith    :: (a -> b -> c)      -> t a -> q b -> t c
    intersectionWith = intersectionWithKey . const

    -- | Left-biased intersection that ignores the values in the second structure.
    
    infixr 6 //\
    (//\) :: t a -> q b -> t a
    (//\) = intersectionWith const

    -- | Right-biased intersection that ignores the values in the first structure.

    infixr 6 /\\
    (/\\) :: t a -> q b -> t b
    (/\\) = intersectionWith (flip const)

-- | The strategy of traversing one structure while querying another can be expressed in terms
-- of the type classes 'Lookup' and 'KeyMaybeFunctor'.
-- The function 'capWithKey' implements this strategy.
-- Since its type is essentially the one of 'intersectionWithKey' it can be used in instance
-- declarations.
-- The particular strategy may not be an optimal one for all containers,
-- which is why this function is not located in a type class.
-- As a mnemonic we have \"a cap is an intersection, but not every intersection is a cap\".

capWithKey :: (Lookup l, KeyMaybeFunctor f) => (Key -> a -> b -> c) -> f a -> l b -> f c
capWithKey op r c = fmapMaybeWithKey (\i x -> scaleLeft (op i) x (clookup i c)) r

-- | A variant of 'capWithKey' that ignores the key values.

capWith :: (Lookup l, KeyMaybeFunctor f) => (a -> b -> c) -> f a -> l b -> f c
capWith = capWithKey . const

-- | A left-biased variant of 'capWith', which ignores the values in the second structure.

capLeft :: (Lookup l, KeyMaybeFunctor f) => f a -> l b -> f a
capLeft = capWith const

-- | A type class for heterogeneous unions.
-- The abstraction is similar to the one for 'Intersectable' -
-- the left structure is traversed and its key-value pairs are inserted into the second one.
-- In case of key equality, a supplied operation is applied to both values.
-- Instances of this type class must satisfy the following laws:
-- 
-- [/left-biased union/]
--      @('\\/') = 'unionWith' 'const'@
-- 
-- [/right-biased union/]
--      @('\//') = 'unionWith' ('flip' 'const')@.
-- 
-- Pairs of types that are both instances of 'Intersectable' and 'Unionable'
-- should satisfy the following rule:
-- 
-- [/absorption/]
--   @(t '/\\' q) '\\/' q = q@.
-- 
-- This is similar to the absorption law for lattices,
-- but one needs to use the right-biased intersection,
-- because the values in @t@ might differ from those in @q@.
-- 
-- The nomenclature is a mnemonic for the fact that the first argument is __t__raversed
-- and the second argument is __i__nserted into.


class Unionable t i where
    {-# Minimal unionWith #-}

    -- | The heterogeneous union that takes an operation @op@
    -- and applies it in case of key equality.
    -- This is to say that the if left structure
    -- contains the key-value pair @(i, x)@ 
    -- and the right structure contains the key-value pair @(i, y)@
    -- the result structure contains the key-value pair @(i, x `op` y)@.
    -- In case of the key of a key-value pair is contained only in one of the structures,
    -- it is added to the result structure as well.

    unionWith :: (a -> a -> a) -> t a -> i a -> i a

    -- | Left-biased union that ignores the values in the second structure.

    infixr 5 \\/
    (\\/) :: t a -> i a -> i a
    (\\/) = unionWith const

    -- | Right-biased union that ignores the values in the first structure.

    infixr 5 \//
    (\//) :: t a -> i a -> i a
    (\//) = unionWith (flip const)

-- | Fold 'unionWith' over a structure.
-- Note that this there is no default empty value for the structure in which the values
-- are inserted,
-- the accumulation parameter of the fold function needs to be supplied.

bigunionWith :: (Unionable t i, Foldable f) => 
    (a -> a -> a)   -- ^ combination operation
 -> i a             -- ^ accumulation parameter
 -> f (t a)         -- ^ structure to fold over
 -> i a
bigunionWith = F.foldr . unionWith

-- | A folded variant of the left-biased union @('\\/')@.

bigunionLeft :: (Unionable t i, Foldable f) => i a -> f (t a) -> i a
bigunionLeft = F.foldr (\\/)

-- | A type class for heterogeneous relative complements.
-- The basic idea is to remove elements of the second structure from the first one.
-- To this extent there are two possible abstractions:
-- 
-- * traverse the left stucture, query the right one and remove occurrences of the same key
-- * traverse the right structure and update the values in the first one,
--   where updating a value to 'Nothing' means deleting said value.
-- 
-- Instances of this type class should satisfy the following laws:
-- 
-- [/querying difference/]
--      @('\\') = 'differenceWith' (\_ _ -> 'Nothing')@
-- 
-- [/updating difference/]
--      @('\\\') = 'differenceWith2' (\_ _ -> 'Nothing')@.
-- 
-- The nomenclature hints at these abstractions -- the first structure is __t__raversed
-- and the second one supports __q__ueries and __u__pdates.

class Complementable t qu where
    {-# Minimal differenceWith, differenceWith2 #-}

    differenceWith  :: (a -> b -> Maybe a) -- ^ operation applied in case of key equality,
                                           --   where 'Nothing' means deletion
                    -> t a                 -- ^ traversed structure
                    -> qu b                -- ^ queried structure
                    -> t a

    differenceWith2 :: (a -> b -> Maybe a) -- ^ operation applied in case of key equality,
                                           --   where 'Nothing' means deletion
                    -> qu a                -- ^ updated structure   
                    -> t b                 -- ^ traversed structure
                    -> qu a

    -- | A variant of 'differenceWith' that removes all values in the second structure
    -- from the first one.

    infixr 5 \\
    (\\) :: t a -> qu b -> t a
    (\\) = differenceWith (\_ _ -> Nothing)

    -- | A variant of 'differenceWith2' that removes all values in the second structure
    -- from the first one.

    infixr 5 \\\
    (\\\) :: qu a -> t b -> qu a
    (\\\) = differenceWith2 (\_ _ -> Nothing)

-- | The abstraction of traversing one structure while querying another is possible in terms
-- of 'Lookup' and 'KeyMaybeFunctor'.
-- This function is an implementation of the strategy intended for 'differenceWith'
-- and can be used in instance declarations.
-- Just as with 'intersectionWithKey' the implemented strategy is not necessarily optimal for all
-- data types.
-- Again, the mnemonic here is \"a dif is a complement, but not every complement is a dif\".

difWith :: (Lookup l, KeyMaybeFunctor f) => 
        (a -> b -> Maybe a) -- ^ operation applied in case of key equality,
                            --   where 'Nothing' means deletion
     -> f a                 -- ^ the traversed structure
     -> l b                 -- ^ the queried structure
     -> f a
difWith op r m = fmapMaybeWithKey (\i x -> maybe (Just x) (op x) (clookup i m)) r

-- | A variant of 'difWith' that removes all values in the second structure from the first one.

dif :: (Lookup v, KeyMaybeFunctor r) => r a -> v b -> r a
dif = difWith (\_ _ -> Nothing)

-- | A type class that combines the three set-theoretic abstractions
-- 'Intersectable', 'Unionable' and 'Complementable' into one.
-- With these three operations it is possible to define the symmetric difference that
-- should satisfy the following law:
-- 
-- [/symmetric difference/]
--      @'symDifference' t q = (t '\\/' q) '\\\' ('t //\ q')@
-- 
-- As for additional laws like distributivity of the intersection over the union and vice versa,
-- they should be satisfied in homogeneous cases, but are often not possible to express in
-- the more general case of heterogeneous set operations.

class (Intersectable r v, Unionable r v, Complementable r v) => SetOps r v where

    symDifference :: r a -> v a -> v a
    symDifference r c = (r \\/ c) \\\ (r //\ c)