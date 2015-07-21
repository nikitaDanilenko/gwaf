----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Algebraic.Semiring
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module contains the semiring type class,
-- as well as the associated type classes for additive and multiplicative monoids
-- (with and without the possibility to distinguish the constants).
-- Additionally, this module provides some (standard) implementations of semirings.
-- There is some overlap with
-- 
--   *  Fischer, Huch, Wilke: "A play on Regular Expressions", ICFP 2010,
--              <sebfisch.github.io/haskell-regexp/regexp-play.pdf available here>
--
--   *  Fischer, Huch, Wilke: 
--              <http://hackage.haskell.org/package/weighted-regexp weighted-regexp>
-- 
-- and with the deprecated package <http://hackage.haskell.org/package/monoids monoids>.

{-# LANGUAGE DeriveFunctor #-}

module Algebraic.Semiring (
    
    -- * Additive Monoids with an additional type class for checking constants
    
    MonoidA ( .. ),
    FindZero ( .. ),
    
    -- * Multiplicative Monoids with an additional type class for checking constants
    
    MonoidM ( .. ),
    FindOne ( .. ),
    
    -- * Semirings with an additional type class for checking constants and idempotend semirings.

    Semiring,
    SemiringC,

    -- * Kleene algebras

    KleeneAlgebra ( .. )
    
    ) where

import Control.Arrow ( first )
import Data.List     ( genericReplicate )

-- | A type class for additive monoids.
--   The prerequisite of additivity is merely a notational choice 
--   and does not come with any additional requirements.
--   Instances of this type class are required to satisfy the rules
--   for all @a, b, c :: m@
--   
--   [/associativity/]
--      @a '.+.' (b '.+.' c) = (a '.+.' b) '.+.' c@
--   
--   [/neutrality/]
--      @a '.+.' 'zero' = a = 'zero' '.+.' a@

class MonoidA a where
    {-# MINIMAL (.+.), zero #-}

    -- | A binary associative operation.

    infixr 5 .+.
    (.+.) :: a -> a -> a

    -- | A neutral element with respect to '(.+.)'.
    --   This element is automatically unique.

    zero  :: a
    
    -- | The sum of a list of elements.

    msum :: [a] -> a
    msum = foldr (.+.) zero

    -- | /n/-fold addition of the same element.

    mtimes :: Integer -> a -> a
    mtimes n x = msum (genericReplicate n x)

-- | A type class for multiplicative monoids.
--   This structure is isomorphic to the 'MonoidA' structure,
--   but has a different fixity for the monoid operation.
--   Instances of this type class are required to satisfy the rules
--   for all @a, b, c :: m@
--   
--   [/associativity/]
--      @a '.*.' (b '.*.' c) = (a '.*.' b) '.*.' c@
--   
--   [/neutrality/]
--      @a '.*.' 'one' = a = 'one' '.*.' a@

class MonoidM m where
    {-# MINIMAL (.*.), one #-}

    -- | A binary associative operation.

    infixr 6 .*.
    (.*.) :: m -> m -> m

    -- | A neutral element with respect to '(.*.)'.
    --   This element is automatically unique.

    one   :: m

    -- | The sum of a list of elements.

    mproduct :: [m] -> m
    mproduct = foldr (.*.) one

    -- | /n/-fold multiplication of the same element.

    mpower :: m -> Integer -> m
    mpower x n = mproduct (genericReplicate n x)

-- | A type class that provides a single predicate that can be used to check,
--   whether an element is \"zero\" or not.
--   The standalone type class is not required to satisfy any rules.
--   However, if a type @a@ has a 'FindZero' and a 'MonoidA' instance,
--   the following equality should hold for all @x :: a@
--   
--   @isZero x == True@ @<===>@ @x@ is mathematically equal to 'zero'

class FindZero a where
    {-# MINIMAL isZero #-}
    -- | A predicate that checks, whether an element is an abstract zero or not.

    isZero :: a -> Bool

-- | A type class that provides a single predicate that can be used to check,
--   whether an element is \"one\" or not.
--   The standalone type class is not required to satisfy any rules.
--   However, if a type @m@ has a 'FindOne' and a 'MonoidM' instance,
--   the following equality should hold for all @x :: m@
--   
--   @isOne x == True@ @\<===\>@ @x@ is mathematically equal to 'one'

class FindOne m where
    {-# MINIMAL isOne #-}
    -- | A predicate that checks, whether an element is an abstract one or not.

    isOne :: m -> Bool

-- | The semiring type class.
--   Semirings do not provide any additional functions other than those already provided
--   by the underlying instances of 'MonoidA' and 'MonoidM'.
--   Thus every semiring instance is trivial and can be obtained by simply writing
--   
--   > instance Semiring Foo
--   
--   However, if @s@ is a type with a 'MonoidA' and a 'MonoidM' instance,
--   the following additional rules are required for a 'Semiring' instance
--   for all @a, b, c :: s@
--   
--   [/left distributivity/]
--      @(a '.+.' b) '.*.' c = a '.*.' c '.+. b '.*.' c@ (multiplication has a higher fixity)
--   
--   [/right distributivity/]
--      @a '.*.' (b '.+.' c) = a '.*.' b '.+.' a '.*.' c@ (multiplication has a higher fixity)
--   
--   [/annulator/]
--      @a '.*.' 'zero' = 'zero' = 'zero' '.*.' a@

class (MonoidA s, MonoidM s) => Semiring s

-- | A type class for idempotent semirings.
--   In idempotent semiring the following rule holds for all @a@
--   
--   [/addition is idempotent/]
--      @a '.+.' a = a@
--   
--   This rule cannot be checked automatically,
--   but needs to be provided by the user.
--   Again, the actual instances of this type class are obtained trivially as
--   
--   > instance IdempotentSemiring Foo where
--   

class Semiring s => IdempotentSemiring s

-- | In idempotent semirings that also have an 'Eq' instance, 
--   one can define the semiring order in a similar fashion as in a lattice.
--   The idempotence is required for the order to be reflexive.

(.<=.) :: (IdempotentSemiring s, Eq s) => s -> s -> Bool
s .<=. t = s .+. t == t

-- | A type class for semirings in which the constants can be distinguished from other elements.
--   This is useful for structures in which the general equality test might be complex
--   (or undecidable),
--   but the test for specific constants is rather simple.
--   Just as with the 'Semiring' type class, instances of 'SemiringC' are rather simple,
--   because they can be obtained writing
--   
--   > instance SemiringC Foo
--   
--   Note that semirings in particular have instances of 'MonoidA' and 'MonoidM' so that
--   the corresponding rules in combination with 'FindZero' and 'FindOne' are required.

class (Semiring s, FindZero s, FindOne s) => SemiringC s

-- | The Kleene algebra type class.
--   Kleene algebras are idempotent semirings with an additional 'star' operation that 
--   satisfies certain properties listed below.
--   We use the definition (and equivalent characterisation) presented in
--   
--   * Dexter Kozen, "A Completeness Theorem for Kleene Algebras and the Algebra of regular events",
--     Information and Computation 110:2 (1994) pp. 366-390,
--     <www.cs.cornell.edu/~kozen/papers/ka.pdf available here>.
--   
--   If @k@ is a type, the following rules must hold for all @a, b, x :: k@
--   for @k@ to become a Kleene algebra, where @\<=@ refers to the mathematical
--   order in an idempotent semiring:
--   
--   [/pre-fixpoint/]
--   
--      @'one' '.+.' a '.*.' 'star' a <= 'star' a@
--      and
--      @'one' '.+.' 'star' a '.*.' a <= 'star' a@
--   
--   [/star inclusion/]
--      
--      @b '.+.' a '.*.' x \<= x ===\>  'star' a '.*.' b \<= x@
--      and
--      @b '.+.' x '.*.' a \<= x ===\>  b '.*.' 'star' a \<= x@.
--   
--   The star inclusion property is equivalent to the following one,
--   which can be substituted instead.
--   
--   [/star inclusion'/]
--   
--      @a '.*.' b \<= b ===\> 'star' a '.*.' b \<= b@
--      and
--      @b '.*.' a \<= b ===\> b '.*.' 'star' a \<= b@

class IdempotentSemiring k => KleeneAlgebra k where
    {-# Minimal star | plus #-}

    -- | The star closure, which is a generalisation of the reflexive-transitive closure of
    --   relations.

    star :: k -> k
    star x = one .+. plus x

    -- | The Kleene closure, which is generalisation of the transitive closure of relations.

    plus :: k -> k
    plus x = x .*. star x

-- | A data type for wrapped numbers similar to the one presented in
--   <http://hackage.haskell.org/package/weighted-regexp weighted-regexp>.

newtype Number n = Number { number :: n }
    deriving ( Eq, Ord, Functor )

-- | Numbers are shown by removing their wrapper.

instance Show n => Show (Number n) where

    show = show . number

-- | Note: This instance declaration is consistent with the previously defined
--   instance for Show. 
--   Since 'Number' is just a wrapper it is considered non-existent 
--   when shown or written as a string,
--   thus simple numbers (belonging to the class 'Num') are read as wrapped numbers.

instance Read n => Read (Number n) where

    readsPrec = map (first Number) <.> readsPrec

instance MonoidA () where

    (.+.) _ _ = ()
    zero      = ()

instance MonoidA Bool where

    (.+.) = (||)
    zero  = False