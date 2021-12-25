----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Algebraic.Structures
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
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

{-# Language DeriveFunctor #-}

module Algebraic.Structures (
    
    -- * Additive semigroups and monoids with an additional type class for checking constants
    
    SemigroupA ( .. ),
    MonoidA ( .. ),
    FindZero ( .. ),
    
    -- * Multiplicative semigroups and monoids with an additional type class for checking constants
    
    SemigroupM ( .. ),
    MonoidM ( .. ),
    FindOne ( .. ),
    
    -- * Semirings with an additional type class for checking constants and idempotent semirings

    Semiring,
    SemiringC,
    IdempotentSemiring,
    (.<=.),

    -- * Kleene algebras with an additional type class for checking constants

    KleeneAlgebra ( .. ),
    (^*),
    KleeneAlgebraC,

    -- * A type class for abstract, additive inverses.

    GroupA ( .. ),

    -- * Data types with algebraic instances
    
    Number ( .. ),
    Tropical ( .. ),
    tropicalToNumber,
    tropicalToNum,
    numberToTropical,
    safeNumberToTropical,
    addTropical,
    multTropical,
    Regular,
    isComposite,
    noWord,
    emptyWord,
    (.><.),
    (.||.),
    repetition
    
    ) where

import Control.Applicative ( ZipList, Applicative ( .. ), liftA2 )
import Control.Arrow       ( first, (***) )
import Data.Function       ( on )
import Data.List           ( genericReplicate )
import Data.Maybe          ( isNothing )
import Data.Monoid         ( First ( .. ), mappend, Sum )
import Data.Ord            ( comparing )
import System.Random       ( Random, random, randomR, split )
import Test.QuickCheck     ( Arbitrary, arbitrary, frequency, suchThat )

import Auxiliary.General   ( (<.>) )

-- | 
-- A type class for additive semigroupgs.
-- The prerequisite of additivity is merely a notational choice 
-- and does not come with any additional requirements.
-- Instances of this type class are required to satisfy the rule
-- for all @a, b, c :: m@
--  
-- [/associativity/]
--    @a '.+.' (b '.+.' c) = (a '.+.' b) '.+.' c@

class SemigroupA a where
  {-# Minimal (.+.) #-}

  -- | A binary associative operation.

  infixr 5 .+.
  (.+.) :: a -> a -> a

-- | 
-- A type class for additive monoids.
-- The prerequisite of additivity is merely a notational choice 
-- and does not come with any additional requirements.
-- Instances of this type class are required to satisfy the rule
-- for all @a, b :: m@
-- 
-- [/neutrality/]
--    @a '.+.' 'zero' = a = 'zero' '.+.' a@
-- 
-- The additional functions have sensible default definitions that can be overwritten for
-- efficiency.
-- These functions are required to satisfy the following laws:
-- 
-- [/msum/]
--    @'msum' = 'foldr' ('.+.') 'zero'@
-- 
-- [/mtimes/]
--    @n >= 0 ==> 'mtimes' n x = 'msum' ('genericReplicate' n x)@

class SemigroupA a => MonoidA a where
    {-# Minimal zero | msum #-}

    -- | 
    -- A neutral element with respect to @('.+.')@.
    -- This element is automatically unique.

    zero  :: a
    zero = msum []
    
    -- | The sum of a list of elements.

    msum :: [a] -> a
    msum = foldr (.+.) zero

    -- | /n/-fold addition of the same element.

    mtimes :: Integer -> a -> a
    mtimes n x = msum (genericReplicate n x)

-- | 
-- A type class for multiplicative semigroups.
-- This structure is isomorphic to the 'SemigroupA' structure,
-- but has a different fixity for the monoid operation.
-- Instances of this type class are required to satisfy the rule
-- for all @a, b, c :: m@
--   
-- [/associativity/]
--    @a '.*.' (b '.*.' c) = (a '.*.' b) '.*.' c@

class SemigroupM m where
  {-# Minimal (.*.) #-}

  -- | A binary associative operation.

  infixr 6 .*.
  (.*.) :: m -> m -> m

-- | 
-- A type class for multiplicative monoids.
-- This structure is isomorphic to the 'MonoidA' structure,
-- but has a different fixity for the monoid operation.
-- Instances of this type class are required to satisfy the rule
-- for all @a, b :: m@
--  
-- [/neutrality/]
--    @a '.*.' 'one' = a = 'one' '.*.' a@
-- 
-- The additional functions have sensible default definitions that can be overwritten for
-- efficiency.
-- These functions are required to satisfy the following laws:
-- 
-- [/mproduct/]
--    @'product' = 'foldr' ('.*.') 'one'@
-- 
-- [/power/]
--    @y >= 0 ==> x '.^.' y = 'mproduct' ('genericReplicate' n x)@

class SemigroupM m => MonoidM m where

  {-# Minimal one | mproduct #-}

  -- | A neutral element with respect to @('.*.')@.
  --   This element is automatically unique.

  one   :: m
  one = mproduct []

  -- | The sum of a list of elements.

  mproduct :: [m] -> m
  mproduct = foldr (.*.) one

  -- | /n/-fold multiplication of the same element.

  (.^.) :: m -> Integer -> m
  x .^. n = mproduct (genericReplicate n x)

-- | A type class that provides a single predicate that can be used to check,
-- whether an element is \"zero\" or not.
-- The standalone type class is required to satisfy the rule
--
-- [/tertium non datur/]
--   @not . 'isZero' == 'isNotZero'@.
-- 
-- If a type @a@ has a 'FindZero' and a 'MonoidA' instance,
-- the following equality should hold for all @x :: a@
--  
-- @isZero x == True@ @\<===\>@ @x@ is mathematically equal to 'zero'

class FindZero a where
    {-# Minimal isZero | isNotZero #-}
    -- | A predicate that checks, whether an element is an abstract zero.

    isZero :: a -> Bool
    isZero = not . isNotZero

    -- | A predicate that checks, whether an element is an abstract zero.
    isNotZero :: a -> Bool
    isNotZero = not . isZero

-- | A type class that provides a single predicate that can be used to check,
-- whether an element is \"one\" or not.
-- The standalone type class is required to satisfy the rule
-- 
-- [/tertium non datur/]
--    @not . 'isOne' == 'isNotOne'@.
--   
-- If a type @m@ has a 'FindOne' and a 'MonoidM' instance,
-- the following equality should hold for all @x :: m@
--   
-- @isOne x == True@ @\<===\>@ @x@ is mathematically equal to 'one'

class FindOne m where

  {-# Minimal isOne | isNotOne #-}

  -- | A predicate that checks, whether an element is an abstract one.

  isOne :: m -> Bool
  isOne = not . isNotOne

  -- | A predicate that checks, whether an element is not an abstract one.

  isNotOne :: m -> Bool
  isNotOne = not . isOne

-- | The semiring type class.
-- Semirings do not provide any additional functions other than those already provided
-- by the underlying instances of 'MonoidA' and 'MonoidM'.
-- Thus every semiring instance is trivial and can be obtained by simply writing
--   
-- > instance Semiring Foo
--   
-- However, if @s@ is a type with a 'MonoidA' and a 'MonoidM' instance,
-- the following additional rules are required for a 'Semiring' instance
-- for all @a, b, c :: s@
--   
-- [/left distributivity/]
--    @(a '.+.' b) '.*.' c = a '.*.' c '.+. b '.*.' c@ (multiplication has a higher fixity)
--   
--   
-- [/right distributivity/]
--    @a '.*.' (b '.+.' c) = a '.*.' b '.+.' a '.*.' c@ (multiplication has a higher fixity)
-- 
--   
-- [/annulator/]
--    @a '.*.' 'zero' = 'zero' = 'zero' '.*.' a@

class (MonoidA s, MonoidM s) => Semiring s

-- | A type class for idempotent semirings.
-- In idempotent semiring the following rule holds for all @a@
--   
-- [/addition is idempotent/]
--    @a '.+.' a = a@
--   
-- This rule cannot be checked automatically,
-- but needs to be provided by the user.
-- Again, the actual instances of this type class are obtained trivially as
--   
-- > instance IdempotentSemiring Foo where
--   

class Semiring s => IdempotentSemiring s

-- | In additive semigroups that also have an 'Eq' instance, 
-- one can define the relation @(.<=.)@ by defining
-- @a .\<=. b = a .+. b == b@
-- This relation is always transitive.
-- Additionally, we have:
--
-- * (.\<=.) reflexive @\<===\>@ @('.+.')@ is idempotent.
-- * (.\<=.) antisymmetric @\<===@ @('.+.') is commutative.

infixr 2 .<=.

(.<=.) :: (SemigroupA asg, Eq asg) => asg -> asg -> Bool
s .<=. t = s .+. t == t

-- | A type class for semirings in which the constants can be distinguished from other elements.
-- This is useful for structures in which the general equality test might be complex
-- (or undecidable),
-- but the test for specific constants is rather simple.
-- Just as with the 'Semiring' type class, instances of 'SemiringC' are rather simple,
-- because they can be obtained writing
--   
-- > instance SemiringC Foo
--   
-- Note that semirings in particular have instances of 'MonoidA' and 'MonoidM' so that
-- the corresponding rules in combination with 'FindZero' and 'FindOne' are required.

class (Semiring s, FindZero s, FindOne s) => SemiringC s

-- | The Kleene algebra type class.
-- Kleene algebras are idempotent semirings with an additional 'star' operation that 
-- satisfies certain properties listed below.
-- We use the definition (and equivalent characterisation) presented in
--   
-- * Dexter Kozen, \"A Completeness Theorem for Kleene Algebras and the Algebra of regular events\",
--   Information and Computation 110:2 (1994) pp. 366-390,
--   <www.cs.cornell.edu/~kozen/papers/ka.pdf available here>.
--   
-- If @k@ is a type, the following rules must hold for all @a, b, x :: k@
-- for @k@ to become a Kleene algebra, where @\<=@ refers to the mathematical
-- order in an idempotent semiring:
--   
-- [/pre-fixpoint/]
--  
--  * @'one' '.+.' a '.*.' 'star' a <= 'star' a@
--  * @'one' '.+.' 'star' a '.*.' a <= 'star' a@
--   
-- [/star inclusion/]
--   
--  * @a '.*.' b \<= b ===\> 'star' a '.*.' b \<= b@
--  * @b '.*.' a \<= b ===\> b '.*.' 'star' a \<= b@

class IdempotentSemiring k => KleeneAlgebra k where

  {-# Minimal star | plus #-}

  -- | The star closure, which is a generalisation of the reflexive-transitive closure of
  -- relations.

  star :: k -> k
  star x = one .+. plus x

  -- | The Kleene closure, which is generalisation of the transitive closure of relations.

  plus :: k -> k
  plus x = x .*. star x

-- | This is the same as 'star', but with the @-XPostfixOperators@ extension
--   switched on this allows a postfix usage.

(^*) :: KleeneAlgebra k => k -> k
(^*) = star

-- | A type class for Kleene algebras in which the constants can be distinguished from other
-- elements.
-- Its behaviour is very similar to the one discussed for 'SemiringC'.

class (KleeneAlgebra k, FindZero k, FindOne k) => KleeneAlgebraC k

-- | A type class for additive groups.
-- The following must hold for all @x, y :: n@:
-- 
-- [/subtraction/]
--   @x '.-.' y = x '.+.' 'neg' y@
-- [/inverse/]
--   @'neg' x = 'zero' '.-.' x

class MonoidA a => GroupA a where

  {-# Minimal inverseA | (.-.) #-}
  
  -- | The inversion operation that returns the additive inverse of its argument.

  inverseA :: a -> a
  inverseA = (.-.) zero

  -- | The subtraction of two values in an additive group.

  (.-.) :: a -> a -> a
  x .-. y = x .+. inverseA y

instance GroupA () where

  inverseA  = const ()
  (.-.) _ _ = ()

instance Num n => GroupA (Number n) where

  inverseA = negate
  (.-.)    = (-)

instance GroupA n => GroupA (ZipList n) where

  inverseA = fmap inverseA
  (.-.)    = liftA2 (.-.)

instance (GroupA n, GroupA n') => GroupA (n, n') where

  inverseA              = inverseA *** inverseA
  (al, ar) .-. (bl, br) = (al .-. bl, ar .-. br)

instance GroupA n => GroupA (x -> n) where

  inverseA = fmap inverseA
  (.-.)    = liftA2 (.-.)

-- | A data type for wrapped numbers similar to the one presented in
-- <http://hackage.haskell.org/package/weighted-regexp weighted-regexp>.

newtype Number n = Number { number :: n }
  deriving ( Eq, Ord, Functor )

instance Num n => Num (Number n) where
  (+)         = Number <.> ((+) `on` number)
  (*)         = Number <.> ((*) `on` number)
  (-)         = Number <.> ((-) `on` number)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = Number . fromInteger

-- | Numbers are shown by removing their wrapper.

instance Show n => Show (Number n) where

  show = show . number

-- | This instance declaration is consistent with the previously defined
-- instance for Show. 
-- Since 'Number' is just a wrapper it is considered non-existent 
-- when shown or written as a string,
-- thus simple numbers (belonging to the class 'Num') are read as wrapped numbers.

instance Read n => Read (Number n) where

  readsPrec = map (first Number) <.> readsPrec

-- | An 'Arbitrary' instance that simply lifts the underlying instance to the 'Number' level.

instance Arbitrary n => Arbitrary (Number n) where

  arbitrary = fmap Number arbitrary

instance Random n => Random (Number n) where

  randomR (l, u) g = (Number n, g')
    where (n, g') = randomR (number l, number u) g

  random = first Number . random

-- | 
-- A maybe-like wrapper for numbers introducing a smallest and a largest element.
-- This data type is used in Dijkstra's algorithm
-- (via correct instantiation of Kleene's algorithm).

data Tropical w = Tropical { weight :: w } | Min | Max
  deriving ( Eq, Functor )

-- | A trivial instance of show to denote 'Min' and 'Max' separately.

instance Show w => Show (Tropical w) where

  show (Tropical w) = show w
  show Min        = "Zero."
  show Max        = "Infinity."

-- | 'Max' is larger than any other element, 'Min' is smaller than any other element, 
-- but the restriction of the order to the objects without these two
-- yields the original order.

instance Ord w => Ord (Tropical w) where

  compare Min Min = EQ
  compare Min _   = LT
  compare _   Min = GT
  compare Max Max = EQ
  compare Max _   = GT
  compare _   Max = LT
  compare w1  w2  = comparing weight w1 w2

-- | An 'Arbitrary' instance that rarely chooses the extremes and usually provides the
-- intermediate cases.

instance (Num w, Ord w, Arbitrary w) => Arbitrary (Tropical w) where

  arbitrary = frequency [(1, return Min), 
                         (1, return Max), 
                         (40, fmap Tropical (arbitrary `suchThat` (> 0)))]

-- | Drawing this number is interpreted as drawing 'Min'.

drawMin :: Int
drawMin = 0

-- | Drawing this number is interpreted as drawing 'Max'.

drawMax :: Int
drawMax = 41

instance (Ord w, Num w, Random w) => Random (Tropical w) where

  random g | c == drawMin = (Min, g')
           | c == drawMax = (Max, g')
           | otherwise    = first (Tropical . abs) (random g')
          where (c, g') = randomR (drawMin, drawMax) g

  randomR (l, u)                   g      | l == u       = (l, fst (split g))
                                          | l >  u       = randomR (u, l) g
  randomR (Min,        Tropical u) g      | c == drawMin = (Min, g')
                                          | otherwise    = mkRndTropical (randomR (fromInteger 0, u) g')
                                          where (c, g') = randomR (drawMin, drawMax - 1) g
  randomR (Min,        Max)        g                     = random g

  randomR (Tropical l,      Tropical u) g                = mkRndTropical (randomR (l, u) g)
  -- In this case lb = Tropical l for some fitting l.
  randomR (lb, Max)        g              | c == drawMax = (Max, g')
                                          | otherwise    = first (lb `addTropicalNum`) (random g')
                                          where (c, g') = randomR (1 + drawMin, drawMax) g

mkRndTropical :: (w, g) -> (Tropical w, g)
mkRndTropical = first Tropical
  
-- | Transforms a weight into a number. This is a partial function, because there is
--   no sensible representation of @Min@ or @Max@ in @Number@s.

tropicalToNumber :: Tropical w -> Number w
tropicalToNumber (Tropical x) = Number x
tropicalToNumber _            = error "Not a number."

-- | Transforms a number into a weight by simply replacing the wrapper.

numberToTropical :: Number n -> Tropical n
numberToTropical = Tropical . number

-- | Transforms a 'Tropical' value into the underlying 'Num' type.
-- 'Min' is interpreted as zero (thus this function is not injective),
-- and 'Max' is interpreted as infinity.

tropicalToNum :: Num n => Tropical n -> n
tropicalToNum Min          = 0
tropicalToNum (Tropical w) = w
tropicalToNum _            = error "Structures.tropicalToNum: Infinity is not a number."

-- | Safely transforms a number into a weight by checking whether the number is greater
-- than zero first.

safeNumberToTropical :: (Ord n, Num n) => Number n -> Tropical n
safeNumberToTropical n | n < 0     = Min
                       | otherwise = numberToTropical n

-- | The numeric addition of tropical value,
-- which coincides with the multiplication in the tropical semiring.

addTropical :: SemigroupA a => Tropical a -> Tropical a -> Tropical a
addTropical = (.*.)

addTropicalNum :: Num n => Tropical n -> Tropical n -> Tropical n
addTropicalNum x y = fmap number (fmap Number x `addTropical` fmap Number y)

-- | The numeric multiplication of tropical values.

multTropical :: SemigroupM m => Tropical m -> Tropical m -> Tropical m
multTropical Min          _            = Min
multTropical _            Min          = Min
multTropical Max          _            = Max
multTropical _            Max          = Max
multTropical (Tropical x) (Tropical y) = Tropical (x .*. y)

-- | A data type for regular expressions.

data Regular a = NoWord 
               | Empty 
               | Single a 
               | Binary BinOp (Regular a) (Regular a)
               | Star (Regular a)

-- | A predicate that checks whether the regular expression is one of the base cases or not

isComposite :: Regular a -> Bool
isComposite (Binary {}) = True
isComposite (Star _)    = True
isComposite _           = False

-- | The empty regular expression.

noWord :: Regular a
noWord = NoWord

-- | The empty word.

emptyWord :: Regular a
emptyWord = Empty

-- | A single letter regular expression.

single :: a -> Regular a
single = Single

-- | Composition of two regular expressions.

infixr 6 .><.
(.><.) :: Regular a -> Regular a -> Regular a
(.><.) = Binary Composition

-- | Alternative of two regular expressions.

infixr 5 .||.
(.||.) :: Regular a -> Regular a -> Regular a
(.||.) = Binary Alternative

-- | Repetition of a regular expression

repetition :: Regular a -> Regular a
repetition = Star

-- | Data type for the binary operations on regular expressions.

data BinOp = Alternative | Composition

-- | A show instance that uses pretty unicode symbols for the binary regular expression operations.

instance Show BinOp where

    show Alternative = "\x2295"
    show Composition = "\x229B"

-- | Returns the numerical precedences of the abstract binary operations on regular expressions.

precedence :: BinOp -> Int
precedence Alternative = 5
precedence Composition = 6

-- | A pretty-printed instance that uses certain unicode symbols to represent the mathematical
--   background, for instance the empty regular expression is shown as a unicode epsilon.

instance Show a => Show (Regular a) where

  showsPrec _ NoWord         = showString "\x2205"
  showsPrec _ Empty          = showString "\x03B5"
  showsPrec _ (Single a)     = shows a
  showsPrec p (Binary b r s) = showParen (p > p') (showsPrec p' r . shows b . showsPrec p' s)
                                  where p' = precedence b
  showsPrec p (Star r)       = showParen (isComposite r) (showsPrec p r) . showString "*"

-- | This instance chooses single letters with a high probability, followed by binary operations,
-- star closure, the empty word and no word.

instance Arbitrary a => Arbitrary (Regular a) where

  arbitrary = frequency [
                         (1, return noWord),
                         (2, return emptyWord),
                         (10, fmap single arbitrary),
                         (4, fmap (uncurry (.||.)) arbitrary),
                         (4, fmap (uncurry (.><.)) arbitrary),
                         (3,  fmap repetition arbitrary)
                          ]

-- Instance declarations

instance SemigroupA () where
  (.+.) _ _ = ()

-- | The disjunction of Boolean values.

instance SemigroupA Bool where
  (.+.) = (||)

-- | The additive number operation.
instance Num n => SemigroupA (Number n) where
  (.+.) = (+)

-- | Structurally similar to the 'Monoid' instance for 'Sum'.
instance Num n => SemigroupA (Sum n) where
  (.+.) = mappend

-- | The Min-Plus instance, where addition is the minimum of two values.

instance Ord t => SemigroupA (Tropical t) where
  (.+.) = min

-- | This is only a monoid up to the induced language of a regular expression!

instance SemigroupA (Regular r) where
  (.+.) = (.||.)

-- | 'ZipList's form an additive semigroup with the pointwise addition.
-- The same strategy works every other 'Applicative' instance.

instance SemigroupA a => SemigroupA (ZipList a) where
  (.+.) = liftA2 (.+.)

-- | 'First' is a monoid by design and we consider it to be additive.

instance SemigroupA (First a) where
  (.+.) = mappend

-- | Direct products of additive monoids are additive monoids as well by
--   using the respective addition in the respective component.

instance (SemigroupA a, SemigroupA a') => SemigroupA (a, a') where
  (al, ar) .+. (bl, br) = (al .+. bl, ar .+. br)

-- | Functions into an additive semigroup form a semigroup as well by defining
--   addition pointwise, i.e. f .+. g = \x -> f x .+. g x

instance SemigroupA a => SemigroupA (x -> a) where
  (.+.) = liftA2 (.+.)

instance MonoidA () where
  zero = ()

instance MonoidA Bool where

  zero       = False
  msum       = or
  mtimes n b = n > 0 && b

instance Num n => MonoidA (Number n) where
  zero  = 0

-- | The Min-Plus instance, where addition is the minimum of two values.

instance Ord t => MonoidA (Tropical t) where
  zero  = Max

-- | This is only a monoid up to the induced language of a regular expression!

instance MonoidA (Regular r) where
  zero  = noWord

instance MonoidA a => MonoidA (ZipList a) where
  zero  = pure zero

instance MonoidA (First a) where
  zero  = First Nothing

instance (MonoidA a, MonoidA a') => MonoidA (a, a') where
  zero                  = (zero, zero)

instance MonoidA a => MonoidA (x -> a) where
  zero  = pure zero

-- | Every element is 'zero'.

instance FindZero () where

  isZero    = const True
  isNotZero = const False

-- | 'False' is 'zero'.

instance FindZero Bool where

  isZero    = not
  isNotZero = id

-- | 'Number 0' is 'zero'.

instance (Eq n, Num n) => FindZero (Number n) where

  isZero    = (0 ==) . number
  isNotZero = (0 /=) . number

-- | The greatest element is neutral with respect to the minimum operation.

instance FindZero (Tropical t) where

  isZero Max = True
  isZero _   = False

-- | The empty word is the 'zero' element with respect to the induced language.

instance FindZero (Regular r) where

  isZero Empty = True
  isZero _     = False

-- | The 'zero' element is @'First' 'Nothing'@.

instance FindZero (First a) where

  isZero = isNothing . getFirst

-- | The 'zero' of pairs is the pair of 'zero'es.

instance (FindZero a, FindZero a') => FindZero (a, a') where

  isZero = uncurry (&&) . (isZero *** isZero)

instance SemigroupM () where
  (.*.) _ _ = ()

-- | The conjunction of Boolean values.

instance SemigroupM Bool where
  (.*.) = (&&)

-- | The multiplicative number operation.

instance Num n => SemigroupM (Number n) where
  (.*.) = (*)

-- | The Min-Plus instance in which the multiplication of 'Tropical' values is their additive
-- semigroup operation.

instance SemigroupA t => SemigroupM (Tropical t) where

  Max .*. _   = Max
  _   .*. Max = Max
  Min .*. w   = w
  w   .*. Min = w
  w1  .*. w2  = Tropical (weight w1 .+. weight w2)

-- | The composition of regular expressions is a multiplicative semigroup with respect to
-- the induced language.

instance SemigroupM (Regular r) where
  (.*.) = (.><.)

-- | 'ZipList's form an multiplicative semigroup with the pointwise multiplication.
-- The same strategy works every other 'Applicative' instance.

instance SemigroupM a => SemigroupM (ZipList a) where
  (.*.) = liftA2 (.*.)

-- | List concatenation with the empty list is a multiplicative semigroup.

instance SemigroupM [a] where
  (.*.)    = (++)

-- | 'First' is an 'Applicative' instance and thus the implementation is the same as for 'ZipList'.

instance SemigroupM m => SemigroupM (First m) where
  (.*.) = liftA2 (.*.)

-- | Usual product construction.

instance (SemigroupM m, SemigroupM m') => SemigroupM (m, m') where
  (ml, mr) .*. (nl, nr) = (ml .*. nl, mr .*. nr)

-- | Functions into an multiplicative semigroup form a semigroup as well by defining
--   multiplication pointwise, i.e. f .*. g = \x -> f x .*. g x.

instance SemigroupM m => SemigroupM (x -> m) where
  (.*.) = liftA2 (.*.)

instance MonoidM () where
  one       = ()

-- | The conjunction of Boolean values.

instance MonoidM Bool where

  one      = True
  mproduct = and
  b .^. n  = n <= 0 || b

-- | The multiplicative number operation.

instance Num n => MonoidM (Number n) where
  one   = 1

-- | The Min-Plus instance in which the multiplication of 'Tropical' values is their additive,
-- monoidal operation.

instance MonoidA t => MonoidM (Tropical t) where
  one = Min

-- | The composition of regular expressions is a multiplicative monoid with respect to
-- the induced language.

instance MonoidM (Regular r) where
  one   = emptyWord

-- | 'ZipList's form an multiplicative monoid with the pointwise multiplication.
-- The same strategy works every other 'Applicative' instance.

instance MonoidM a => MonoidM (ZipList a) where
  one   = pure one

-- | List concatenation with the empty list is a multiplicative monoid.

instance MonoidM [a] where
  one      = []
  mproduct = concat

-- | 'First' is an 'Applicative' instance and thus the implementation is the same as for 'ZipList'.

instance MonoidM m => MonoidM (First m) where
  one   = pure one

-- | Usual product construction.

instance (MonoidM m, MonoidM m') => MonoidM (m, m') where
  one                   = (one, one)

-- | Functions into an multiplicative semigroup form a semigroup as well by defining
--   multiplication pointwise, i.e. f .*. g = \x -> f x .*. g x.

instance MonoidM m => MonoidM (x -> m) where
  one   = pure one

-- | Every element is a multiplicative unit.

instance FindOne () where

  isOne    = const True
  isNotOne = const False

-- | The value 'True' is a multiplicative unit for @('&&')@.

instance FindOne Bool where

  isOne    = id
  isNotOne = not

-- | The value 'Number 1' is a multiplicative unit.

instance (Num n, Eq n) => FindOne (Number n) where

  isOne (Number 1) = True
  isOne _          = False

-- | The multiplicative unit with respect to the tropical multiplication is 'Min'.

instance FindOne (Tropical t) where

  isOne Min = True
  isOne _   = False

-- | The empty word is a neutral element for the composition of regular expressions with
-- respect to the induced language.

instance FindOne (Regular r) where

  isOne Empty = True
  isOne _     = False

-- | The multiplicative unit of list concatenation is the empty list.

instance FindOne [a] where

  isOne = null

-- | The multiplicative unit of the doubly listed monoid operation is the doubly lifted unit.

instance FindOne a => FindOne (First a) where

  isOne (First (Just x)) = isOne x
  isOne _                = False

-- | The multiplicative unit of pairs is the pair of multiplicative units.

instance (FindOne a, FindOne b) => FindOne (a, b) where

  isOne = uncurry (&&) . (isOne *** isOne)

-- | The trivial semiring.

instance Semiring ()

-- | The Boolean semiring.

instance Semiring Bool

-- | The usual, numerical semiring.

instance Num n => Semiring (Number n)

-- | The tropical semiring.

instance (Ord t, MonoidA t) => Semiring (Tropical t)

-- | The unreduced free semiring.

instance Semiring (Regular r)

-- | The pointwise semiring of 'ZipList's.

instance Semiring s => Semiring (ZipList s)

-- | The product semiring.

instance (Semiring s, Semiring s') => Semiring (s, s')

-- | The function semiring with pointwise operations.

instance Semiring s => Semiring (x -> s)

instance SemiringC ()
instance SemiringC Bool
instance (Num n, Eq n) => SemiringC (Number n)
instance (Ord t, MonoidA t) => SemiringC (Tropical t)
instance SemiringC (Regular r)
instance (SemiringC s, SemiringC s') => SemiringC (s, s')

instance IdempotentSemiring ()
instance IdempotentSemiring Bool
instance (Ord t, MonoidA t) => IdempotentSemiring (Tropical t)

-- | This is only true with respect to the induced language.

instance IdempotentSemiring (Regular r)

instance IdempotentSemiring s => IdempotentSemiring (ZipList s)
instance (IdempotentSemiring s, IdempotentSemiring s') => IdempotentSemiring (s, s')
instance IdempotentSemiring s => IdempotentSemiring (x -> s)

-- | The trivial Kleene algebra.

instance KleeneAlgebra () where

  star = id
  plus = id

-- | The Boolean Kleene algebra.
-- By definition @b '.^.' 0 = 'one' = True@ and since @('.+.') = (||)@ we get that
--   @'star' b = 'one'@.

instance KleeneAlgebra Bool where

  star = const one
  plus = id

-- | The tropical Kleene algebra.
-- By definition we have: @ w '.^.' 0 = 'one' = 'Min'@ and @w '.^.' n@ is
-- either @n * w@ in the overloaded mathematical sense
-- (if @w &#x2209; {'Min', 'Max'}@), @Min@ (if @w = 'Min'@) or @'Max'@ (if @w = 'Max'@). 
-- By the order extension @'Min'@ is smaller than any other
-- element and thus the Kleene closure of any element is @'Min' = 'one'@.

instance (Ord t, MonoidA t) => KleeneAlgebra (Tropical t) where

  star = const one
  plus = id

-- | This is only true with respect to the induced language.

instance KleeneAlgebra (Regular r) where

  star = repetition

-- | Star closure and Kleene closure is carried out pointwise.
-- This is possible for all 'Applicative' instances.

instance KleeneAlgebra k => KleeneAlgebra (ZipList k) where

  star = fmap star
  plus = fmap plus

instance (KleeneAlgebra k, KleeneAlgebra k') => KleeneAlgebra (k, k') where

  star = star *** star
  plus = plus *** plus

instance KleeneAlgebra k => KleeneAlgebra (x -> k) where

  star = fmap star
  plus = fmap plus

instance KleeneAlgebraC ()
instance KleeneAlgebraC Bool
instance (Ord t, MonoidA t) => KleeneAlgebraC (Tropical t)
instance KleeneAlgebraC (Regular r)
instance (KleeneAlgebraC k, KleeneAlgebraC k') => KleeneAlgebraC (k, k')
