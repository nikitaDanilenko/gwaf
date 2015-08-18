----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.General
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module contains some general purpose functions and combinators as well as
-- merge-based lists operations.
-- Several specialised cases of merging operations are provided in the package
-- <http://hackage.haskell.org/package/data-ordlist data-ordlist>,
-- but are less general, because no combination functions are used.
-- 
-- Additionally,
-- this module provides the type synonyms that are used in graph-theoretic applications
-- in this library.

module Auxiliary.General (

    -- * Combinators, operators and higher-order functions
    
    (<.>) ,
    scaleLeft,
    wrap,
    idThird,
    onSecond,
    stepwise,
    stepwise2,

    -- * Comparison

    minBy,
    dcomparing,
    compareFirsts,

    -- * Functions on (association) lists

    orderedLookup,
    mergeWith,
    consWith,
    evens,
    odds,
    evensOdds,

    -- ** Union functions

    unionByWith,
    unionByFstWith,
    unionWith,
    
    -- ** Intersection functions

    intersectionByWith,
    intersectionByFstWith,
    intersectionBy,
    intersectionWith,

    -- ** Complement functions

    differenceByWith,
    differenceWith,
    differenceByFstWith,
    differenceBy,
    differenceByFst,

    -- ** Symmetric difference

    symmetricDifferenceByWith,
    symmetricDifferenceWith,
    symmetricDifferenceByFstWith,
    symmetricDifferenceByFst,

    -- * Graph-related types
    
    Key,
    Arc,
    Row,
    Mat,

    -- * 'Arbitrary' helper function
    
    mkArbitrary

    ) where

import Control.Arrow   ( (&&&) )
import Data.Ord        ( comparing )
import Test.QuickCheck ( Arbitrary, Gen, sized, choose, suchThat, arbitrary ) 

-- | 
-- This function is known as @('.:')@ from the Haskell wiki and the functional
-- graph library <http://hackage.haskell.org/package/fgl fgl> by Martin Erwig.
-- It is used to compose a function
-- @f :: a -> b -> c@ with a function @g :: c -> d@ such that
-- 
-- > (g <.> f) x y = g (f x y).
-- 
-- The infix binding of @('<.>')@ is exactly one unit less than the one of @('.')@,
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

-- | Lifts a binary function to a function that operates on a structure in its second argument.
-- The idea is reminiscent of a scalar multiplication for vectors,
-- which is obtained from lifting the field multiplication to the vector level.

scaleLeft :: Functor f => (a -> b -> c) -> a -> f b -> f c
scaleLeft = (fmap .)

-- | Maps a pair whose second component is a 'Functor' to a 'Functor' containing the pair.
-- For example @wrap ('a', Just 5) = Just ('a', 5)@.

wrap :: Functor f => (a, f b) -> f (a, b)
wrap = uncurry (fmap . (,))

-- | This function takes a binary function @f@ and two pairs @(i, x)@ and @(j, y)@ and returns
-- the pair @(i, f x y)@.

onSecond :: (a -> b -> c) -> (k, a) -> (k, b) -> (k, c)
onSecond f (i, x) (_, y) = (i, f x y)

-- | Stepwise computation of a result.
-- The first argument is used to compute a possible improvement and
-- the second argument is the value that is improved as long as possible.

stepwise :: (a -> Maybe a) -> a -> a
stepwise improve = fun where
  fun current = maybe current fun (improve current)

-- | Stepwise computation of a result using a binary function.
-- The first argument computes a possible improvement.
-- The first argument is applied to the second and third argument as long as the result is not
-- 'Nothing'.

stepwise2 :: (a -> a -> Maybe (a, a)) -> a -> a -> (a, a)
stepwise2 = curry . stepwise . uncurry

-- | A non-overloaded version of 'min' that takes an ordering function as an argument
-- and returns the smaller one. 

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y = case cmp x y of
                  LT -> x
                  _  -> y

-- | This function is a generalisation of the function 'Data.Ord.comparing'.
-- While 'Data.Ord.comparing' takes a function that is applied to two values of the same
-- type to map them into a comparable type,
-- the function 'dcomparing' takes two different functions with the same co-domain.
-- Thus we have
-- 
-- > Data.Ord.comparing f = dcomparing f f
-- 
-- but not necessarily vice versa, because @f@ can be polymorphic (i.e. @f = 'fst'@).

dcomparing :: Ord c => (a -> c) -> (b -> c) -> a -> b -> Ordering
dcomparing f g x y = compare (f x) (g y)

-- | Compares the first components of two given pairs.
-- This is /not/ the same as @'Data.Ord.comparing' 'fst'@,
-- because the two arguments have different types.

compareFirsts :: Ord a => (a, b) -> (a, c) -> Ordering
compareFirsts = dcomparing fst fst

-- | This function is a version of 'Prelude.lookup' that assumes the given list to be ordered with
--   respect to the keys. This way once a key is encountered that is larger than the one we are
--   looking for, the search can be stopped, since it cannot be contained in the remaining list.
--   The precondition that the list is increasingly sorted with respect to the indices is not 
--   checked.

orderedLookup :: Ord k => k -> [(k, v)] -> Maybe v
orderedLookup _   []                          = Nothing
orderedLookup key ((k, v) : kvs ) | k == key  = Just v
                                  | k < key   = orderedLookup key kvs
                                  | otherwise = Nothing

-- | The general pattern of merging (abstractly) ordered lists.
-- This function is fully parametric in every possible case that can occur during
-- the merging process.

mergeWith :: (a -> b -> Ordering) -- ^ comparison function
         -> ([b] -> c)           -- ^ operation if the first list is empty
         -> ([a] -> c)           -- ^ operation if the second list is empty
         -> (a -> b -> c -> c)   -- ^ operation in the @EQ@ case
         -> (a -> b -> c -> c)   -- ^ operation in the @LT@ case
         -> (a -> b -> c -> c)   -- ^ operation in the @GT@ case
         -> [a]                  -- ^ first argument list
         -> [b]                  -- ^ second argument list
         -> c
mergeWith cmp lEmpty rEmpty lt eq gt = merge where
    merge [] ys  = lEmpty ys
    merge xs []  = rEmpty xs
    merge l@(x:xs) m@(y:ys) = case cmp x y of
                                LT -> lt x y (merge xs  m)
                                EQ -> eq x y (merge xs ys)
                                GT -> gt x y (merge l  ys)

-- | A union merging pattern.
-- This function takes a comparison operator and a function that is applied in case of
-- equality.
-- It returns a binary list function that behaves like a union
-- on (abstractly) ordered lists.
-- If an element occurs in both lists,
-- the two occurrences are combined with the supplied combination function and the result is
-- added to the result list.
-- Otherwise (if the element occurs in only one of the lists) the element is added to the
-- result list unchanged.
-- For example we have the following results:
-- 
-- > unionByWith compare (+) [1,2,3] [2,3,5] 
-- >    = [1,4,6,5]
-- > unionByWith (comparing fst) (onSecond (++)) 
-- >             [(1, "M"), (3, "n"), (4, "i")] [(2, "o"), (3, "o"), (5, "d")] 
-- >    = [(1,"M"),(2,"o"),(3,"no"),(4,"i"),(5,"d")]

unionByWith :: (a -> a -> Ordering) -> (a -> a -> a) -> [a] -> [a] -> [a]
unionByWith cmp op = mergeWith cmp id id (consWith const) (consWith op) (consWith (flip const))

-- | An instance of 'unionByWith' that uses ordered types and the respective comparison
-- operation.

unionWith :: Ord a => (a -> a -> a) -> [a] -> [a] -> [a]
unionWith = unionByWith compare

-- | An instance of 'unionByWith' that operates on association lists and uses ordered
-- keys for the comparison of elements in the list.

unionByFstWith :: Ord k => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
unionByFstWith = unionByWith (comparing fst) . onSecond

-- | An intersection merging pattern.
-- This function takes a comparison operator and a function that is applied in case of equality.
-- It returns a binary list function that behaves like an intersection of abstractly ordered lists.
-- If an element occurs in both lists, these occurrences are combined with the supplied combination
-- function and the result is added to the result list.
-- Otherwise (if an element occurs in only one of the lists) the element is ignored.
-- For example we have the following applications:
-- 
-- > intersectionByWith compare (+) [1,2,3] [2,3,5] 
-- >    = [4, 6]
-- > intersectionByWith (comparing fst) (onSecond (++)) 
-- >                 [(1, "M"), (3, "n"), (4, "i")] [(2, "o"), (3, "o"), (5, "d")]
-- >    = [(3, "no")]
-- > intersectionByWith compare (onSecond (++)) 
-- >                 [(1, "M"), (3, "n"), (4, "i")] [(2, "o"), (3, "o"), (5, "d")]
-- >    = []
-- 
-- The last example is due to the fact that @(3, "n")@ and @(3, "o")@ are different pairs in
-- the default order on pairs.

intersectionByWith :: (a -> b -> Ordering) -> (a -> b -> c) -> [a] -> [b] -> [c]
intersectionByWith cmp op = mergeWith cmp (const []) (const []) idThird (consWith op) idThird

-- | An instance of 'intersectionByWith' that uses ordered keys for the comparison of values
-- in an association list.
-- The supplied operation is applied to the values only.

intersectionByFstWith :: Ord k => (a -> b -> c) -> [(k, a)] -> [(k, b)] -> [(k, c)]
intersectionByFstWith = intersectionByWith compareFirsts . onSecond

-- | An instance of 'intersectionByWith' that uses the 'compare' function of ordered types.

intersectionWith :: Ord a => (a -> a -> a) -> [a] -> [a] -> [a]
intersectionWith = intersectionByWith compare

-- | A left-biased intersection that uses a supplied comparison function.
-- Since the comparison function is heterogeneous, so is the resulting intersection.

intersectionBy :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
intersectionBy cmp = intersectionByWith cmp const

-- | A complement merging pattern.
-- This function takes a comparison operator and a function that is applied in case of equality.
-- It returns a binary list function that behaves like a relative complement 
-- of abstractly ordered lists.
-- If an element occurs in both lists, these occurrences are combined with the supplied combination
-- function.
-- The result of this combination is checked for being 'Nothing' or @'Just' r@.
-- If an element occurs in the first list only, it is added to the result
-- and if an element occurs in the second list only, it is ignored.
-- In the first case, nothing is added to the result list,
-- in the second case the element @r@ is added to the result list.
-- Below are some example applications of 'differenceByWith'.
-- 
-- > differenceByWith compare (\_ _ -> Nothing) [1, 2, 3] [2, 3, 5]
-- >    = [1]
-- > differenceByWith compare (Just <.> (+)) [1, 2, 3] [2, 3, 5]
-- >    = [1, 4, 6]
-- > differenceByWith (comparing fst) (\_ _ -> Nothing) 
-- >             [(1, "M"), (3, "n"), (4, "i")] [(2, "o"), (3, "o"), (5, "d")]
-- >    = [(1,"M"),(4,"i")]

differenceByWith:: (a -> b -> Ordering) -> (a -> b -> Maybe a) -> [a] -> [b] -> [a]
differenceByWith cmp op = mergeWith cmp (const []) id (consWith const) (maybe id (:) <.> op) idThird

-- | An instance of 'differenceByWith' that uses the 'compare' function of ordered types.

differenceWith :: Ord a => (a -> a -> Maybe a) -> [a] -> [a] -> [a]
differenceWith = differenceByWith compare

-- | A version of 'differenceByWith' that uses ordered keys for the comparison of values in
-- an association list.

differenceByFstWith :: Ord k => (a -> b -> Maybe a) -> [(k, a)] -> [(k, b)] -> [(k, a)]
differenceByFstWith op = differenceByWith compareFirsts (wrap <.> onSecond op)

-- | A variant of 'differenceByWith' that takes a comparison function and then removes all values
-- contained in the second list from the first one.

differenceBy :: (a -> b -> Ordering) -> [a] -> [b] -> [a]
differenceBy cmp = differenceByWith cmp (\_ _ -> Nothing)

-- | A version of 'differenceBy' that uses the 'compare' function on ordered keys.

differenceByFst :: Ord k => [(k, v)] -> [(k, w)] -> [(k, v)]
differenceByFst = differenceBy compareFirsts

-- | A merging scheme for symmetric difference.
-- This function takes a comparison operator and a function that is applied in case of equality.
-- It returns a binary list function that behaves like a symmetric difference
-- of abstractly ordered lists.
-- If an element occurs in both lists, these occurrences are combined with the supplied combination
-- function.
-- The result of this combination is checked for being 'Nothing' or @'Just' r@.
-- In the first case, nothing is added to the result list,
-- in the second case the element @r@ is added to the result list.
-- If an element occurs in only one of the lists, it is added to the result list unchanged
-- Below are some example applications of 'symmetricDifferenceByWith'.
-- 
-- > symmetricDifferenceByWith compare (\_ _  -> Nothing) [1,2,3] [2,3,5]
-- >    = [1,5]
-- > symmetricDifferenceByWith compare (Just <.> (+)) [1,2,3] [2,3,5]
-- >    = [1,4,6,5]
-- > symmetricDifferenceByWith (comparing fst) (\_ _ -> Nothing) 
-- >                           [(1, "M"), (3, "n"), (4, "i")] [(2, "o"), (3, "o"), (5, "d")]
-- >    = [(1,"M"),(2,"o"),(4,"i"),(5,"d")]

symmetricDifferenceByWith :: (a -> a -> Ordering) -> (a -> a -> Maybe a) -> [a] -> [a] -> [a]
symmetricDifferenceByWith cmp op = 
    mergeWith cmp id id (consWith const) (maybe id (:) <.> op) (consWith (flip const))

-- | An instance of 'symmetricDifferenceByWith' that uses the 'compare' function of ordered types.

symmetricDifferenceWith :: Ord a => (a -> a -> Maybe a) -> [a] -> [a] -> [a]
symmetricDifferenceWith = symmetricDifferenceByWith compare

-- | An instance of 'symmetricDifferenceByWith' that uses ordered keys for the comparison of
-- elements in an association list.

symmetricDifferenceByFstWith :: Ord k => (v -> v -> Maybe v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
symmetricDifferenceByFstWith op = symmetricDifferenceByWith (comparing fst) (wrap <.> onSecond op)

-- | A version of 'symmetricDifferenceByFstWith' that removes all elements that occur in both lists.

symmetricDifferenceByFst :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
symmetricDifferenceByFst = symmetricDifferenceByFstWith (\_ _ -> Nothing)

-- | Given a function @f@, two arguments @x :: a, y :: b@ and a list @l :: [c]@,
-- this function returns the list @f x y : l@.

consWith :: (a -> b -> c) -> a -> b -> [c] -> [c]
consWith = (<.>) (:) -- same as: @consWith op x y zs = op x y : zs@

-- | Returns the values at the even positions of a list.

evens :: [a] -> [a]
evens (x : _ : xs) = x : evens xs
evens l            = l

-- | Returns the values at the odd positions of a list.

odds :: [a] -> [a]
odds (_ : x : xs) = x : odds xs
odds _            = []

-- | Splits a list into the values at its even positions and the values at its odd positions.

evensOdds :: [a] -> ([a], [a])
evensOdds = evens &&& odds

-- | A ternary function that ignores its first two arguments and returns the third one.

idThird :: a -> b -> c -> c
idThird _ _ = id

-- | A type for keys in key-value pairs and association lists.

type Key   = Int

-- | An arc is a key-value pair.
-- The name originates from the graph-theoretic interpretation that an arc points to its
-- key (vertex) and is labelled with its value.

type Arc a = (Key, a)

-- | A row is simply an association list.

type Row a = [Arc a]

-- | A matrix is a row of rows.
-- This type has no additional constraints and may not be suited for efficient operations.

type Mat a = Row (Row a)

-- | This 'Arbitrary' generator creates an arbitrary 'Row',
-- such that its indices are in the range @[0 .. n]@ for some natural number /n/.

mkArbitrary :: Arbitrary a => Gen (Row a)
mkArbitrary =  sized $ \n -> do k <- choose (0, n)
                                is <- sequence [suchThat arbitrary (>= 0) | _ <- [0 .. k]]
                                es <- arbitrary
                                return (zip is es)