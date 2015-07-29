----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.Matrix
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable

-- This module contains an implementation of matrices as association lists of association lists.
-- The main idea behind the presented abstraction is that there are likely to be two different kinds
-- of association lists involved.
-- The outer association list type is intended to be queried directly,
-- while the inner type is largely used in set-like operations like unions.

{-# Language MultiParamTypeClasses #-}

module Algebraic.Matrix (
  
  -- * Matrix data type

  Matrix,
  matrix,
  fromRows,
  fromMappings,
  toMat,

  -- * Auxiliary functions

  isEmptyMatrix,
  filterMatrix,
  removeZeroesMatrix,
  emptyMatrix,
  identityMatrix,
  
  -- * Functor operations

  rowMapWithKey,
  rowMap,

  -- * Query and modification

  (!!!),
  mAtZ,
  mAt,
  rowNumbers,
  rowDimension,
  addValue,
  addValueWith,
  
  -- * Vector-matrix multiplication

  vecMatMult,
  liftVecMatMult,
  smultWithKeys,
  smultWithKey,
  HasHetVMM ( .. ),
  HasVMM,
  (.*),
  (.**),
  (.*+-),
  (.*-+),
  (.*||),
  
  -- * Algebraic matrix operations

  (**>),
  (**>>),
  (.**.),
  (.***.),
  (.++.),
  (.+++.),
  (.--.),
  inverseAMat,
  transposeSquare,
  transposeNonSquare

  ) where

import Data.Function                ( on )
import Data.Foldable                ( Foldable, toList )
import qualified Data.Foldable as F ( all )
import Data.IntMap                  ( IntMap )
import Data.List                    ( intercalate )
import Test.QuickCheck              ( Arbitrary, arbitrary, shrink, sized )

import Algebraic.Semiring           ( MonoidA, (.+.), FindZero, isNotZero, zero, GroupA, inverseA,
                                      MonoidM, (.*.), Semiring, FindOne, SemigroupA,
                                      SemigroupM )
import Algebraic.Vector             ( (*>), removeZeroes, unitVector )
import Auxiliary.AList              ( AList, asList )
import Auxiliary.General            ( Key, Arc, Row, Mat, (<.>), scaleLeft )
import Auxiliary.KeyedClasses       ( Lookup, maybeAt, KeyFunctor, fmapWithKey, KeyMaybeFunctor,
                                      ffilter, restrictKeys )
import Auxiliary.Mapping            ( Mapping, MappingV, fromRow, toRow, keys, isEmpty, empty,
                                      insert, insertWith, toMappingWith )
import Auxiliary.SafeArray          ( SafeArray )
import Auxiliary.SetOps             ( Intersectable, intersectionWith, intersectionWithKey,
                                      Unionable, unionWith, Complementable, differenceWith,
                                      differenceWith2, UnionableHom, SetOps, bigunionWith,
                                      (\\/) )
import Auxiliary.SetOpsInstances    ()

-- | Matrix data type.
-- Both @o@ and @i@ are type constructors and serve as mnemonics for the __o__uter, respectively
-- __i__nner association list types.
-- 
-- The indended application is the following:
-- 
-- * Both @o@ and @i@ are instances of the 'Mapping' type class.
-- * The matrix rows are indexed with @[0 .. n-1]@ for some natural number /n/.
--   For every @i <- [0 .. n-1]@ the @i@-th entry in the outer mapping denotes the @i@-th row
--   of the matrix.
--   
-- This behaviour is guaranteed by the functions provided in this module.
-- It is /not/ guaranteed in general, because it is possible to unwrap a matrix
-- and manipulate the outer layer such that it violates one of the conditions.
-- 
-- 
-- If the outer matrix layer is an instance of 'Lookup' and
-- the inner one is an instance of 'MappingV',
-- the matrix rows can be accessed using the function @('!!!')@.
-- The query function @('!!!')@ yields the empty row for every row index that is not filled.
-- 
-- Note that the matrix size is not encoded anywhere (in particular not in the matrix size).
-- This is convenient in the sense that one can easily create and manipulate matrices.
-- For example, multiplication of matrices is /type-safe/ for arbitrary matrix sizes,
-- because missing positions are simply ignored.
-- However, this comes with the downside that it is not possible to define constants for
-- monoid instances, because these constants would have to know their size.

newtype Matrix o i a = Matrix { matrix :: o (i a) }

-- | Two matrices are equal, if and only if their respective canonic representations
-- as rows of rows are equal.

instance (Mapping o, Mapping i, Eq a) => Eq (Matrix o i a) where

  (==) = (==) `on` toMat

instance (Functor o, Functor i) => Functor (Matrix o i) where

  fmap f = Matrix . fmap (fmap f) . matrix

-- | Pretty-prints a matrix.
-- Each row is represented as @i : (k1 | v1) ... (kr | vr)@,
-- where @(ki, vi)@ are the key-value pairs in the underlying association list.

instance (Mapping o, Mapping i, Show a) => Show (Matrix o i a) where

  show = intercalate "\n" . map f . toMat where

    f (j, r)        = unwords [show j, ":", unwords (map showElem r)]

    showElem (i, a) = concat ["(", unwords [show i, "|", show a], ")"]

-- | Transforms a matrix into its canonic representation as a row of rows.

toMat :: (Mapping o, Mapping i) => Matrix o i a -> Mat a
toMat = map (fmap toRow) . toRow . matrix

-- | Transforms a row of rows into a matrix.
-- Note that this removes duplicate entries,
-- so that @'fromRows' . 'map' 'snd' . toMat' = 'id'@ holds,
-- but @'toMat' . 'fromMat' . 'map' 'snd' = 'id'@ is not true in general.

fromRows :: (Mapping o, Mapping i, Foldable f, Functor f) => f (Row a) -> Matrix o i a
fromRows = fromMappings . fmap fromRow

-- | Transforms a list of indexed 'Mapping's into a matrix.

fromMappings :: (Mapping o, Foldable f) => f (i a) -> Matrix o i a
fromMappings = Matrix . fromRow . zip [0 ..] . toList

-- | Auxiliary function that applies a supplied combinator to the collections inside the
-- matrix parameters and wraps the result in a 'Matrix wrapper again.

combineWith :: (o (i a) -> p (j b) -> q (k c)) 
            -> Matrix o i a -> Matrix p j b -> Matrix q k c
combineWith f (Matrix ma) (Matrix mb) = Matrix (f ma mb)

-- | Checks whether a given matrix is empty. 
-- To determine that we check, whether all inner mappings are empty.
-- Note that this is conceptually different from checking for an empty outer layer.

isEmptyMatrix :: (Foldable o, Mapping i) => Matrix o i a -> Bool
isEmptyMatrix = F.all isEmpty . matrix

-- | Maps a row transformation function that depends on keys over the matrix.

rowMapWithKey :: KeyFunctor o => 
  (Int -> i a -> j b) -> Matrix o i a -> Matrix o j b
rowMapWithKey f = Matrix . fmapWithKey f . matrix

-- | Maps a row transformation function over the matrix.

rowMap :: Functor o => (i a -> j b) -> Matrix o i a -> Matrix o j b
rowMap f = Matrix . fmap f . matrix

-- | Filters the values in a matrix with respect to a given predicate.

filterMatrix :: (Functor o, KeyMaybeFunctor i) => 
  (a -> Bool) -> Matrix o i a -> Matrix o i a
filterMatrix p = Matrix . fmap (ffilter p) . matrix

-- | Filters all occurrences of 'zero' in case of a structure with a notion of a zero element.

removeZeroesMatrix :: (Functor o, KeyMaybeFunctor i, FindZero ma) => 
  Matrix o i ma -> Matrix o i ma
removeZeroesMatrix = filterMatrix isNotZero

-- | Returns the specified row of a matrix.
-- The result is 'empty', if the given row is not explicitly filled in the matrix.

(!!!) :: (Lookup o, MappingV i) => Matrix o i a -> Key -> i a
(!!!) = maybeAt empty . matrix

-- | Returns the value at a given position.
-- If said position is not filled, the supplied element is returned.
-- The first position argument denotes the row and the second one denotes the column.

mAtZ :: (Lookup o, MappingV i) => a -> Matrix o i a -> Key -> Key -> a
mAtZ z m i = maybeAt z (m !!! i)

mAt :: (Lookup o, MappingV i, MonoidA a) => Matrix o i a -> Key -> Key -> a
mAt = mAtZ zero

-- | Returns the list of the row numbers in the matrix.

rowNumbers :: Mapping o => Matrix o i a -> [Key]
rowNumbers = keys . matrix

-- | Returns the number of rows of a matrix.

rowDimension :: Mapping o => Matrix o i a -> Int
rowDimension = length . rowNumbers

-- | Inserts a value at a given position in the matrix.
-- If there is already a value at that position, the supplied function is used to combine
-- these two values.
-- If the position is out of the row bounds,
-- the original matrix is returned.

addValueWith :: (KeyFunctor o, MappingV i) => 
  (a -> a -> a) -> Key -> Key -> a -> Matrix o i a -> Matrix o i a
addValueWith op i j x = Matrix . fmapWithKey f . matrix where

    f k | i == k    = insertWith op j x
        | otherwise = id

-- | Adds a value at a given position and overwrites possibly existing values.
-- If the position is out of the row bounds,
-- the original matrix is returned.

addValue :: (KeyFunctor o, MappingV i) => Key -> Key -> a -> Matrix o i a -> Matrix o i a
addValue = addValueWith const

-- | Returns the empty matrix of the same size as the argument matrix.

emptyMatrix :: (Functor o, MappingV i) => Matrix o i' a -> Matrix o i b
emptyMatrix = rowMap (const empty)

-- | Returns the identity matrix of the same size as the argument matrix.

identityMatrix :: (KeyFunctor o, Mapping i, MonoidM mm) => Matrix o i a -> Matrix o i mm
identityMatrix = rowMapWithKey (const . unitVector)

-- | An abstraction of the vector-matrix multiplication pattern.
-- The two functions denote abstractions of a vector sum and a scalar multiplication
-- respectively.
-- The scalar multiplication is used in an intersection operation and the sum function is
-- applied to the result of the intersection operation.

vecMatMult :: Intersectable t q => (t c -> d)                    -- ^ sum
                                -> (Key -> a -> i b -> c)        -- ^ scalar multiplication
                                -> t a -> Matrix q i b -> d
vecMatMult fSum fMult vec = fSum . intersectionWithKey fMult vec . matrix

-- | A fully parametric abstraction of vector-matrix multiplication.
-- The type class summarises all necessary constraints and provides a default definition of
-- a multiplication generator that depends on value operations only.
-- The supplied definition can be overwritten for efficiency.
-- The function 'mkVMMWith' should satisfy
-- 
-- [/vector-matrix multiplication/]
--  @'mkVMMWith' p t = 'vecMatMult' ('bigunionWith' p 'empty') ('smultWithKey' t)@

class (Foldable vec1, Intersectable vec1 q, Mapping vec2, Unionable vec2 vec3, MappingV vec3)
        => HasHetVMM vec1 q vec2 vec3 where

  mkVMMWith :: (c -> c -> c)              -- ^ addition
            -> (Key -> a -> b -> c)       -- ^ scalar multiplication
            -> vec1 a  
            -> Matrix q vec2 b
            -> vec3 c
  mkVMMWith p t = vecMatMult (bigunionWith p empty) (smultWithKey t)

instance HasHetVMM AList  AList     AList  AList
instance HasHetVMM AList  IntMap    AList  AList
instance HasHetVMM IntMap IntMap    IntMap IntMap
instance HasHetVMM AList  SafeArray AList  AList
instance HasHetVMM AList  SafeArray AList  IntMap
instance HasHetVMM AList  SafeArray IntMap IntMap
instance HasHetVMM IntMap SafeArray AList  AList
instance HasHetVMM IntMap SafeArray AList  IntMap
instance HasHetVMM IntMap SafeArray IntMap IntMap

-- | In many cases the result of vector-matrix multiplications should have the same type as
-- the input vector.
-- In these cases one usually also has the property that the same container type is used for
-- the inner layers of the matrix.
-- We abbreviate this variation of the 'HasHetVMM' type class in a new homogeneous type class.

class HasHetVMM vec q vec vec => HasVMM vec q

instance HasVMM AList  AList
instance HasVMM AList  IntMap
instance HasVMM AList  SafeArray
instance HasVMM IntMap IntMap
instance HasVMM IntMap SafeArray

-- | The canonic implementation of the usual vector-matrix multiplication over semirings.
-- The result vector may contain zeroes.

(.*) :: (HasHetVMM vec1 q vec2 vec3, Semiring s) => vec1 s -> Matrix q vec2 s -> vec3 s
(.*) = mkVMMWith (.+.) (const (.*.))

-- | An improved implementation of the vector-matrix multiplication over semirings.
-- The result vector does not contain any zero entries.
-- Also, the scalar multiplication is optimised to check for constants first.

(.**) :: (HasHetVMM vec1 q vec2 vec3, MappingV vec2, Semiring s, FindZero s, FindOne s) => 
  vec1 s -> Matrix q vec2 s -> vec3 s
(.**) = removeZeroes <.> vecMatMult (bigunionWith (.+.) empty) (const (*>))

-- | Vector-matrix multiplication that ignores the values in the argument vector.

(.*-+) :: HasVMM vec q => vec a -> Matrix q vec b -> vec b
(.*-+) = mkVMMWith const (\_ _ e -> e)

-- | Vector-matrix multiplication that ignores the values in the matrix.

(.*+-) :: HasVMM vec q => vec a -> Matrix q vec b -> vec a
(.*+-) = mkVMMWith const (\_ l _ -> l)

-- | Vector-matrix multiplication that collects all outgoing edges and their
-- respective labels.

(.*||) :: HasVMM vec q => vec [Arc a] -> Matrix q vec a -> vec [Arc a]
(.*||) = mkVMMWith (++) (\i es e -> (i, e) : es)

-- Lift a vector-matrix multiplication to the matrix level by successively applying it to
-- all the rows.
-- Note that while the suffix suggests a vector-matrix multiplication,
-- the second argument of the supplied function is arbitrary.

liftVecMatMult :: Functor o => 
  (i a -> b -> i' c) -> Matrix o i a -> b -> Matrix o i' c
liftVecMatMult mult a b = rowMap (`mult` b) a

-- | Scalar multiplication of matrices.
-- The multiplication @('*>>')@ is applied to every row.
-- The resulting matrix may contain 'zero' entries.

(**>>) :: (Functor o, Functor i, MonoidM mm) => mm -> Matrix o i mm -> Matrix o i mm
(**>>) = scaleLeft (.*.)

-- | Scalar multiplication of matrices, which checks the scalar for being a special constant.
-- This function uses @('*>')@ and the resulting matrix does not contain zero entries.

(**>) :: (Functor o, MappingV i, MonoidM mm, FindZero mm, FindOne mm) => 
  mm -> Matrix o i mm -> Matrix o i mm
(**>) = rowMap . (*>)

-- | A fully parametric matrix multiplication.
-- In the particular case of a homogeneous vector-matrix multiplication setting
-- (i.e. 'HasVMM'),
-- this function can be used to provide a multiplicative semigroup instance for matrices.

(.***.) :: (Functor o, HasHetVMM vec1 q vec2 vec3, Semiring s) =>
  Matrix o vec1 s -> Matrix q vec2 s -> Matrix o vec3 s
(.***.) = liftVecMatMult (.*)

-- | Similar to @('.***.')@, but also removes zeroes and uses an optimised scalar multiplication.

(.**.) :: (Functor o, HasHetVMM vec1 q vec2 vec3, MappingV vec2, Semiring s, FindZero s, FindOne s)
  =>  Matrix o vec1 s -> Matrix q vec2 s -> Matrix o vec3 s
(.**.) = liftVecMatMult (.**)

-- | A fully parametric matrix addition.
-- In the particular case of a homogeneous setting
-- this function can be used to provide an additive semigroup instance for matrices.
-- However, this function is more general than the special case.

(.+++.) :: (Unionable t i, Unionable vec vec, SemigroupA asg) =>
  Matrix t vec asg -> Matrix i vec asg -> Matrix i vec asg
(.+++.) = unionWith (.+.)

-- | This is just @('.+++.')@ followed by a removal of all zeroes.

(.++.) :: (Functor i, Unionable t i, KeyMaybeFunctor vec, 
            Unionable vec vec, SemigroupA asg, FindZero asg) =>
  Matrix t vec asg -> Matrix i vec asg -> Matrix i vec asg
(.++.) = removeZeroesMatrix <.> (.+++.)

-- | Computes the additive inverse of a matrix.
-- This is essentially the same definition as for structures over additive groups,
-- however, since the set of all matrices (of arbitrary sizes!) does not have a neutral
-- element with respect to addition,
-- we provide this function manually.

inverseAMat :: (Functor o, KeyMaybeFunctor i, GroupA ag, FindZero ag) => 
  Matrix o i ag -> Matrix o i ag
inverseAMat = fmap inverseA

-- | Computes the difference of two matrices.
-- Just as with 'inverseAMat' this is the same definition as in the case of additive groups,
-- without a group instance for matrices.

(.--.) :: (Functor i, KeyMaybeFunctor vec, Unionable t i, 
           Unionable vec vec, GroupA asg, FindZero asg)
  => Matrix t vec asg -> Matrix i vec asg -> Matrix i vec asg
a .--. b = a .++. inverseAMat b

-- | Creates a scalar multiplication from a combination function.
-- The combination function takes the \"outer\" key of the value the mapping is scaled with
-- and also the \"inner\" keys of the individual filled positions.

smultWithKeys :: Mapping m => (Key -> a -> Key -> b -> c) -> Key -> a -> m b -> m c
smultWithKeys op = fmapWithKey <.> op

-- | A variant of 'smultWithKeys' that ignores the inner keys.

smultWithKey :: Mapping m => (Key -> a -> b -> c) -> Key -> a -> m b -> m c
smultWithKey op = smultWithKeys (const <.> op)

-- | This function contains the main pattern for the computation of the transposition,
-- while being parametric in the two controlling arguments,
-- which decide what kind of transposition will be the result.

preTranspose :: 
  (HasVMM vec q, Unionable vec o, Mapping i, Functor o) => 
  vec [Arc a] -> o [Arc a] -> Matrix q vec a -> Matrix o i a
preTranspose vs cols m = Matrix (fmap fromRow (vs .*|| m \\/ cols))

-- | Auxiliary function that creates a contiguous mapping with a constant value.

mkMapping :: Mapping m => Int -> a -> m a
mkMapping n x = toMappingWith x [0 .. n - 1]

-- | Transposition of a square matrix.
-- The requirement @'Unionable' vec q@ suggests that @q@ should be a
-- 'MappingV' instance as well.

transposeSquare :: (Unionable vec q, HasVMM vec q, Mapping q) => 
  Matrix q vec a -> Matrix q vec a
transposeSquare mat = preTranspose (mkMapping n []) (mkMapping n []) mat where
  n  = rowDimension mat

-- | Transposes a non-square matrix.
-- The additional 'Int' parameter denotes the number of columns in the matrix.
-- This number cannot be computed from the matrix alone,
-- because missing positions can mean both zero values and non-existent ones.

transposeNonSquare :: (Unionable vec q, HasVMM vec q, Mapping q) =>
  Int -> Matrix q vec a -> Matrix q vec a
transposeNonSquare cols mat = 
  preTranspose (mkMapping (rowDimension mat) []) (mkMapping cols []) mat

-- | The intersection of matrices is an intersection on the
-- outer level followed by an intersection on each inner level.
-- Since matrices are intended to have contiguous key areas,
-- this operation is similar to a 'zipWith' on the outer layer.
-- The class constraints reflect the need for two different kinds of intersection
-- and can be considered canonic.

instance (Intersectable tOuter qOuter, Intersectable tInner qInner) 
  => Intersectable (Matrix tOuter tInner) (Matrix qOuter qInner) where

  intersectionWithKey = combineWith . intersectionWith . intersectionWithKey

-- | The union of matrices is a union on the outer level
-- followed by a union on each inner level.
-- However, since unions are homogeneous in the contained types,
-- the inner unions are homogeneous as well.

instance (Unionable tOuter iOuter, Unionable tInner tInner) 
  => Unionable (Matrix tOuter tInner) (Matrix iOuter tInner) where

  unionWith = combineWith . unionWith . unionWith

-- | The relative difference of two matrices is the relative difference of each individual row.
-- The outer operation is rather a union (or intersection) than a complement.
-- This behaviour is achieved by using a special difference on the outer level
-- that yields 'Just' results in every possible application of a proper difference operation
-- to rows.

instance (Complementable tOuter quOuter, Complementable tInner quInner)
         => Complementable (Matrix tOuter tInner) (Matrix quOuter quInner) where

  differenceWith  op = combineWith (differenceWith (Just <.> differenceWith op))
  differenceWith2 op = combineWith (differenceWith2 (Just <.> differenceWith2 op))

-- | Matrices with traversable outer levels and traversable inner levels that support
-- homogeneous set operations also support set operations.

instance (SetOps tOuter quOuter, SetOps tquInner tquInner) 
  => SetOps (Matrix tOuter tquInner) (Matrix quOuter tquInner)

-- | The additive semigroup instance for matrices over additive semigroups.
-- The addition is the component-wise addition.

instance (UnionableHom o, Mapping o, UnionableHom i, Mapping i, SemigroupA asg) 
  => SemigroupA (Matrix o i asg) where
    
  (.+.) = (.+++.)

-- | The multiplicative semigroup instance for matrices over semigroups.
-- The multiplication is the usual matrix multiplication.

instance (Mapping o, HasVMM i o, Semiring s) => SemigroupM (Matrix o i s) where
  
  (.*.) = (.***.)

-- | This instance generates only square matrices.
-- Since matrices are used to represent graphs,
-- it is a sensible choice.
-- The 'shrink' function yields all submatrices of smaller sizes.

instance (Mapping o, Mapping i, Arbitrary a) => Arbitrary (Matrix o i a) where

  arbitrary = sized fun where
    fun n = fmap fromRows (mapM (fmap (asList . restrictKeys n) . const arbitrary) [0 .. n - 1])

  shrink m = map (\k -> Matrix (fmap (restrictKeys k) (restrictKeys k rs))) (rowNumbers m)
    where rs = matrix m
    