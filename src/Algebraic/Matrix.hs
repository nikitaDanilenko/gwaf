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
  fromMat,
  toMat,
  
  -- * Functor operations

  rowMapWithKey,
  rowMap,

  -- * Query

  (!!!),
  mAt,
  rowNumbers,
  rowDimension,
  
  -- * Vector-matrix multiplication

  vecMatMult,
  liftVecMatMult,
  

  ) where

import Data.Function                ( on )
import Data.Foldable                ( Foldable )
import qualified Data.Foldable as F ( all )
import Data.List                    ( intercalate )

import Algebraic.Semiring           ( MonoidA, (.+.), zero )
import Auxiliary.General            ( Key, Mat, (<.>) )
import Auxiliary.KeyedClasses       ( Lookup, maybeAt, KeyFunctor, fmapWithKey )
import Auxiliary.Mapping            ( Mapping, MappingV, fromRow, toRow, keys, isEmpty, empty )
import Auxiliary.SetOps             ( Intersectable, intersectionWith, intersectionWithKey,
                                      Unionable, unionWith, Complementable, differenceWith,
                                      differenceWith2, UnionableHom, SetOps )

import Auxiliary.AList -- DEBUGGING!

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
-- If the outer matrix layer is an instance of 'Lookup' and
-- the inner one is an instance of 'MappingV',
-- the matrix rows can be accessed using the function @('!!!')@.
-- The query function @('!!!')@ yields the empty row for every row index that is not filled.

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
-- so that @'fromMat' . toMat' = 'id'@ holds,
-- but @'toMat' . 'fromMat' = 'id'@ is not true in general.

fromMat :: (Mapping o, Mapping i) => Mat a -> Matrix o i a
fromMat = Matrix . fromRow . map (fmap fromRow)

-- | Auxiliary function that applies a supplied combinator to the collections inside the
-- matrix parameters and wraps the result in a 'Matrix' wrapper again.

combineWith :: (o (i a) -> p (j b) -> q (k c)) -> Matrix o i a -> Matrix p j b -> Matrix q k c
combineWith f (Matrix ma) (Matrix mb) = Matrix (f ma mb)

-- | Checks whether a given matrix is empty. 
-- To determine that we check, whether all inner mappings are empty.
-- Note that this is conceptually different from checking for an empty outer layer.

isEmptyM :: (Foldable o, Mapping i) => Matrix o i a -> Bool
isEmptyM = F.all isEmpty . matrix

-- | Maps a row transformation function that depends on keys over the matrix.

rowMapWithKey :: KeyFunctor o => (Int -> i a -> j b) -> Matrix o i a -> Matrix o j b
rowMapWithKey f = Matrix . fmapWithKey f . matrix

-- | Maps a row transformation function over the matrix.

rowMap :: Functor o => (i a -> j b) -> Matrix o i a -> Matrix o j b
rowMap f = Matrix . fmap f . matrix

-- | Returns the specified row of a matrix.
-- The result is 'empty', if the given row is not explicitly filled in the matrix.

(!!!) :: (Lookup o, MappingV i) => Matrix o i a -> Key -> i a
(!!!) = maybeAt empty . matrix

-- | Returns the value at a given position.
-- If said position is not filled, the supplied element is returned.
-- The first position argument denotes the row and the second one denotes the column.

mAt :: (Lookup o, MappingV i) => a -> Matrix o i a -> Key -> Key -> a
mAt z m i = maybeAt z (m !!! i)

-- | Returns the list of the row numbers in the matrix.

rowNumbers :: Mapping o => Matrix o i a -> [Key]
rowNumbers = keys . matrix

-- | Returns the number of rows of a matrix.

rowDimension :: Mapping o => Matrix o i a -> Int
rowDimension = length . rowNumbers

emptyMatrix :: (Functor o, MappingV i) => Matrix o i a -> Matrix o i b
emptyMatrix = Matrix . fmap (const empty) . matrix

-- | An abstraction of the vector-matrix multiplication pattern.
-- The two functions denote abstractions of a vector sum and a scalar multiplication
-- respectively.
-- The scalar multiplication is used in an intersection operation and the sum function is
-- applied to the result of the intersection operation.

vecMatMult :: Intersectable t q => (t c -> d)                    -- ^ sum
                                -> (Key -> a -> i b -> c)        -- ^ scalar multiplication
                                -> t a -> Matrix q i b -> d
vecMatMult fSum fMult vec = fSum . intersectionWithKey fMult vec . matrix

-- Lift a vector-matrix multiplication to the matrix level by successively applying it to
-- all the rows.
-- Note that while the suffix suggests a vector-matrix multiplication,
-- the second argument of the supplied function is arbitrary.

liftVecMatMult :: Functor o => (i a -> b -> i' c) -> Matrix o i a -> b -> Matrix o i' c
liftVecMatMult (.*) a b = rowMap (.* b) a

-- | Creates a scalar multiplication from a combination function.
-- The combination function takes the \"outer\" key of the value the mapping is scaled with
-- and also the \"inner\" keys of the individual filled positions.

smultWithKeys :: Mapping m => (Key -> a -> Key -> b -> c) -> Key -> a -> m b -> m c
smultWithKeys op = fmapWithKey <.> op

-- | A variant of 'smultWithKeys' that ignores the inner keys.

smultWithKey :: Mapping m => (Key -> a -> b -> c) -> Key -> a -> m b -> m c
smultWithKey op = smultWithKeys (const <.> op)

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

instance (Unionable tOuter iOuter, UnionableHom tInner) 
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

instance (SetOps tOuter quOuter, SetOpsHom tquInner) 
  => SetOps (Matrix tOuter tquInner) (Matrix quOuter tquInner)

-- | An additive monoid instance for matrices over additive monoids.
-- The 'zero' element satisfies the required monoid laws,
-- but is a matrix with no rows,
-- rather than a matrix with the \"right\" number of rows.

instance (UnionableHom o, Mapping o, UnionableHom i, MonoidA ma) => MonoidA (Matrix o i ma) where
    
    (.+.) = unionWith (.+.)
    zero  = Matrix (fromRow [])