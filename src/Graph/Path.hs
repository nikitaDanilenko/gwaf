----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.Closure
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides the data type for paths, both labelled and unlabelled.

module Graph.Path (

  -- * Labelled paths

  LabelledPath,
  initial,
  labelledStepLeft,
  labelledStepRight,
  labelledPathFrom,
  labelledPathToPair,
  verticesLabelled,
  labels,
  lengthLabelled,
  toEitherList,
  reverseLabelledPath,

  -- * Unlabelled paths

  Path,
  emptyPath,
  stepLeft,
  stepRight,
  verticesPath,
  pathFromList,
  lengthPath,
  reversePath

  ) where

import Control.Applicative          ( liftA2 )
import Data.Either                  ( partitionEithers )
import qualified Data.Foldable as F ( foldr )
import Data.Foldable                ( Foldable, toList )
import Data.List                    ( intercalate, inits )
import Data.Sequence                ( Seq, empty, singleton, (|>), (<|), fromList )
import qualified Data.Sequence as S ( length )
import Test.QuickCheck              ( Arbitrary, arbitrary, shrink )

-- | The data type for labelled paths.
-- Labelled paths are always non-empty.

newtype LabelledPath vertex label = LabelledPath { getLabelledPath :: Seq (Either vertex label) }
  deriving Eq

-- | A (somewhat) pretty-printing instance, which puts the values along the edges in parentheses
-- and intercalates arrows between the vertices.

instance (Show vertex, Show label) => Show (LabelledPath vertex label) where

  show = intercalate " -> " . map (either show (\x -> concat ["(", show x, ")"])) . toEitherList

instance Functor (LabelledPath vertex) where

  fmap f = LabelledPath . fmap (fmap f) . getLabelledPath

-- | Folds the values along the labelled path.

instance Foldable (LabelledPath vertex) where

  foldr f e = foldr f e . labels

instance (Arbitrary vertex, Arbitrary label) => Arbitrary (LabelledPath vertex label) where

  arbitrary = liftA2 labelledPathFrom arbitrary arbitrary

-- | A path that starts at the specified position.

initial :: vertex -> LabelledPath vertex label
initial = LabelledPath . singleton . Left

-- | Adds a step to the end of the path.

labelledStepRight :: LabelledPath vertex label -> label -> vertex -> LabelledPath vertex label
labelledStepRight path l v = LabelledPath (getLabelledPath path |> Right l |> Left v)

-- | Adds a step to the beginning of the path.

labelledStepLeft :: vertex -> label -> LabelledPath vertex label -> LabelledPath vertex label
labelledStepLeft v l path = LabelledPath (Left v <| Right l <| getLabelledPath path)

-- | Creates a labelled path from an initial vertex and a list of pairs,
-- where the first component is the edge label and the second one is the edge target.

labelledPathFrom :: vertex -> [(label, vertex)] -> LabelledPath vertex label
labelledPathFrom = foldl (uncurry . labelledStepRight) . initial

-- | Transforms a labelled path into a pair. 
-- The first component is the initial vertex
-- and the second component is a list of pairs,
-- where the first component is the edge label and the second one is the edge target.

labelledPathToPair :: LabelledPath vertex label -> (vertex, [(label, vertex)])
labelledPathToPair path = (s, uncurry (flip zip) (partitionEithers rest)) where
  Left s : rest = toEitherList path

-- | Returns the vertices in their order of traversal on a labelled path.

verticesLabelled :: LabelledPath vertex label -> [vertex]
verticesLabelled = fst . partitionEithers . toEitherList

-- | Returns the length of a labelled path, which is the number of vertices along that path.

lengthLabelled :: LabelledPath vertex label -> Int
lengthLabelled = length . verticesLabelled

-- | Returns the labels in their order of traversal on a labelled path.

labels :: LabelledPath vertex label -> [label]
labels = snd . partitionEithers . toEitherList

-- | Reverses a labelled path.

reverseLabelledPath :: LabelledPath vertex label -> LabelledPath vertex label
reverseLabelledPath = fromEitherList . reverse . toEitherList

-- | Transforms a labelled path into a list of 'Either' values.

toEitherList :: LabelledPath vertex label -> [Either vertex label]
toEitherList = toList . getLabelledPath

-- | Turns a list of 'Either' values into a path.
-- Since the list does not necessarily have the alternating property (starts and ends with 'Left'
-- and no equal constructors at subsequent positions),
-- this function is not exported.

fromEitherList :: [Either vertex label] -> LabelledPath vertex label
fromEitherList = LabelledPath . fromList

-- | The data type for unlabelled paths.

newtype Path vertex = Path { getPath :: Seq vertex }
  deriving Eq

instance Show vertex => Show (Path vertex) where

  show = intercalate " -> " . map show . verticesPath

instance Arbitrary vertex => Arbitrary (Path vertex) where

  arbitrary = fmap pathFromList arbitrary
  shrink    = map pathFromList . inits . verticesPath

-- | The empty path.

emptyPath :: Path vertex
emptyPath = Path empty

-- | Adds a step to the end of the path.

stepRight :: Path vertex -> vertex -> Path vertex
stepRight path v = Path (getPath path |> v)

-- | Adds a step to the beginning of the path.

stepLeft :: vertex -> Path vertex -> Path vertex
stepLeft v path = Path (v <| getPath path)

-- | Computes the length of a path.

lengthPath :: Path vertex -> Int
lengthPath = S.length . getPath

-- | Returns the vertices on a path in their order of traveral.

verticesPath :: Path vertex -> [vertex]
verticesPath = toList . getPath

-- | Creates a path from a list of vertices.

pathFromList :: [vertex] -> Path vertex
pathFromList = Path . fromList

-- | Reverses a path.

reversePath :: Path vertex -> Path vertex
reversePath = pathFromList . reverse . verticesPath