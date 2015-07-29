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

  -- * Unlabelled paths

  Path,
  emptyPath,
  stepLeft,
  stepRight,
  verticesPath,
  pathFromList,
  lengthPath

  ) where

import Data.Either                  ( partitionEithers )
import qualified Data.Foldable as F ( foldr )
import Data.Foldable                ( Foldable, toList )
import Data.List                    ( intercalate )
import Data.Sequence                ( Seq, empty, singleton, (|>), (<|), fromList )
import qualified Data.Sequence as S ( length )

-- | The data type for labelled paths.
-- Labelled paths are always non-empty.

newtype LabelledPath k v = LabelledPath { getLabelledPath :: Seq (Either k v) }
  deriving Eq

-- | A (somewhat) pretty-printing instance, which puts the values along the edges in parentheses
-- and intercalates arrows between the vertices.

instance (Show k, Show v) => Show (LabelledPath k v) where

  show = intercalate " -> " . map (either show (\x -> concat ["(", show x, ")"])) . toEitherList

instance Functor (LabelledPath k) where

  fmap f = LabelledPath . fmap (fmap f) . getLabelledPath

-- | Folds the values along the labelled path.

instance Foldable (LabelledPath k) where

  foldr f e = foldr f e . labels

-- | A path that starts at the specified position.

initial :: k -> LabelledPath k v
initial = LabelledPath . singleton . Left

-- | Adds a step to the end of the path.

labelledStepRight :: LabelledPath k v -> v -> k -> LabelledPath k v
labelledStepRight path v k = LabelledPath (getLabelledPath path |> Right v |> Left k)

-- | Adds a step to the beginning of the path.

labelledStepLeft :: k -> v -> LabelledPath k v -> LabelledPath k v
labelledStepLeft k v path = LabelledPath (Left k <| Right v <| getLabelledPath path)

-- | Creates a labelled path from an initial vertex and a list of pairs,
-- where the first component is the edge label and the second one is the edge target.

labelledPathFrom :: k -> [(v, k)] -> LabelledPath k v
labelledPathFrom = foldl (uncurry . labelledStepRight) . initial

-- | Transforms a labelled path into a pair. 
-- The first component is the initial vertex
-- and the second component is a list of pairs,
-- where the first component is the edge label and the second one is the edge target.

labelledPathToPair :: LabelledPath k v -> (k, [(v, k)])
labelledPathToPair path = (s, uncurry (flip zip) (partitionEithers rest)) where
  Left s : rest = toEitherList path

-- | Returns the vertices in their order of traversal on a labelled path.

verticesLabelled :: LabelledPath k v -> [k]
verticesLabelled = fst . partitionEithers . toEitherList

-- | Returns the length of a labelled path, which is the number of vertices along that path.

lengthLabelled :: LabelledPath k v -> Int
lengthLabelled = length . verticesLabelled

-- | Returns the labels in their order of traversal on a labelled path.

labels :: LabelledPath k v -> [v]
labels = snd . partitionEithers . toEitherList

reverseLabelledPath :: LabelledPath k v -> LabelledPath k v
reverseLabelledPath = fromEitherList . reverse . toEitherList

-- | Transforms a labelled path into a list of 'Either' values.

toEitherList :: LabelledPath k v -> [Either k v]
toEitherList = toList . getLabelledPath

-- | Turns a list of 'Either' values into a path.
-- Since the list does not necessarily have the alternating property (starts and ends with 'Left'
-- and no equal constructors at subsequent positions),
-- this function is not exported.

fromEitherList :: [Either k v] -> LabelledPath k v
fromEitherList = LabelledPath . fromList

-- | The data type for unlabelled paths.

newtype Path k = Path { getPath :: Seq k }
  deriving Eq

instance Show k => Show (Path k) where

  show = intercalate " -> " . map show . verticesPath

-- | The empty path.

emptyPath :: Path k
emptyPath = Path empty

-- | Adds a step to the end of the path.

stepRight :: Path k -> k -> Path k
stepRight path k = Path (getPath path |> k)

-- | Adds a step to the beginning of the path.

stepLeft :: k -> Path k -> Path k
stepLeft k path = Path (k <| getPath path)

-- | Computes the length of a path.

lengthPath :: Path k -> Int
lengthPath = S.length . getPath

-- | Returns the vertices on a path in their order of traveral.

verticesPath :: Path k -> [k]
verticesPath = toList . getPath

-- | Creates a path from a list of vertices.

pathFromList :: [k] -> Path k
pathFromList = Path . fromList