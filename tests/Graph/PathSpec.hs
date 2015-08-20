----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.PathSpec
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides tests for the properties that should hold for paths.

{-# Language ScopedTypeVariables #-}

module Graph.PathSpec where

import Control.Arrow     ( (***) )
import Test.Hspec        ( Spec )
import Test.QuickCheck   ( Arbitrary, property )

import Auxiliary.General ( evensOdds )
import Graph.Path        ( Path, LabelledPath, lengthLabelled, labelledPathToPair,
                           labelledPathFrom, labels, verticesLabelled, toEitherList,
                           verticesPath, pathFromList, stepRight, emptyPath, stepLeft, reversePath )
import Helpers           ( LabProperties, Proxy ( Proxy ), mkSuite )

-- | Tests whether labelled paths are non-empty.

prop_nonEmpty :: LabelledPath vertex label -> Bool
prop_nonEmpty p = lengthLabelled p > 0

-- | Tests whether 'labelledPathToPair' is a left-inverse for 'labelledPathFrom' (up to currying).

prop_toFromLabelled :: (Eq vertex, Eq label) => vertex -> [(label, vertex)] -> Bool
prop_toFromLabelled s es = labelledPathToPair (labelledPathFrom s es) == (s, es)

-- | Tests whether 'labelledPathToPair' is a right-inverse for 'labelledPathFrom' (up to currying).

prop_fromToLabelled :: (Eq vertex, Eq label) => LabelledPath vertex label -> Bool
prop_fromToLabelled path = uncurry labelledPathFrom (labelledPathToPair path) == path

-- | Tests the superficial intertwining property.

prop_lengthVerticesLabels :: LabelledPath vertex label -> Bool
prop_lengthVerticesLabels path = lengthLabelled path == 1 + length (labels path)

-- | Tests whether the 'Left' and 'Right' values in the underlying representation are
-- at the right positions.

prop_intertwined :: (Eq vertex, Eq label) => LabelledPath vertex label -> Bool
prop_intertwined path = 
  (verticesLabelled path, labels path) == (map unLeft *** map unRight) (evensOdds (toEitherList path))
  where unLeft  (Left x)  = x
        unRight (Right y) = y

-- | The properties that are tested for labelled paths.

propsLabelledPath :: 
  forall vertex label . 
    (Eq vertex, Eq label, Arbitrary vertex, Arbitrary label, Show vertex, Show label) => 
  Proxy (vertex, label) -> LabProperties
propsLabelledPath _ = zip testsLabelledPath
  [ 
      property (prop_nonEmpty             :: LabelledPath vertex label -> Bool)
    , property (prop_toFromLabelled       :: vertex -> [(label, vertex)] -> Bool)
    , property (prop_fromToLabelled       :: LabelledPath vertex label -> Bool)
    , property (prop_lengthVerticesLabels :: LabelledPath vertex label -> Bool)
    , property (prop_intertwined          :: LabelledPath vertex label -> Bool)
  ]

testsLabelledPath :: [String]
testsLabelledPath = [
    "labelled paths are non-empty"
  , "labelledPathToPair . uncurry labelledPathFrom = id"
  , "uncurry labelledPathFrom . labelledPathToPair = id"
  , "lengthLabelled = (1 +) . length . labels"
  , "labelled paths are alternating sequences of Left and Right"
  ]

-- | Tests whether 'verticesPath' is a left-inverse for 'pathFromList'.

prop_toFrom :: Eq vertex => [vertex] -> Bool
prop_toFrom vs = verticesPath (pathFromList vs) == vs

-- | Tests whether 'verticesPath' is a right-inverse for 'pathFromList'.

prop_fromTo :: Eq vertex => Path vertex -> Bool
prop_fromTo path = pathFromList (verticesPath path) == path

-- | Tests whether 'fromList' is a folded 'stepRight.

prop_fromListStepLeft :: Eq vertex => [vertex] -> Bool
prop_fromListStepLeft vs = pathFromList vs == foldr stepLeft emptyPath vs

-- | Tests whether 'fromList' is the same as
-- @'reversePath' . 'foldr' ('flip' 'stepRight') 'emptyPath'@.

prop_fromListReverseStepRight :: Eq vertex => [vertex] -> Bool
prop_fromListReverseStepRight vs = 
  pathFromList vs == reversePath (foldr (flip stepRight) emptyPath vs)

-- | The properties that are tested for paths.

propsPath :: forall vertex . (Eq vertex, Arbitrary vertex, Show vertex) => Proxy vertex -> LabProperties
propsPath _ = zip testsPath
  [ 
      property (prop_toFrom                   :: [vertex] -> Bool)
    , property (prop_fromTo                   :: Path vertex -> Bool)
    , property (prop_fromListStepLeft         :: [vertex] -> Bool)
    , property (prop_fromListReverseStepRight :: [vertex] -> Bool)
  ]

testsPath :: [String]
testsPath = [
    "verticesPath . pathFromList = id"
  , "pathFromList . verticesPath = id"
  , "pathFromList = foldr stepLeft emptyPath"
  , "pathFromList = reverse . foldr (flip stepRight) emptyPath"
  ]

typesLabelled :: [String]
typesLabelled = ["Int Bool", "Int Double", "Integer String", "(Integer, Char) [Bool]"]

typesPath :: [String]
typesPath = ["Int", "Double", "Integer", "String", "[Bool]"]

spec :: Spec
spec = mkSuite $ 
  zip (map ("LabelledPath " ++) typesLabelled)
      [
          propsLabelledPath (Proxy :: Proxy (Int, Bool))
        , propsLabelledPath (Proxy :: Proxy (Int, Double))
        , propsLabelledPath (Proxy :: Proxy (Integer, String))
        , propsLabelledPath (Proxy :: Proxy ((Integer, Char), [Bool]))
      ]
  ++
  zip (map ("Path " ++) typesPath)
      [
          propsPath (Proxy :: Proxy Int)
        , propsPath (Proxy :: Proxy Double)
        , propsPath (Proxy :: Proxy Integer)
        , propsPath (Proxy :: Proxy String)
        , propsPath (Proxy :: Proxy [Bool])
      ]