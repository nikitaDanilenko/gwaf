{-# Language ScopedTypeVariables #-}

module Auxiliary.Helpers where

import Test.QuickCheck       (  Property )
import Test.Hspec            ( Spec, describe, context )
import Test.Hspec.QuickCheck ( prop )

-- | A labelled 'Property'.

type LabProperty = (String, Property)

-- | A list of labelled properties.

type LabProperties = [LabProperty]

-- | Creates a test group from labelled properties using 'prop' and sequencing the result.

mkTestGroup :: LabProperties -> Spec
mkTestGroup = mapM_ (uncurry prop) 

-- | Creates a test suite gradually descending the three description levels.

mkTestSuite :: [(String, [(String, LabProperties)])] -> Spec
mkTestSuite lats =
 sequence_ 
    [describe law (context st (mkTestGroup tests)) | (law, testss) <- lats, (st, tests) <- testss]

-- | Creates a test suite where every label (e.g. data structure) is associated with a labelled
-- list of tests (e.g. laws that need to hold).

mkSuite :: [(String, LabProperties)] -> Spec
mkSuite stsAndProps =
  sequence_ [describe law (mkTestGroup tests) | (law, tests) <- stsAndProps]

-- | A phantom type.

data Proxy a = Proxy deriving Show