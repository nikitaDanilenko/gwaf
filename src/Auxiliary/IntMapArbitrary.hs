----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.IntMapArbitrary
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides an 'Arbitrary' instance for 'IntMap's.
-- The underlying strategy is exactly the same one as for 'AList's and 'SafeArray's
-- and uses the function 'mkArbitrary' from "Auxiliary.General".

module Auxiliary.IntMapArbitrary where

import Data.IntMap       ( IntMap )
import Test.QuickCheck   ( Arbitrary, arbitrary )

import Auxiliary.General ( mkArbitrary )
import Auxiliary.Mapping ( fromRow )

instance Arbitrary a => Arbitrary (IntMap a) where

  arbitrary = fmap fromRow mkArbitrary