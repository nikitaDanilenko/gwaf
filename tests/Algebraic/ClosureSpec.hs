----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Algebraic.ClosureSpec
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides tests for the properties that should hold for the star closure operation.

{-# Language ScopedTypeVariables #-}

module Algebraic.ClosureSpec where

import Data.IntMap               ( IntMap )
import Test.Hspec                ( Spec )
import Test.QuickCheck           ( Arbitrary, property )

import Algebraic.Closure         ( starClosureIC, starClosureOC )
import Algebraic.Matrix          ( Matrix, identityMatrix, (.**.), HasVMM, removeZeroesMatrix )
import Algebraic.Semiring        ( KleeneAlgebraC, Tropical, Number, (.+.), (.<=.) )
import Auxiliary.AList           ( AList )
import Auxiliary.IntMapArbitrary ()
import Auxiliary.Mapping         ( Mapping, MappingV )
import Auxiliary.SafeArray       ( SafeArray )
import Auxiliary.SetOps          ( UnionableHom )
import Helpers                   ( mkSuite, LabProperties, (===>), Proxy ( Proxy ) )

-- | A polymorphic test that checks whether
-- @'one' '.+.' 'plus' a == 'plus' ('one' '.+.' a)@ holds for matrices.

starPlusIOC :: 
	(Mapping o, UnionableHom o, MappingV i, UnionableHom i, KleeneAlgebraC kac, Eq kac) =>
  	Matrix o i kac -> Bool
starPlusIOC m = starClosureIC m' == starClosureOC m'
	where m' = removeZeroesMatrix m -- avoid redundant zeroes

-- | A polymorphic test that checks whether the star closure is a fixpoint of
-- @\x -> one '.+.' x '.*.' a.

isLeftFixpoint :: 
	(Mapping o, UnionableHom o, MappingV i, UnionableHom i, HasVMM i o, KleeneAlgebraC kac, Eq kac) =>
		Matrix o i kac -> Bool
isLeftFixpoint m = identityMatrix m' .+. starM .**. m' == starM where
	starM = starClosureOC m'
	m'    = removeZeroesMatrix m -- avoid redundant zeroes

-- | A polymorphic test that checks whether the star closure is a fixpoint of 
-- @\x -> one '.+.' a '.*.' x.

isRightFixpoint :: 
	(Mapping o, UnionableHom o, MappingV i, UnionableHom i, HasVMM i o, KleeneAlgebraC kac, Eq kac) =>
		Matrix o i kac -> Bool
isRightFixpoint m = identityMatrix m' .+. m' .**. starM == starM where
	starM = starClosureOC m'
	m'    = removeZeroesMatrix m -- avoid redundant zeroes

-- | Checks whether @a '.*.' b '.<=-' b@ implies @'starClosureOC' a '.**.' b '.<=.' b

isLeftClosed ::
	(HasVMM i o, UnionableHom i, UnionableHom o, Mapping o, KleeneAlgebraC kac, Eq kac) => 
		Matrix o i kac -> Matrix o i kac -> Bool
isLeftClosed a b = (a .**. b .<=. b) ===> (starClosureOC a .**. b .<=. b)

-- | Checks whether @b '.*.' a '.<=-' b@ implies @b '.**.' 'starClosureOC' a '.<=.' b

isRightClosed :: 
	(HasVMM i o, UnionableHom i, UnionableHom o, Mapping o, KleeneAlgebraC kac, Eq kac) => 
		Matrix o i kac -> Matrix o i kac -> Bool
isRightClosed a b = (b .**. a .<=. b) ===> (b .**. starClosureOC a .<=. b)

-- | List of tested laws.

propsClosure :: forall o i kac . 
	(HasVMM i o, UnionableHom i, UnionableHom o, Mapping o, 
	 KleeneAlgebraC kac, Eq kac, Arbitrary kac, Show kac) => 
		Proxy (Matrix o i kac) -> LabProperties
propsClosure _ = zip laws [
	property (starPlusIOC     :: Matrix o i kac -> Bool),
	property (isLeftFixpoint  :: Matrix o i kac -> Bool),
	property (isRightFixpoint :: Matrix o i kac -> Bool),
	property (isLeftClosed    :: Matrix o i kac -> Matrix o i kac -> Bool),
	property (isRightClosed   :: Matrix o i kac -> Matrix o i kac -> Bool)
	]

-- | List of descriptions for the tested laws.

laws :: [String]
laws = [
	"plus (1 + a) = 1 + (plus a)",
	"1 + star a * a = star a",
	"1 + a * star a = star a",
	"a * b <= b ===> star a * b <= b",
	"b * a <= b ===> b * star a <= b"
	]

-- | A list of structure combinations the laws are tested for.

combinations :: [(String, String)]
combinations = [("AList", "AList"), 
                ("IntMap", "AList"), ("IntMap", "IntMap"),
                ("SafeArray", "AList"), ("SafeArray", "IntMap")]

-- | A list of types the laws are tested for.

types :: [String]
types = ["Bool", "Tropical (Number Integer)", "Bool x Tropical (Number Rational)"]

spec :: Spec
spec = mkSuite $
    zip [unwords ["Matrix", o, i, t] | (o, i) <- combinations, t <- types, i /= "SafeArray"]
        [
          propsClosure (Proxy :: Proxy (Matrix AList AList Bool)),
          propsClosure (Proxy :: Proxy (Matrix AList AList (Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix AList AList (Bool, Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix IntMap AList Bool)),
          propsClosure (Proxy :: Proxy (Matrix IntMap AList (Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix IntMap AList (Bool, Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix IntMap IntMap Bool)),
          propsClosure (Proxy :: Proxy (Matrix IntMap IntMap (Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix IntMap IntMap (Bool, Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix SafeArray AList Bool)),
          propsClosure (Proxy :: Proxy (Matrix SafeArray AList (Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix SafeArray AList (Bool, Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix SafeArray IntMap Bool)),
          propsClosure (Proxy :: Proxy (Matrix SafeArray IntMap (Tropical (Number Integer)))),
          propsClosure (Proxy :: Proxy (Matrix SafeArray IntMap (Bool, Tropical (Number Integer))))
        ]