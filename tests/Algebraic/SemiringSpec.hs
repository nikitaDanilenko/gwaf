----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.MappingSpec
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Tests of the properties of algebraic structures.

{-# Language ScopedTypeVariables #-}

module Algebraic.SemiringSpec ( spec ) where

import Data.List           ( genericReplicate )

import Data.Monoid         ( First )
import Test.Hspec          ( Spec )
import Test.QuickCheck     ( Arbitrary, property )

import Algebraic.Semiring  ( SemigroupA, (.+.), MonoidA, zero, msum, mtimes,
                             FindZero, isZero, isNotZero, GroupA, (.-.), inverseA,
	                           SemigroupM, (.*.), MonoidM, one, mproduct, (.^.),
                             FindOne, isOne, isNotOne,
                             Semiring, IdempotentSemiring, (.<=.), KleeneAlgebra, star, plus,
                             Tropical, Number )
import Helpers             ( (===), (===>), mkSuite, Proxy ( Proxy ), LabProperties )

-- | Is the given operation associative?

isAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative op a b c = a `op` (b `op` c) == (a `op` b) `op` c

-- | Is the second argument neutral with respect to the first one?

isNeutral :: Eq a => (a -> a -> a) -> a -> a -> Bool
isNeutral op e a = a `op` e == a && e `op` a == a

-- | Is the first argument a folded version of the second and third one?

isFolded :: Eq a => ([a] -> a) -> (a -> a -> a) -> a -> [a] -> Bool
isFolded folded op e = folded === foldr op e

-- | Is the first argument expressible in terms of the second one?

isNtimes :: (Eq a, Integral i) => (i -> a -> a) -> ([a] -> a) -> i -> a -> Bool
isNtimes times folded n x = times n x == folded (genericReplicate n x)

-- | Are the given predicates complementary?

isAndNot :: (a -> Bool) -> (a -> Bool) -> a -> Bool
isAndNot p notP = p === not . notP

-- | Is the predicate the equality with the given element?

isEqualConstant :: Eq a => (a -> Bool) -> a -> a -> Bool
isEqualConstant p constant = p === (constant ==)

-- | Is the inverse expressible in terms of a binary function and the neutral element?

isInverseBinary :: Eq a => (a -> a) -> (a -> a -> a) -> a -> a -> Bool
isInverseBinary inv bin neutral = inv === bin neutral

-- | Does the first operation distribute over the second one?

isDistributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
isDistributive mult add a b c = a `mult` (b `add` c) == (a `mult` b) `add` (a `mult` c)

-- | Tests associativity in additive monoids.

prop_addAssociative :: (Eq asg, SemigroupA asg) => asg -> asg -> asg -> Bool
prop_addAssociative = isAssociative (.+.)

-- | Tests neutrality of 'zero' in additive monoids.

prop_zeroNeutral :: (Eq asg, MonoidA asg) => asg -> Bool
prop_zeroNeutral = isNeutral (.+.) zero

-- | Tests the sum rule in additive monoids.

prop_sumFolded :: (Eq asg, MonoidA asg) => [asg] -> Bool
prop_sumFolded = isFolded msum (.+.) zero

-- | Tests the multiplication with integers in additive monoids.

prop_times :: (Eq asg, MonoidA asg) => Integer -> asg -> Bool
prop_times = isNtimes mtimes msum

-- | Checks 'not' '.' 'isZero' = 'isNotZero' in additive monoids with constants.

prop_isZeroIsNotZero :: (Eq asg, MonoidA asg, FindZero asg) => asg -> Bool
prop_isZeroIsNotZero = isAndNot isZero isNotZero

-- | Checks whether 'isZero' is the same as 'zero ==' in additive monoids with constants.

prop_isZeroConstant :: (Eq asg, MonoidA asg, FindZero asg) => asg -> Bool
prop_isZeroConstant = isEqualConstant isZero zero

-- | Checks whether 'inverseA' @a@ = 'zero' '.-.' @a@ holds in additive groups.

prop_isNegateMinus :: (GroupA ag, Eq ag) => ag -> Bool
prop_isNegateMinus = isInverseBinary inverseA (.-.) zero

-- | Tests associativity in multiplicative monoids.

prop_multAssociative :: (Eq msg, SemigroupM msg) => msg -> msg -> msg -> Bool
prop_multAssociative = isAssociative (.*.)

-- | Tests neutrality of 'one' in multiplicative monoids.

prop_oneNeutral :: (Eq msg, MonoidM msg) => msg -> Bool
prop_oneNeutral = isNeutral (.*.) one

-- | Tests the product rule in multiplicative monoids.

prop_productFolded :: (Eq msg, MonoidM msg) => [msg] -> Bool
prop_productFolded = isFolded mproduct (.*.) one

-- | Tests the power rule in multiplicative monoids.

prop_power :: (Eq msg, MonoidM msg) => msg -> Integer -> Bool
prop_power = flip (isNtimes (flip (.^.)) mproduct)

-- | Checks 'not' '.' 'isOne' = 'isNotOne' in multiplicative monoids with constants.

prop_isOneIsNotOne :: (Eq msg, MonoidM msg, FindOne msg) => msg -> Bool
prop_isOneIsNotOne = isAndNot isOne isNotOne

-- | Checks whether 'isOne' is the same as 'one ==' in multiplicative monoids with constants.

prop_isOneConstant :: (Eq msg, MonoidM msg, FindOne msg) => msg -> Bool
prop_isOneConstant = isEqualConstant isOne one

-- | Checks the left distributive law in semirings.

prop_isLeftDistributive :: (Semiring s, Eq s) => s -> s -> s -> Bool
prop_isLeftDistributive = isDistributive (.*.) (.+.)

-- | Tests the right distributive law in semirings.

prop_isRightDistributive :: (Semiring s, Eq s) => s -> s -> s -> Bool
prop_isRightDistributive = isDistributive (flip (.*.)) (.+.)

-- | Tests whether 'zero' is annihilating in semirings.

prop_isZeroAnnihilating :: (Semiring s, Eq s) => s -> Bool
prop_isZeroAnnihilating a = zero .*. a == zero && a .*. zero == zero

-- | Tests whether addition is commutative in semirings.

prop_isAdditionCommutative :: (Semiring s, Eq s) => s -> s -> Bool
prop_isAdditionCommutative a b = a .+. b == b .+. a

-- | Tests whether addition is idempotent in semirings.

prop_isAdditionIdempotent :: (IdempotentSemiring s, Eq s) => s -> Bool
prop_isAdditionIdempotent a = a .+. a == a

-- | Tests whether @'star' a = 'one' '.+.' 'plus' a@ holds in Kleene algebras.

prop_starAsPlus :: (KleeneAlgebra ka, Eq ka) => ka -> Bool
prop_starAsPlus a = star a == one .+. plus a

-- | Tests whether @'plus' a = a '.*.' 'star' a@ holds in Kleene algebras.

prop_plusAsStar :: (KleeneAlgebra ka, Eq ka) => ka -> Bool
prop_plusAsStar a = plus a == a .*. star a

-- | Tests the left pre-fixpoint property in Kleene algebras.

prop_leftPreFixpoint :: (KleeneAlgebra ka, Eq ka) => ka -> Bool
prop_leftPreFixpoint a = one .+. star a .*. a .<=. star a

-- | Tests the right pre-fixpoint property in Kleene algebras.

prop_rightPreFixpoint :: (KleeneAlgebra ka, Eq ka) => ka -> Bool
prop_rightPreFixpoint a = one .+. a .*. star a .<=. star a

-- | Tests the left inclusion property in Kleene algebras.

prop_leftInclusion :: (KleeneAlgebra ka, Eq ka) => ka -> ka -> Bool
prop_leftInclusion a b = a .*. b .<=. b ===> star a .*. b .<=. b

-- | Tests the right inclusion property in Kleene algebras.

prop_rightInclusion :: (KleeneAlgebra ka, Eq ka) => ka -> ka -> Bool
prop_rightInclusion a b = b .*. a .<=. b ===> b .*. star a .<=. b

-- | Laws for additive monoids.

propsMonoidA :: forall am . (MonoidA am, Eq am, Arbitrary am, Show am) => 
	Proxy am -> LabProperties 
propsMonoidA _ = zip 
	[ 
	  "Addition associative", 
	  "Zero neutral",
	  "Sum = Folded +",
	  "Times ~ Sum"] 
	[
		property (prop_addAssociative :: am -> am -> am -> Bool),
		property (prop_zeroNeutral    :: am -> Bool),
		property (prop_sumFolded      :: [am] -> Bool),
		property (prop_times          :: Integer -> am -> Bool)
	]

-- | Laws for additive groups.

propsGroupA :: forall ag . (GroupA ag, Eq ag, Arbitrary ag, Show ag) =>
	Proxy ag -> LabProperties
propsGroupA _ = zip
	["InverseA = (zero -)"]
	[property (prop_isNegateMinus :: ag -> Bool)]

-- | Laws for additive monoids with constants.

propsMonoidAC :: forall amc . (MonoidA amc, FindZero amc, Eq amc, Arbitrary amc, Show amc) =>
	Proxy amc -> LabProperties
propsMonoidAC _ = zip 
	[ 
	  "not . isZero = isNotZero",
	  "isZero = (zero ==)"
	] 
	[
		property (prop_isZeroIsNotZero :: amc -> Bool),
		property (prop_isZeroConstant  :: amc -> Bool)
	]

-- | Laws for multiplicative monoids.

propsMonoidM :: forall mm . (MonoidM mm, Eq mm, Arbitrary mm, Show mm) => 
	Proxy mm -> LabProperties 
propsMonoidM _ = zip 
	[ 
		"Multiplication associative", 
	  "One neutral", 
	  "Product = Folded *", 
	  "Power ~ Product"
	]
	[
		property (prop_multAssociative :: mm -> mm -> mm -> Bool),
		property (prop_oneNeutral      :: mm -> Bool),
		property (prop_productFolded   :: [mm] -> Bool),
		property (prop_power           :: mm -> Integer -> Bool)
	]

-- | Laws for multiplicative monoids with constants.

propsMonoidMC :: forall mmc . (MonoidM mmc, FindOne mmc, Eq mmc, Arbitrary mmc, Show mmc) =>
	Proxy mmc -> LabProperties
propsMonoidMC _ = zip 
	[
	 	"not . isOne = isNotOne", 
	 	"isOne = (one ==)"]
	[
		property (prop_isOneIsNotOne :: mmc -> Bool),
		property (prop_isOneConstant :: mmc -> Bool)
	]

-- | Laws for semirings, the monoid laws are not tested.

propsSemiring :: forall s . (Semiring s, Eq s, Arbitrary s, Show s) =>
	Proxy s -> LabProperties
propsSemiring _ = zip 
	[
		"multiplication left-distributive", 
   	"multiplication right-distributive",
   	"zero annihilating",
   	"addition commutative"]
	[ 
		property (prop_isLeftDistributive    :: s -> s -> s -> Bool),
	  property (prop_isRightDistributive   :: s -> s -> s -> Bool),
	  property (prop_isZeroAnnihilating    :: s -> Bool),
	  property (prop_isAdditionCommutative :: s -> s -> Bool)
	]

-- | Laws for idempotent semirings. The semiring laws are not tested.

propsIdempotentSemiring :: forall is . (IdempotentSemiring is, Eq is, Arbitrary is, Show is) =>
	Proxy is -> LabProperties
propsIdempotentSemiring _ = zip
	["addition is idempotend"]
	[property (prop_isAdditionIdempotent :: is -> Bool)]

-- | Laws for Kleene algebras. The laws for idempotent semirings are not tested.

propsKleeneAlgebra :: forall ka . (KleeneAlgebra ka, Eq ka, Arbitrary ka, Show ka) =>
	Proxy ka -> LabProperties
propsKleeneAlgebra _ = zip
	[
		"left pre-fixpoint",
		"right pre-fixpoint",
		"left inclusion",
		"right inclusion",
		"star as plus",
		"plus as star"
	]
	[
		property (prop_leftPreFixpoint  :: ka -> Bool),
	 	property (prop_rightPreFixpoint :: ka -> Bool),
	 	property (prop_leftInclusion    :: ka -> ka -> Bool),
	 	property (prop_rightInclusion   :: ka -> ka -> Bool),
	 	property (prop_starAsPlus       :: ka -> Bool),
	 	property (prop_plusAsStar       :: ka -> Bool)
	]

-- | Types for additive monoids.

amTypes :: [String]
amTypes = ["Bool", "Number Integer", "Number Rational", "Tropical (Number Integer)", 
           "Tropical (Number Rational)", "(Bool, Tropical (Number Integer))", 
           "First (Number Integer)"]

-- | Types for additive monoids with constants.

amcTypes :: [String]
amcTypes = amTypes

-- | Types for additive groups.

agTypes :: [String]
agTypes = ["Number Integer, (Number Integer, Number Rational)"]

-- | Types for multiplicative monoids.

mmTypes :: [String]
mmTypes = ["Bool", "Number Integer", "Number Rational", "Tropical (Number Integer)", 
         "Tropical (Number Rational)", "(Bool, Tropical (Number Integer))", 
         "First (Number Integer)", "[Bool]"]

-- | Types for multiplicative monoids with constants.

mmcTypes :: [String]
mmcTypes = mmTypes

-- | Types for semirings.

srTypes :: [String]
srTypes = amTypes

-- | Types for idempotent semirings.

isrTypes :: [String]
isrTypes = ["Bool", "Tropical (Number Integer)", "(Bool, Tropical (Number Integer))", "First"]

-- | Types for Kleene algebras.

kaTypes :: [String]
kaTypes = ["Bool", "Tropical (Number Integer)", "(Bool, Tropical (Number Integer))"]

spec :: Spec
spec = mkSuite $ concat [
	zip (map ("Additive monoid: " ++) amTypes)
			[
				propsMonoidA (Proxy :: Proxy Bool),
			 	propsMonoidA (Proxy :: Proxy (Number Integer)),
			 	propsMonoidA (Proxy :: Proxy (Number Rational)),
			 	propsMonoidA (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsMonoidA (Proxy :: Proxy (Tropical (Number Rational))),
			 	propsMonoidA (Proxy :: Proxy (Bool, Tropical (Number Integer))),
			 	propsMonoidA (Proxy :: Proxy (First (Number Integer)))
			]
  ,
  zip (map ("Additive monoid with constants: " ++) amcTypes)
      [
      	propsMonoidAC (Proxy :: Proxy Bool),
				propsMonoidAC (Proxy :: Proxy (Number Integer)),
			 	propsMonoidAC (Proxy :: Proxy (Number Rational)),
			 	propsMonoidAC (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsMonoidAC (Proxy :: Proxy (Tropical (Number Rational))),
			 	propsMonoidAC (Proxy :: Proxy (Bool, Tropical (Number Integer))),
			 	propsMonoidAC (Proxy :: Proxy (First (Number Integer)))
			]
  ,
  zip (map ("Additive group: " ++) agTypes)
  		[
  			propsGroupA (Proxy :: Proxy (Number Integer)),
  			propsGroupA (Proxy :: Proxy (Number Integer, Number Rational))
  		]
  ,
  zip (map ("Multiplicative monoid: " ++) mmTypes)
			[
			 	propsMonoidM (Proxy :: Proxy Bool),
			 	propsMonoidM (Proxy :: Proxy (Number Integer)),
			 	propsMonoidM (Proxy :: Proxy (Number Rational)),
			 	propsMonoidM (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsMonoidM (Proxy :: Proxy (Tropical (Number Rational))),
			 	propsMonoidM (Proxy :: Proxy (Bool, Tropical (Number Integer))),
			 	propsMonoidM (Proxy :: Proxy (First (Number Integer))),
			  propsMonoidM (Proxy :: Proxy [Bool])
			]
	,
	zip (map ("Multiplicative monoid with constants: " ++) mmcTypes)
      [
      	propsMonoidMC (Proxy :: Proxy Bool),
			 	propsMonoidMC (Proxy :: Proxy (Number Integer)),
			 	propsMonoidMC (Proxy :: Proxy (Number Rational)),
			 	propsMonoidMC (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsMonoidMC (Proxy :: Proxy (Tropical (Number Rational))),
			 	propsMonoidMC (Proxy :: Proxy (Bool, Tropical (Number Integer))),
			 	propsMonoidMC (Proxy :: Proxy (First (Number Integer))),
			  propsMonoidMC (Proxy :: Proxy [Bool])
			]
  ,
  zip (map ("Semiring: " ++) srTypes)
  		[
  			propsSemiring (Proxy :: Proxy Bool),
			 	propsSemiring (Proxy :: Proxy (Number Integer)),
			 	propsSemiring (Proxy :: Proxy (Number Rational)),
			 	propsSemiring (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsSemiring (Proxy :: Proxy (Tropical (Number Rational))),
			 	propsSemiring (Proxy :: Proxy (Bool, Tropical (Number Integer)))
			]
	,
	zip (map ("Idempotent semiring: " ++) isrTypes)
	    [
	    	propsIdempotentSemiring (Proxy :: Proxy Bool),
			 	propsIdempotentSemiring (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsIdempotentSemiring (Proxy :: Proxy (Tropical (Number Rational)))
			]
	,
	zip (map ("KleeneAlgebra: " ++) kaTypes)
			[
	    	propsKleeneAlgebra (Proxy :: Proxy Bool),
			 	propsKleeneAlgebra (Proxy :: Proxy (Tropical (Number Integer))),
			 	propsKleeneAlgebra (Proxy :: Proxy (Bool, Tropical (Number Integer)))
			]
	]
