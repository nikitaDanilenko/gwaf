----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Auxiliary.Closure
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  GPL Version 3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module provides an implementation of the star closure and the Kleene closure for matrices.
-- It is based upon the results presented in
-- 
-- * Nikita Danilenko, "Functional Kleene Closures", LOPSTR 2014, LNCS 8981, pp. 241-285

module Algebraic.Closure (

  kleeneClosureWith,
  kleeneClosure,
  kleeneClosureC,

  starClosureI,
  starClosureO,
  starClosureIC,
  starClosureOC,

  addIdentity
  ) where

import Control.Applicative ( (<*>) )

import Algebraic.Matrix   ( Matrix, (!!!), rowMap, rowNumbers, identityMatrix )
import Algebraic.Semiring ( KleeneAlgebra, KleeneAlgebraC, Semiring, star, (.*.), (.+.) )
import Algebraic.Vector   ( (*>), (*>>), (.@.), (<+++>) )
import Auxiliary.General  ( Key )
import Auxiliary.Mapping  ( Mapping, MappingV )
import Auxiliary.SetOps   ( UnionableHom )

-- | This function contains the main strategy of the computation,
-- which is taking the step iterator and folding it over the row indices of the matrix.
-- However, both the step iterator and the row indices are supplied as parameters.
-- The step iterator is an abstraction of the function @newMat@ from the paper mentioned above.
-- The order of the row indices of the matrix does not matter,
-- however, there are orders that are better suited for particular matrices 
-- (cf. Gaussian elimination and determinants).

kleeneClosureWith :: (Key -> Matrix o i k -> Matrix o i k)  -- ^ the new iteration step
                  -> (Matrix o i k -> [Key])                -- ^ the keys of the matrix in
                                                            --   the desired order
                  -> Matrix o i k -> Matrix o i k
-- In the above paper the step iterator is computed separately as the function @tau@,
-- which turns out to be the right-fold of the step iterator.
-- Here, we inline this unnecessary definition.
kleeneClosureWith next keys = foldr next <*> keys -- same as @\m -> foldr next m (keys m)@

-- | This function is an abstraction of the \"next step computation\" defined in the above source.
-- The additional parameter denote an abstraction of scalar multiplication of vectors.
-- The motivation behind this abstraction is that one can provide at least two variants of
-- the scalar multiplication depending on whether or not the underlying Kleene algebra supports
-- constant recognition.
-- If it does, one can improve the scalar multiplication such that no zeroes are contained in
-- the result vector.
-- As for the addition, since Kleene algebras are idempotent semirings,
-- the non-zero values are closed under addition.
-- Thus adding vectors without zero entries does not introduce new zeroes.

newMatWith :: (Mapping o, MappingV i, UnionableHom i, KleeneAlgebra k) => 
  (k -> i k -> i k) -> Key -> Matrix o i k -> Matrix o i k
newMatWith scale i a = rowMap (\aj -> (((aj .@. i) .*. star (ai .@. i)) `scale` ai) <+++> aj) a
  where ai = a !!! i

-- | The Kleene closure function over Kleene algebras with distinguishable constants.
-- The result matrix does not contain any occurrences of 'zero'.

kleeneClosureC :: (Mapping o, MappingV i, UnionableHom i, KleeneAlgebraC kc) =>
  Matrix o i kc -> Matrix o i kc
kleeneClosureC = kleeneClosureWith (newMatWith (*>)) rowNumbers

-- | The Kleene closure function over arbitrary Kleene algebras.
-- The result matrix may contain 'zero' entries.

kleeneClosure :: (Mapping o, MappingV i, UnionableHom i, KleeneAlgebra k) => 
  Matrix o i k -> Matrix o i k
kleeneClosure = kleeneClosureWith (newMatWith (*>>)) rowNumbers

-- | Adds the identity matrix to the argument matrix.

addIdentity :: (Mapping o, UnionableHom o, Mapping i, UnionableHom i, Semiring s) =>
  Matrix o i s -> Matrix o i s
addIdentity a = identityMatrix a .+. a

-- | Computes the star closure of a matrix as @'star' a = 'plus' ('one' '.+.' a)@,
-- where the function 'kleeneClosure' is used for 'plus'.

starClosureI :: (Mapping o, UnionableHom o, MappingV i, UnionableHom i, KleeneAlgebra k) =>
  Matrix o i k -> Matrix o i k
starClosureI = kleeneClosure . addIdentity

-- | Computes the star closure of a matrix as @'star' a = 'plus' ('one' '.+.' a)@,
-- where the function 'kleeneClosureC' is used for 'plus'.

starClosureIC :: (Mapping o, UnionableHom o, MappingV i, UnionableHom i, KleeneAlgebraC kc) =>
  Matrix o i kc -> Matrix o i kc
starClosureIC = kleeneClosureC . addIdentity

-- | Computes the star closure of a matrix as @'star' a = 'one' '.+.' 'plus' a)@,
-- where the function 'kleeneClosure' is used for 'plus'.

starClosureO :: (Mapping o, UnionableHom o, MappingV i, UnionableHom i, KleeneAlgebra k) =>
  Matrix o i k -> Matrix o i k
starClosureO = addIdentity . kleeneClosure

-- | Computes the star closure of a matrix as @'star' a = 'one' '.+.' 'plus' a)@,
-- where the function 'kleeneClosureC' is used for 'plus'.

starClosureOC :: (Mapping o, UnionableHom o, MappingV i, UnionableHom i, KleeneAlgebraC kc) =>
  Matrix o i kc -> Matrix o i kc
starClosureOC = addIdentity . kleeneClosureC