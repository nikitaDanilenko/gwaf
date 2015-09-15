----------------------------------------------------------------------------------------------------
-- |
-- Module      :  Test.RandomMatrix
-- Copyright   :  (c) Nikita Danilenko 2015
-- License     :  BSD3
-- Maintainer  :  nikita.danilenko.is@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module Test.RandomMatrix (

	-- * Type synonyms

	Rows,
	Cols,
	Density,
	RandomGenerator,

	-- * Random matrix functions

	randomMatrixWith,
	randomMatrixWith',
	randomMatrix,
	randomMatrix',
	randomGraph,
	randomGraph',
	randomRelation,
	randomRelation',
	randomDiagonal,
	randomDiagonal',
	randomTriangle,
	randomTriangle',
	randomStrictTriangle,
	randomStrictTriangle',

	-- * Auxiliary functions

	breakAt,
	chopUniform,
	chopTriangle,
	chopStrictTriangle,
	mkRow

	) where

import Data.List             ( genericTake, genericDrop )
import Data.Maybe            ( mapMaybe )
import System.Random         ( Random, RandomGen, randomRs, split, StdGen, mkStdGen, random,
                               randomR, randoms )
import System.Random.Shuffle ( shuffle' )

import Auxiliary.General     ( Row, Mat, wrap )

-- | Given a function, an integer and a list, this function breaks the list in sublists.
-- The given function is used to determine the length of the next chunk.
-- For instance:
-- 
--   > breakAt id    3 "Explanation" = ["Exp","lan","ati","on"]
--   > breakAt (+ 1) 1 "Explanation" = ["E","xp","lan","atio","n"]

breakAt :: Integral i => (i -> i) -> i -> [a] -> [[a]]
breakAt f = go where

    go _ [] = []
    go n xs = genericTake n xs : go (f n) (genericDrop n xs)

-- | Given an integer /n/ and a list this function breaks the list into chunks of length /n/.
-- The last chunk is shorter, iff the length of the given list is not a multiple of /n/.

chopUniform :: Integral i => i -> [a] -> [[a]]
chopUniform = breakAt id

-- | This function chops a given list into sublists of increasing length beginning with 1. The last
--   element is shorter than the second to last, if the length of the list is not /n*(n+1)\/2/ for
--   some integer /n/.
--   
--   The name of the function hints at its use, since one can use the resulting chunks to fill a
--   lower triangle matrix.

chopTriangle :: [a] -> [[a]]
chopTriangle = breakAt (+ 1) 1

-- | This function behaves very similarly to 'chopTriangle', but its first list is empty.
-- The last element of this list is shorter than the second to last iff the list length 
-- is not /n*(n+1)\/2/ for some integer /n/.
--   
--   Again, the name hints at the function's application, namely the construction of a strict lower
--   triangle matrix.

chopStrictTriangle :: [a] -> [[a]]
chopStrictTriangle = breakAt (+1) 0

-- | This function transforms a list of 'Maybe' values into a 'Row' by first indexing the list and
--   then removing the 'Nothing' values. For example,
--   
--   > mkRow [Just 'h', Nothing, Just 'i'] = [(0, 'h'), (2, 'i')]

mkRow :: [Maybe a] -> Row a
mkRow = mapMaybe wrap . zip [0 .. ]

-- | The number of rows in a matrix.

type Rows            = Int

-- | The number of columns in a matrix.

type Cols            = Int

-- | The density of a matrix (ranging between 0,0 and 1.0)

type Density         = Double

-- | The number of  a random generator.
-- This is used in conjunction with 'mkStdGen'.

type RandomGenerator = Int

-- | Given the parameters the function creates a matrix with random entries.
--   The automatic creation of a random matrix allows zero values for the edges.
--   This can be useful, if non-existent edges are semantically different from
--   edges having weight zero. 
--   Removing zeroes breaks the uniform distribution of the matrices.
--   It is possible to maintain the uniform distribution by simply
--   specifying the bounds such that zero is not contained in the interval the
--   random values are drawn from.
--   Generation is realised via shuffling a fixed matrix, which is known to be
--   uniformly distributed.
--   
--   N.B.: The values in every kind of matrix are arbitrary values a-priori,
--   which is why zeroes from certain semirings are still possible.
--   Thus when working in an algebraic context,
--   one should filter the resulting matrix to remove zeroes.

randomMatrixWith :: (RandomGen g, Random a) =>
    g                           -- ^ random generator
 -> Rows                        -- ^ number of rows
 -> Cols                        -- ^ number of columns
 -> (Int -> Int -> Int)         -- ^ function that computes the number of entries
                                --   in the matrix, i.e. @(*)@
 -> ([Maybe a] -> Mat a)        -- ^ function that splits the overall list into
                                --   sublists which then become rows
 -> Density                     -- ^ density, i.e. percentage of edges, which is
                                --   a 'Double' value between /0/ and /1/
 -> (a, a)                      -- ^ Lower\/upper bounds for the random values
 -> Mat a
randomMatrixWith g rs cs size resize d lu = resize shuffled where
    shuffled = shuffle' toGo entries g2
    entries  = size rs cs
    fill     = floor (fromIntegral entries * d)
    toGo     =    map Just (take fill (randomRs lu g1)) -- \"interesting\" values
               ++ replicate (entries - fill) Nothing    -- \"zeroes\"
    (g1, g2) = split g

-- | Same as 'randomMatrixWith',
-- but the random generator is created from a given 'Int',
-- rather than already provided as an argument.

randomMatrixWith' :: Random a =>
    RandomGenerator             -- ^ random generator
 -> Rows                        -- ^ number of rows
 -> Cols                        -- ^ number of columns
 -> (Int -> Int -> Int)         -- ^ function that computes the number of entries
                                --   in the matrix, i.e. @(*)@
 -> ([Maybe a] -> Mat a)        -- ^ function that splits the overall list into
                                --   sublists which then become rows
 -> Density                     -- ^ density, i.e. percentage of edges, which is
                                --   a 'Double' value between /0/ and /1/
 -> (a, a)                      -- ^ Lower\/upper bounds for the random values
 -> Mat a
randomMatrixWith' = withStdGen randomMatrixWith

-- | This function creates a random matrix by computing the necessary number of entries,
--   then shuffling them and finally splitting them into uniform chunks which are then
--   used as rows.

randomMatrix :: (RandomGen g, Random a) =>
	   g        -- ^ random generator
  -> Rows     -- ^ number of rows
  -> Cols     -- ^ number of colums
  -> Density  -- ^ density (/0 <= d <= 1/)
  -> (a, a)   -- ^ lower\/upper bounds
  -> Mat a
randomMatrix gen rows cols = randomMatrixWith gen rows cols (*) (resizeWith (chopUniform cols))

-- | Same as 'randomMatrix',
-- but the random generator is created from a given 'Int',
-- rather than already provided as an argument.

randomMatrix' :: Random a =>
	   RandomGenerator   -- ^ random generator
  -> Rows              -- ^ number of rows
  -> Cols              -- ^ number of colums
  -> Density           -- ^ density (/0 <= d <= 1/)
  -> (a, a)            -- ^ lower\/upper bounds
  -> Mat a
randomMatrix' = withStdGen randomMatrix

-- | A random graph is a random matrix with the same number of rows and columns.

randomGraph :: (RandomGen g, Random a) => 
	   g        -- ^ random generator
  -> Rows     -- ^ number of vertices in the graph
  -> Density  -- ^ density (/0 <= d <= 1/)
  -> (a, a)   -- ^ lower\/upper bounds
  -> Mat a
randomGraph gen size = randomMatrix gen size size

-- | Same as 'randomGraph',
-- but the random generator is created from a given 'Int',
-- rather than already provided as an argument.

randomGraph' :: Random a =>
		 RandomGenerator   -- ^ random generator
  -> Rows              -- ^ number of vertices in the graph
  -> Density           -- ^ density (/0 <= d <= 1/)
  -> (a, a)            -- ^ lower\/upper bounds
  -> Mat a
randomGraph' = withStdGen randomGraph

-- | A random relation is a special case of a matrix where entries are either
--   existent or not. Existent entries are denoted by the value (), non-existent
--   entries are simply not contained in the corresponding list.

randomRelation :: RandomGen g => 
	   g        -- ^ random generator
	-> Rows     -- ^ cardinality of the source set
	-> Cols     -- ^ cardinality of the target set
	-> Density  -- ^ density (/0 <= d <= 1/)
	-> Mat ()
randomRelation gen rows cols dens = randomMatrix gen rows cols dens ((), ())

-- | Same as 'randomRelation',
-- but the random generator is created from a given 'Int',
-- rather than already provided as an argument.

randomRelation' ::
	   RandomGenerator   -- ^ random generator
	-> Rows              -- ^ cardinality of the source set
	-> Cols              -- ^ cardinality of the target set
	-> Density           -- ^ density (/0 <= d <= 1/)
	-> Mat ()
randomRelation' = withStdGen randomRelation

-- | Creates a random diagonal square matrix.
-- Please note that the density is computed with respect to the diagonal 
-- and /not/ the number of entries altogether.
-- That is: @randomDiagonal (mkStdGen 1234) 10 0.3 (0, 1)@ will create a square matrix
-- with exactly three (not thirty) entries.

randomDiagonal :: (RandomGen g, Random a) => 
	   g        -- ^ random generator
  -> Rows     -- ^ number of rows (and columns)
  -> Density  -- ^ percentage (between 0 and 1) of filled positions along the main diagonal
  -> (a, a)   -- ^ lower\/upper bounds
  -> Mat a
randomDiagonal gen size = randomMatrixWith gen size size const resize where

    resize = zipWith (\i mv -> (i, maybe [] (return . (,) i) mv)) [0..]

randomDiagonal' :: Random a => 
	   RandomGenerator   -- ^ random generator
  -> Rows              -- ^ number of rows (and columns)
  -> Density           -- ^ percentage (between 0 and 1) of filled positions along the main diagonal
  -> (a, a)            -- ^ lower\/upper bounds
  -> Mat a
randomDiagonal' = withStdGen randomDiagonal

-- | Creates a random triangle square matrix.
-- As with 'randomDiagonal' the density refers to the density of the triangle.
-- That is the number of entries in the matrix will be @'floor' (density * size * (size + 1) / 2)@.

randomTriangle :: (RandomGen g, Random a) =>
		 g        -- ^ random generator
  -> Rows     -- ^ number of rows (and columns)
  -> Density  -- ^ percentage (between 0 and 1) of filled positions in the lower triangle
  -> (a, a)   -- ^ lower\/upper bounds
  -> Mat a
randomTriangle gen size = randomMatrixWith gen size size f (resizeWith chopTriangle) where

    f n _ = n * (n + 1) `div` 2

randomTriangle' :: Random a =>
		 RandomGenerator   -- ^ random generator
  -> Rows              -- ^ number of rows (and columns)
  -> Density           -- ^ percentage (between 0 and 1) of filled positions in the lower triangle
  -> (a, a)            -- ^ lower\/upper bounds
  -> Mat a
randomTriangle' = withStdGen randomTriangle

-- | Creates a random strict triangle matrix (no entries at the diagonal). The
--   density refers to the density of the strict triangle, that is the number of
--   entries is @floor (density * size * (size - 1) / 2)@.

randomStrictTriangle :: (RandomGen g, Random a) =>
	   g        -- ^ random generator
	-> Rows     -- ^ number of rows (and columns)
	-> Density  -- ^ percentage (between 0 and 1) of filled positions in the strict lower triangle
	-> (a, a)   -- ^ lower\/upper bounds
	-> Mat a
randomStrictTriangle gen size = randomMatrixWith gen size size f (resizeWith chopStrictTriangle)

    where f n _ = n * (n - 1) `div` 2

randomStrictTriangle' :: Random a =>
		 RandomGenerator   -- ^ random generator
	-> Rows              -- ^ number of rows (and columns)
	-> Density           -- ^ percentage (between 0 and 1) of filled positions 
	                     --   in the strict lower triangle
	-> (a, a)            -- ^ lower\/upper bounds
	-> Mat a
randomStrictTriangle' = withStdGen randomStrictTriangle

-- | Apply a function that takes a standard random generator compose it with 'mkStdGen'.

withStdGen :: (StdGen -> a) -> Int -> a
withStdGen f = f . mkStdGen

-- | Creates a matrix from a (full) row generator function.

resizeWith :: (a -> [[Maybe b]]) -> a -> Mat b
resizeWith f = zip [0 .. ] . map mkRow . f

-- | This instance is trivial and deterministic.

instance Random () where

    random g   = ((), fst (split g))
    randomR _  = random
    randoms _  = repeat ()
    randomRs _ = randoms

instance (Random a, Random b) => Random (a, b) where

    randomR ((l1, l2), (u1, u2)) g = ((r1, r2), g2)
        where (r1, g1) = randomR (l1, u1) g
              (r2, g2) = randomR (l2, u2) g1

    random g = ((r1, r2), g2) where
        (r1, g1) = random g
        (r2, g2) = random g1

instance (Random a, Random b, Random c) => Random (a, b, c) where

    randomR ((l1, l2, l3), (u1, u2, u3)) g = ((r1, r2, r3), g3)
        where (r1, g1) = randomR (l1, u1) g
              (r2, g2) = randomR (l2, u2) g1
              (r3, g3) = randomR (l3, u3) g2

    random g = ((r1, r2, r3), g3) where
        (r1, g1) = random g
        (r2, g2) = random g1
        (r3, g3) = random g2