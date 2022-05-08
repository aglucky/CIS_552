{-
---
fulltitle: "In class exercise: Random Generation"
---
-}
{-# LANGUAGE ScopedTypeVariables #-}

module RandomGen where

-- make sure you have filled in all of the 'undefined' values in the State module.
-- If you have not, update the State import below to Control.Monad.State
import Control.Monad
import qualified State as S
import System.Random (StdGen)
import qualified System.Random as Random (mkStdGen, randomIO, uniform, uniformR)

{-
Random Generation
-----------------

Recall that QuickCheck needs to randomly generate values of any type. It turns
out that we can use the state monad to define something like the `Gen` monad
used in the QuickCheck libary.

First, a brief discussion of pseudo-random number generators. [Pseudo-random
number generators](http://en.wikipedia.org/wiki/Pseudorandom_number_generator)
aren't really random, they just look like it. They are more like functions
that are so complicated that they might as well be random. The nice property
about them is that they are repeatable, if you give them the same *seed* they
will produce the same sequence of "random" numbers.

Haskell has a library for Pseudo-Random numbers called
[`System.Random`](http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html).
It features the following elements:

~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type StdGen  -- A type for a "standard" random number generator.

-- | Construct a generator from a given seed. Distinct arguments
-- are likely to produce distinct generators.
mkStdGen :: Int -> StdGen

-- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
uniform  :: StdGen -> (Int, StdGen)
~~~~~~~~~~~~~~~~~~~~~~~~~~

Side note: the default constructor `mkStdGen` is a bit weak so we wrap it to
perturb the seed a little first:
-}

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

{-
For example, we can generate a random integer by constructing a random
number generator, calling `uniform` and then projecting the result.
-}

testRandom :: Int -> Int
testRandom i = fst (Random.uniform (mkStdGen i))

{-
Our random integers depend on the seed that we provide.
-}

-- >>> testRandom 1

-- >>> testRandom 2

-- >>> testRandom 3

{-
If we'd like to constrain that integer to a specific inclusive range `(0, n)`
we can use `uniformR`.
-}

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound = Random.uniformR (0, bound)

{-
These tests should all produce random integers between 0 and 20.
-}

testBounded :: Int -> Int
testBounded = fst . nextBounded 20 . mkStdGen

-- >>> testBounded 1

-- >>> testBounded 2

-- >>> testBounded 3

{-
QuickCheck is defined by a class of types that can construct random
values. Let's do it first the hard way... i.e. by explicitly passing around the
state of the random number generator.

-}

-- | Extract random values of any type
class Arb1 a where
  arb1 :: StdGen -> (a, StdGen)

instance Arb1 Int where
  arb1 = Random.uniform

instance Arb1 Bool where
  arb1 = Random.uniform

{-
What about for pairs? Note that Haskell needs the type annotations for
the two calls to `arb1` to resolve ambiguity.
-}

instance (Arb1 a, Arb1 b) => Arb1 (a, b) where
  arb1 :: StdGen -> ((a, b), StdGen)
  arb1 s =
    let (a :: a, s1) = arb1 s
        (b :: b, s2) = arb1 s
     in ((a, b), s2)

{-
And for lists? Give this one a try!  Although we don't have QCs combinators
available, you should be able to control the frequency of when cons and nil
is generated so that you get reasonable lists.

-}

instance Arb1 a => Arb1 [a] where
  arb1 s = undefined

testArb1 :: Arb1 a => Int -> a
testArb1 = fst . arb1 . mkStdGen

-- >>> testArb1 1 :: [Int]

-- >>> testArb1 2 :: [Int]

-- >>> testArb1 3 :: [Int]

{-
Ouch, there's a lot of state passing going on here.

State Monad to the Rescue
-------------------------

Previously, we have developed a reusable library for the State monad.
Let's use it to *define* a generator monad for QuickCheck.

Our reusable library defines an abstract type for the state monad, and
the following operations for working with these sorts of computations.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type State s a = ...

instance Monad (State s) where ...

get      :: State s s
put      :: s -> State s ()

runState :: State s a -> s -> (a,s)
~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let's define a type for generators, using the state monad.
-}

type Gen a = S.State StdGen a

{-
With this type, we can create a type class similar to the one in the
QuickCheck library.
-}

class Arb a where
  arb :: Gen a

{-
For example, we can use the operations on the state monad to access and update the
random number generator stored in the `State StdGen a` type.
-}

instance Arb Int where
  arb = do
    s <- S.get
    let (y :: Int, s') = Random.uniform s
    S.put s'
    return y

{-
What if we want a bounded generator? See if you can define one without using `Random.uniformR`.
-}

bounded :: Int -> Gen Int
bounded b = undefined

{-
Now define a `sample` function, which generates and prints 10 random values.
-}

sample :: Show a => Gen a -> IO ()
sample gen = do
  seed <- (Random.randomIO :: IO Int) -- get a seed from the global random number generator
  -- hidden in the IO monad
  undefined

{-
For example, you should be able to sample using the `bounded` combinator.

    ghci> sample (bounded 10)
    5
    9
    0
    5
    4
    6
    0
    0
    7
    6

What about random generation for other types?  How does the state
monad help that definition? How does it compare to the version above?
-}

instance (Arb a, Arb b) => Arb (a, b) where
  arb = undefined

{-
Can we define some standard QuickCheck combinators to help us?
What about `elements`, useful for the `Bool` instance ?
-}

elements :: [a] -> Gen a
elements = undefined

instance Arb Bool where
  arb = elements [False, True]

{-
or `frequency`, which we can use for the `[a]` instance ?
-}

frequency :: [(Int, Gen a)] -> Gen a
frequency = undefined

instance (Arb a) => Arb [a] where
  arb = frequency [(1, return []), (3, (:) <$> arb <*> arb)]
