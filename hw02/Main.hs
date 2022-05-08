{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- {-# OPTIONS_GHC -fdefer-type-errors  #-}

module Main where
import Prelude hiding (takeWhile, all, concat)
import Test.HUnit      -- unit test support

import XMLTypes        -- support file for XML problem (provided)
import Play            -- support file for XML problem (provided)

doTests :: IO ()
doTests = do
  _ <- runTestTT $ TestList [ testHO, testFoldr, testTree, testFoldTree, testXML ]
  return ()

main :: IO ()
main = doTests

testHO :: Test
testHO = TestList [ttakeWhile, tfind, tall, tmap2, tmapMaybe]

-- | `takeWhile`, applied to a predicate `p` and a list `xs`,
-- returns the longest prefix (possibly empty) of `xs` of elements
-- that satisfy `p`.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined
ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: (assertFailure "testcase for takeWhile" :: Assertion)
 

-- | `find pred lst` returns the first element of the list that
-- satisfies the predicate. Because no element may do so, the
-- answer is returned in a `Maybe`.
--
-- >>> find odd [0,2,3,4]
-- Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find = undefined
tfind :: Test
tfind = "find" ~: (assertFailure "testcase for find" :: Assertion)
 

-- | `all pred lst` returns `False` if any element of `lst`
-- fails to satisfy `pred` and `True` otherwise.
--
-- >>> all odd [1,2,3]
-- False

all  :: (a -> Bool) -> [a] -> Bool
all = undefined
tall :: Test
tall = "all" ~: (assertFailure "testcase for all" :: Assertion)
 

-- | `map2 f xs ys` returns the list obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one list is longer than the other, then the extra elements
-- are ignored.
-- i.e.
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- >>> map2 (+) [1,2] [3,4]
-- [4,6]
--
-- NOTE: `map2` is called `zipWith` in the Prelude

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 = undefined

tmap2 :: Test
tmap2 = "map2" ~: (assertFailure "testcase for map2" :: Assertion)

-- | Apply a partial function to all the elements of the list,
-- keeping only valid outputs.
--
-- >>> mapMaybe root [0.0, -1.0, 4.0]
-- [0.0,2.0]
--
-- (where `root` is defined below.)
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = undefined

tmapMaybe :: Test
tmapMaybe = "mapMaybe" ~: (assertFailure "testcase for mapMaybe" :: Assertion)

root :: Double -> Maybe Double
root d = if d < 0.0 then Nothing else Just $ sqrt d

----------------------------------------------------------------------



testFoldr :: Test
testFoldr = TestList [ tconcat,  tstartsWith, tendsWith, ttails, tcountSub]

-- concat

concat :: [[a]] -> [a]
concat = undefined

tconcat :: Test
tconcat = "concat" ~: (assertFailure "testcase for concat" :: Assertion)

-- startsWith

startsWith :: String -> String -> Bool
startsWith = undefined
tstartsWith = "tstartsWith" ~: (assertFailure "testcase for startsWith" :: Assertion)

-- INTERLUDE: para

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para = undefined

tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

tails' = undefined

ttails :: Test
ttails = "tails" ~: TestList [
    "tails0" ~: tails' "abc" ~?= ["abc", "bc", "c", ""],
    "tails1" ~: tails' ""    ~?= [""],
    "tails2" ~: tails' "a"   ~?= ["a",""] ]

-- endsWith

endsWith :: String -> String -> Bool
endsWith = undefined

tendsWith :: Test
tendsWith = "endsWith" ~: (assertFailure "testcase for endsWith" :: Assertion)
 

-- countSub

countSub  :: String -> String -> Int
countSub = undefined
tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)



----------------------------------------------------------------------

testTree :: Test
testTree = TestList [
    tappendTree, tinvertTree, ttakeWhileTree, tallTree, tmap2Tree,
    tinfixOrder1, tinfixOrder2 ]

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

-- The `appendTree` function takes two trees and replaces all of the `Empty`
-- constructors in the first with the second tree.  For example:
--     appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty)
-- returns
--     Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)

appendTree :: Tree a -> Tree a -> Tree a
appendTree = undefined
tappendTree :: Test
tappendTree = "appendTree" ~: (assertFailure "testcase for appendTree"  :: Assertion)
 

-- The `invertTree` function takes a tree of pairs and returns a new tree
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Empty Empty)
-- returns
--     Branch (1,"a") Empty Empty

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = undefined
tinvertTree :: Test
tinvertTree = "invertTree" ~: (assertFailure "testcase for invertTree" :: Assertion)
 

-- `takeWhileTree`, applied to a predicate `p` and a tree `t`,
-- returns the largest prefix tree of `t` (possibly empty)
-- where all elements satisfy `p`.
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

--     takeWhileTree (< 3) tree1   returns   Branch 1 (Branch 2 Empty Empty) Empty
--     takeWhileTree (< 9) tree1   returns   tree1
--     takeWhileTree (< 0) tree1   returns   Empty

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree = undefined
ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: (assertFailure "testcase for takeWhileTree" :: Assertion)
 

-- `allTree pred tree` returns `False` if any element of `tree`
-- fails to satisfy `pred` and `True` otherwise. For example:
--     allTree odd tree1
-- returns
--     False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree = undefined
tallTree :: Test
tallTree = "allTree" ~: (assertFailure "testcase for allTree" :: Assertion)
 

-- WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by `foldTree` can itself be a function.)

-- `map2Tree f xs ys` returns the tree obtained by applying `f` to
-- to each pair of corresponding elements of `xs` and `ys`. If
-- one branch is longer than the other, then the extra elements
-- are ignored.
-- for example:
--     map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
-- returns
--     Branch 4 Empty Empty

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree = undefined

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: (assertFailure "testcase for map2Tree" :: Assertion)

----------------------------------------------------------------------

testFoldTree :: Test
testFoldTree = TestList [ tinfixOrder1, tinfixOrder2, trevOrder, tfoldrTree', tfoldlTree' ]

infixOrder :: Tree a -> [a]
infixOrder Empty = []
infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

exTree :: Tree Int
exTree = Branch 5 (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
                  (Branch 9 Empty (Branch 7 Empty Empty))

testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1,2,4,5,9,7]

infixOrder1 :: Tree a -> [a]
infixOrder1 = undefined

tinfixOrder1 = "infixOrder2" ~: infixOrder1 exTree ~?= [1,2,4,5,9,7]

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ e Empty = e
foldrTree f e (Branch k l r) = foldrTree f (f k (foldrTree f e r)) l

infixOrder2 :: Tree a -> [a]
infixOrder2 = undefined

tinfixOrder2 = "infixOrder2" ~: infixOrder2 exTree ~?= [1,2,4,5,9,7]

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree = undefined

revOrder :: Tree a -> [a]
revOrder = foldlTree (flip (:)) []
trevOrder = "revOrder" ~: revOrder exTree ~?= [7,9, 5, 4, 2, 1]

foldrTree' :: (a -> b -> b) -> b ->  Tree a -> b
foldrTree' = undefined

tfoldrTree' :: Test
tfoldrTree' = "foldrTree'" ~: foldrTree' (+) 0 tree1 ~?= 6

foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
foldlTree' = undefined

tfoldlTree' :: Test
tfoldlTree' = "foldlTree'" ~: foldlTree (+) 0 tree1 ~?= 6

answer1 :: String
answer1 = undefined

----------------------------------------------------------------------




formatPlay :: SimpleXML -> SimpleXML
formatPlay = error "implement formatPlay"





firstDiff :: Eq a => [a] -> [a] -> Maybe ([a], [a])
firstDiff [] [] = Nothing
firstDiff (c : cs) (d : ds)
    | c == d = firstDiff cs ds
    | otherwise = Just (c : cs, d : ds)
firstDiff cs ds = Just (cs, ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do
    f1 <- readFile file1
    f2 <- readFile file2
    case firstDiff f1 f2 of
        Nothing -> return ()
        Just (cs,ds) -> assertFailure msg where
            msg = "Results differ: '" ++ take 20 cs ++ "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do
    writeFile "dream.html" (xml2string (formatPlay play))
    testResults "dream.html" "sample.html"

-----------------------------------------------------------------------------

answer2 :: String
answer2 = undefined

