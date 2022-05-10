{-# OPTIONS_GHC -Wincomplete-patterns #-}

{-# OPTIONS_GHC -fdefer-type-errors  #-}

module Main where
import Prelude hiding (reverse, concat, zip, (++))
import Test.HUnit
import Data.List (minimumBy)  
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Text.Read as Read
import Data.Ord (comparing)

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testWeather,
                               testSoccer ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzip ]

-- Part One

abc :: Bool -> Bool -> Bool -> Bool
abc x y z = (x && y) || (x && z)

tabc :: Test
tabc = "abc" ~: TestList [abc True False True  ~?= True,
                          abc True False False ~?= False,
                          abc False True True  ~?= False]

-- Part Two

arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) = (b*f -c*e, c*d - a*f, a*e - b*d)

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

-- Part Three
reverse :: [a] -> [a]
reverse [] = []
reverse l =  rev l []
  where
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

-- Part Four

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []
tzip :: Test
tzip = "zip" ~:
  TestList [ zip "abc" [True,False,True] ~?= [('a',True),('b',False), ('c', True)],
             zip "abc" [True] ~?= [('a', True)],
             zip [] [] ~?= ([] :: [(Int,Int)]) ]

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList
  [tstartsWith, tendsWith, ttranspose, tconcat, tcountSub]

-- Part One

-- | The 'startsWith' function takes two strings and returns 'True'
-- iff the first is a prefix of the second.
--
-- >>> "Hello" `startsWith` "Hello World!"
-- True
--
-- >>> "Hello" `startsWith` "Wello Horld!"
-- False

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

tstartsWith :: Test
tstartsWith = "startsWith" ~: (assertFailure "testcase for startsWith" :: Assertion)

-- Part Two

-- | The 'endsWith' function takes two lists and returns 'True' iff
-- the first list is a suffix of the second. The second list must be
-- finite.
--
-- >>> "ld!" `endsWith` "Hello World!"
-- True
--
-- >>> "World" `endsWith` "Hello World!"
-- False

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith xs ys = startsWith (reverse xs) (reverse ys)

tendsWith :: Test
tendsWith = "endsWith" ~: (assertFailure "testcase for endsWith" :: Assertion)


-- Part Three

-- | The concatenation of all of the elements of a list of lists
--
-- >>> concat [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
--
-- NOTE: do not use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.

helper :: [a] -> [a] -> [a]
helper [] ys = ys
helper (x:xs) ys = x:xs `helper` ys

concat :: [[a]] -> [a]
concat = foldr (helper) []

tconcat :: Test
tconcat = "concat" ~: (assertFailure "testcase for concat" :: Assertion)

-- Part Four

-- | The 'transpose' function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose.
--
-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
-- >>> transpose [[1,2],[3,4,5]]
-- [[1,3],[2,4]]
-- >>> transpose ([[]] :: [[Integer]])
-- []

-- transpose is defined in Data.List
-- (WARNING: this one is tricky!)

transpose :: [[a]] -> [[a]]
transpose list
  | any null list = []
  | otherwise =
      foldr buildRow [] list : transpose (foldr deleteRow [] list)
  where
    buildRow cols row = take 1 cols `helper` row
    deleteRow row rows = drop 1 row : rows

transpose' :: [[a]] -> [[a]]
transpose' l
  | any null l = []
  | otherwise  = map head l : transpose' (map tail l)

ttranspose :: Test
ttranspose = "transpose" ~: (assertFailure "testcase for transpose" :: Assertion)

-- Part Five

-- | The 'countSub' function returns the number of (potentially overlapping)
-- occurrences of a substring sub found in a string.
--
-- >>> countSub "aa" "aaa"
-- 2

countSub :: Eq a => [a] -> [a] -> Int
countSub = error

tcountSub :: Test
tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)
--------------------------------------------------------------------------------

-- Part One: Weather

weather :: String -> String
weather str = show $ (\(a, _, _) -> a) $ auxHelp str

auxHelp :: String -> (Int, Int, Int)
auxHelp str = minimumBy (comparing (\(_, a, b) -> a+b)) $ map quickConvert $ getWeatherList str

getWeatherList :: String -> [[Maybe Int]]
getWeatherList str = filter isValid (map (parseLine . words) (lines str))

parseLine :: [String] -> [Maybe Int]
parseLine [a, b, c, es] = [readInt a, readInt b, readInt c]
parseLine _ = []

isValid :: [Maybe Int] -> Bool
isValid [Just a, Just b, Just c] = True
isValid _ = False

quickConvert :: [Maybe Int] -> (Int, Int, Int)
quickConvert [Just a, Just b, Just c] = (a, b, c)
quickConvert _ = (-90, 0, 0)

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "jul19.dat"
  putStrLn (weather str)

readInt :: String -> Maybe Int
readInt = Read.readMaybe

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "jul19.dat"
                              weather str @?= "8"

-- Part Two: Soccer League Table

soccer :: String -> String
soccer = error "unimplemented"
 

soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "soccer.dat"
  putStrLn (soccer str)

testSoccer :: Test
testSoccer = "soccer" ~: do
  str <- readFile "soccer.dat"
  soccer str @?= "Aston_Villa"

-- Part Three: DRY Fusion

weather2 :: String -> String
weather2 = undefined

soccer2 :: String -> String
soccer2 = undefined

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?

shortAnswer1 :: String
shortAnswer1 = "Fill in your answer here"

-- Was the way you wrote the second program influenced by writing the first?

shortAnswer2 :: String
shortAnswer2 = "Fill in your answer here"

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?

shortAnswer3 :: String
shortAnswer3 = "Fill in your answer here"



