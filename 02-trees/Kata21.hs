{-
---
fulltitle: "In class exercise: Kata code review"
date: September 22, 2021
---

Below are three different solutions to the Kata problem from hw01 submitted by members of
the Fall 2021 CIS 552 class.

Questions to discuss and answer with your partner:

 1. Which of these answers is the most readable to you right now? Why?

 2. The general structure of the problem is to read in data from a test file and
    then calculate with that data. In which of these versions is that
    structure the most apparent?

 3. How could each these examples be improved? Pick one (not yours) and improve it.

-}

module Kata21 where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Test.HUnit (Test (TestList), runTestTT, (@?=), (~:))
import qualified Text.Read as Read

readInt :: String -> Maybe Int
readInt = Read.readMaybe

-----------------------------------
-- SAMPLE A --

-- | Unwords consecutive strings that are not integers.
reassembleStrings :: [String] -> [String]
reassembleStrings str =
  let grouped = List.groupBy (\s1 s2 -> isNotInt s1 && isNotInt s2) str
   in map unwords grouped
  where
    isNotInt = Maybe.isNothing . readInt

-- | Get n-th element of list, or Nothing if list is too short.
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth n (_ : xs) = nth (n - 1) xs

-- | Returns data extracted into a list of rows of columns, where rows are
-- separated by newlines and columns are separated by whitespace.
extractData :: String -> [[String]]
extractData str = map (reassembleStrings . words) (lines str)

-- | Returns data with header and footer rows removed.
removeHeaderFooter :: Int -> Int -> [[String]] -> [[String]]
removeHeaderFooter numHeaderRows numFooterRows rows =
  let numDataRows = length rows - numHeaderRows - numFooterRows
   in take numDataRows (drop numHeaderRows rows)

-- | For [parseColumns idCol bigCol smallCol rows],
-- where the [col] indices are 0-indexed column numbers,
-- return rows where each element is of form (|big - small|, id).
parseColumns :: Int -> Int -> Int -> [[String]] -> [(Int, String)]
parseColumns idCol bigCol smallCol =
  let getColumns row = case (nth idCol row, nth bigCol row, nth smallCol row) of
        (Just id, Just big, Just small) -> Just (id, big, small)
        _ -> Nothing
   in let computeDiff (id, big, small) = case (readInt big, readInt small) of
            (Just big, Just small) -> Just (abs (big - small), id)
            _ -> Nothing
       in Maybe.mapMaybe computeDiff . Maybe.mapMaybe getColumns

computeMin :: [(Int, String)] -> Maybe String
computeMin rows = case rows of
  [] -> Nothing
  parsedData@(hd : _) -> Just (snd (foldr min hd parsedData))

weather2_A :: String -> Maybe String
weather2_A = computeMin . parseColumns 0 1 2 . removeHeaderFooter 18 52 . extractData

soccer2_A :: String -> Maybe String
soccer2_A = computeMin . parseColumns 1 6 7 . removeHeaderFooter 1 5 . extractData

-----------------------------------
-- SAMPLE B --

-- | parses an list of lines, searching for the second line of equals signs
findSecondEquals :: [String] -> Int -> [String]
findSecondEquals ([] : lines) count = findSecondEquals lines count
findSecondEquals (line : lines) count =
  if (line !! 0) == '='
    then if count == 1 then lines else findSecondEquals lines 1
    else findSecondEquals lines count
findSecondEquals [] _ = []

-- | given a list of lines, return a list where each element is the temperautre range in that line
-- Stops when encounters an equals sign or runs out of lines
getTemps :: [String] -> [Int]
getTemps [] = []
getTemps ([] : lines) = getTemps lines
getTemps (line : lines) = if (line !! 0) == '=' then [] else getRange line : getTemps lines

-- Given a string representing a line of data, returns the high - low range
getRange :: String -> Int
getRange [] = 99999
getRange line = extractJust (readInt [line !! 4, line !! 5]) (readInt [line !! 8, line !! 9])

-- | Takes in 2 Maybe Ints and returns they're differece, and 99999 if either is Nothing
extractJust :: Maybe Int -> Maybe Int -> Int
extractJust (Just x) (Just y) = x - y
extractJust _ _ = 99999

-- | Given a list of Ints in a line, returns the absolute winLoss Difference
winLoss :: [Int] -> Int
winLoss [] = error "winLoss called on empty array"
winLoss xs = maximum [xs !! 5 - xs !! 6, xs !! 6 - xs !! 5]

-- | Given a single line broken into words, extracts all the numbers from it
getNumbers :: [String] -> [Int]
getNumbers [] = []
getNumbers (word : words) =
  if Char.isDigit (word !! 0) || word !! 0 == '-'
    then removeJust (readInt word) : getNumbers words
    else getNumbers words

-- | Given the lines, returns an array of the numbers in that line. Each number corresponds to a differenet data point
getWinLossDifferences :: [String] -> [Int]
getWinLossDifferences [] = []
getWinLossDifferences (line : lines) =
  if not (Char.isDigit (line !! 0))
    then getWinLossDifferences lines
    else winLoss (getNumbers (words line)) : getWinLossDifferences lines

removeJust :: Maybe Int -> Int
removeJust (Just x) = x
removeJust Nothing = error "tried Remove Just on nothing"

-- | Given the lines and the index of the team in question, retreives the name of the team
getTeam :: [String] -> Int -> String
getTeam [] _ = error "could not find team with this index"
getTeam (line : lines) i =
  if Maybe.isNothing (readInt (words line !! 0)) || (removeJust (readInt (words line !! 0)) /= (i + 1))
    then getTeam lines i
    else words line !! 1

{-
>
-}

-- | given an Int array, returns the index of the minimum element
minIdx :: [Int] -> Int -> Int -> Int -> Int
minIdx [] _ currMinIdx _ = currMinIdx
minIdx (x : xs) currMin currMinIdx currIdx =
  if x < currMin
    then minIdx xs x currIdx (currIdx + 1)
    else minIdx xs currMin currMinIdx (currIdx + 1)

weather2_B :: String -> Maybe String
weather2_B str = Just (show (minIdx (getTemps (findSecondEquals (lines str) 0)) 99999 0 0 + 1))

soccer2_B :: String -> Maybe String
soccer2_B str = Just (getTeam (lines str) (minIdx (getWinLossDifferences (lines str)) 99999 0 0))

-------------------------------------------------
-- SAMPLE C --

getListOfUsefulLineSplits :: String -> Int -> Int -> [[String]]
getListOfUsefulLineSplits str topMargin usefulLines = map words (take usefulLines (drop topMargin (lines str)))

minTuple :: (a, Int) -> (a, Int) -> (a, Int)
minTuple (i, x) (j, y) = if x <= y then (i, x) else (j, y)

getAggregateMin :: [(String, Int)] -> (String, Int)
getAggregateMin [] = ("null", maxBound :: Int)
getAggregateMin [(i, x)] = (i, x)
getAggregateMin ((i, x) : xs) = minTuple (i, x) (getAggregateMin xs)

getMaybeAnswer :: (String, Int) -> Maybe String
getMaybeAnswer (index, value) = if value /= -1 then Just index else Nothing

{-
>
-}

-- Takes Tuone line in the file, split into a list of values, into an (index, spread) tuple
getSpreadTuple :: [String] -> (String, Int)
getSpreadTuple (i : x : y : _) = (i, calcSpread (readInt x) (readInt y))
getSpreadTuple _ = ("-1", -1)

getAbsoluteDiffTuple :: [String] -> (String, Int)
getAbsoluteDiffTuple (_ : name : _ : _ : _ : _ : f : a : _) = (name, abs (parseInt (readInt f) - parseInt (readInt a)))
getAbsoluteDiffTuple _ = ("null", -1)

-- Takes two ints and calculates the difference between them, the first being max, second being min
calcSpread :: Maybe Int -> Maybe Int -> Int
calcSpread (Just x) (Just y) = x - y
calcSpread _ _ = -1

parseInt :: Maybe Int -> Int
parseInt (Just x) = x
parseInt Nothing = -1

weather2_C :: String -> Maybe String
weather2_C str = getMaybeAnswer (getAggregateMin (map getSpreadTuple (getListOfUsefulLineSplits str 18 31)))

soccer2_C :: String -> Maybe String
soccer2_C str = getMaybeAnswer (getAggregateMin (map getAbsoluteDiffTuple (getListOfUsefulLineSplits str 1 20)))

-----------------------------------

testWeather :: (String -> Maybe String) -> Test
testWeather weather =
  "weather" ~: do
    str <- readFile "jul21.dat"
    weather str @?= Just "18"

testSoccer :: (String -> Maybe String) -> Test
testSoccer soccer =
  "soccer" ~: do
    str <- readFile "soccer20.dat"
    soccer str @?= Just "Everton"

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ "A" ~: testWeather weather2_A,
          "B" ~: testWeather weather2_B,
          "C" ~: testWeather weather2_C,
          "A" ~: testSoccer soccer2_A,
          "B" ~: testSoccer soccer2_B,
          "C" ~: testSoccer soccer2_C
        ]
  return ()
