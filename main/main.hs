module Core where

import Data.Map (Map)
import qualified Data.Map as Map

myMap :: Map String Int
myMap = Map.fromList [("a", 1), ("b", 2)]

-- Default Map: Takes a default value and a sequence of keys and constructs a map.
defaultMap :: Ord b => a -> [b] -> Map b a
defaultMap v xs = Map.fromList [(x, v) | x <- xs]

-- Get-last: Returns the last element in a sequence.
-- Restriction: last
getLast :: Ord a => [a] -> a
getLast xs = head (reverse xs)

-- Penultimate Element: Returns the second to last element from a sequence.
getPenultimate :: Ord a => [a] -> a
getPenultimate xs = last (init xs)

-- Nth Element: Returns the Nth element from a sequence.
-- Restriction: nth
getElement :: Ord a => [a] -> Int -> a
getElement xs n = head (drop n xs)

-- Count a Sequence: Returns the total number of elements in a sequence.
-- Restriction: length
count :: Ord a => [a] -> Int
count xs = sum  [1 | x <- xs]

-- Sum It All Up: Returns the sum of a sequence number.
-- Restriction: Sum
sumItAll :: Num a => [a] -> a
sumItAll xs = foldr (+) 0 xs