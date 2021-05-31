module Core where

import Data.Map (Map)
import qualified Data.Map as Map

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

-- oddNumber: Returns only the odd number of a sequence.
-- Restriction: length
oddNumber :: Integral a => [a] -> [a]
oddNumber xs = filter odd xs

-- reverseSeq: Reverses a sequence.
-- Restriction: reverse
reverseSeq :: Ord a => [a] -> [a]
reverseSeq [] = []
reverseSeq [x] = [x]
reverseSeq (x:xs) = (reverseSeq xs) ++ [x]

-- palindromeDetector: Returns true if the given sequence is a palindrome.
palindromeDetector :: Ord a => [a] -> Bool
palindromeDetector xs = reverse xs == xs

-- fibonacci: Return fibonacci number.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- fibonacciSeq: Returns the first X fibonacci numbers.
fibonacciSeq :: Int -> [Int]
fibonacciSeq n = [fibonacci x |  x <- [0..n]] 

-- maximumNumber: Takes a list and returns the maximum number.
maximunNumber :: Ord a => [a] -> a
maximunNumber [] = error "Error, empthy list"
maximunNumber [x] = x
maximunNumber (x:xs) = max x (maximunNumber xs)

-- getTheCaps: Takes a string and returns a new string containing only the capital letters.

-- duplicateSeq: Duplicates each element of a sequence.