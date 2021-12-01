{-# LANGUAGE OverloadedStrings #-}
module Day01 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Data.Functor ( (<&>) )
import Data.List

import Criterion.Main
import Data.Either

run1 = readInput >>= print . solve1 . parseInput
run2 = readInput >>= print . solve2 . parseInput
--run2 = readInput >>= print . (\x -> solve2 x 1 0)

runDay1 = do
    putStrLn "Day 01"
    run1
    run2

file = "res/day01.txt"

dec :: T.Text -> Int
dec x = case decimal x of
    Left _ -> undefined
    Right (a,_) -> a

readInput :: IO T.Text
readInput = TIO.readFile file

parseInput :: T.Text -> [Int]
parseInput = map dec.T.lines


solve1 :: [Int] -> Int
solve1 [] = 0
solve1 [x] = 0
solve1 (x:y:xs) | x < y = 1 + solve1 (y:xs)
                | otherwise = solve1 (y:xs)

createSlidingWindow :: [Int] -> [Int]
createSlidingWindow (x:y:z:xs) = (x+y+z) : createSlidingWindow (y:z:xs)
createSlidingWindow _ = []

solve2 = solve1.createSlidingWindow

--bench11 = readInput >>= \x -> bench "Day 1 p1" $ whnf x