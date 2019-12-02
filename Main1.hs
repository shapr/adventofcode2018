{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text hiding (take, takeWhile)
import qualified Data.HashSet         as S
import           Data.List            (foldl', group, sort, tails)
import qualified Data.Text            as T
import           Data.Text.IO         as TIO

main :: IO ()
main = do input <- TIO.readFile "day1"
          print "day 1 part 1"
          print input
          let res = parseOnly (double `sepBy` char '\n') input
          print $ sum $ fromRight res
          print "day 2 part 2"
          print $ "do the tests work? " <> show tests'
          print $ checkit S.empty (scans $ fromRight res)

fromRight (Right a) = a

checkit s []     = []
checkit s (a:as) = if S.member a s then [a] else checkit (S.insert a s) as

scans ns = scanl (+) 0 (cycle ns)

tests' = and [
          checkit S.empty (scans [7, 7, -2, -7, -4] :: [Int]) == [14]
         , checkit S.empty (scans [3, 3, 4, -2, -4] :: [Int]) == [10]
         , checkit S.empty (scans [-6, 3, 8, 5, -6] :: [Int]) == [5]
         , checkit S.empty (scans [1,-1] :: [Int]) == [0]
         ]
