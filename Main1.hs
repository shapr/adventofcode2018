{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.List (sort, group, tails)
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (take, takeWhile)


main :: IO ()
main = do input <- TIO.readFile "input1.txt"
          print "day 1 part 1"
          print input
          let res = parseOnly (double `sepBy` char '\n') input
          print $ sum $ fromRight res
          print "day 2 part 2"
          print $ "do the tests work? " <> show tests
          print $ uglyhack (fromRight res ++  fromRight res)


pNum = decimal

fromRight (Right a) = a

-- ugly hack it
hack ns = filter ((> 1) . length) (group $ sort ns)

-- sublists ns =  reverse $ tails $ reverse ns
sublists ns = flip take ns <$> [1..]


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

uglyhack ns = take 1 $ concat $ hack <$> sublists scans
  where scans = scanl (+) 0 (cycle ns)

scans ns = scanl (+) 0 (cycle ns)

tests = all id [
  uglyhack [7, 7, -2, -7, -4] == [[14,14]],
  uglyhack [3, 3, 4, -2, -4] == [[10,10]],
  uglyhack [-6, 3, 8, 5, -6] == [[5,5]],
  uglyhack [1,-1] == [[0,0]]
  ]

--
