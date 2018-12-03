{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List

main :: IO ()
main = do input <- Prelude.readFile "input2.txt"
          print "day 2 part 1"
          let ls = lines input
          print $ check 2 ls * check 3 ls
          print "day 2 part 2"
          print $ map fst $ filter (uncurry (==)) (uncurry zip $ diff ls)

test = [ "abcdef",
         "bababc",
         "abbcde",
         "abcccd",
         "aabcdd",
         "abcdee",
         "ababab"
       ]

check n ls = length $ filter ((> 0) . length) $ (filter ((== n) . length)) <$> (group . sort <$> ls)

test2 = ["abcde",
         "fghij",
         "klmno",
         "pqrst",
         "fguij",
         "axcye",
         "wvxyz"
        ]

diff :: [String] -> (String,String)
diff [] = ("","")
diff (x:xs) = let
  dists = (distance x) <$> xs
  f = filter ((==1) . fst) dists
  in
    if (length f) == 1 then
      (x, snd $ head f)
    else diff xs

distance :: String -> String -> (Int,String)
distance x y = (length $ filter (not . id) $ zipWith (==) x y, y)
