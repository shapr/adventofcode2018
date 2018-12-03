{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.Text
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text hiding (take, takeWhile)

main :: IO ()
main = do input <- TIO.readFile "input3.txt"
          let ls = T.lines input
          print "day 3 part 1"
          let claims = fromRight (parseM input :: Either String [Claim])
          let coords = Prelude.concatMap (getCoords . conv) claims
          let counts = Prelude.foldl myIns emptyCmap coords
          print $ Prelude.length $ M.toList $ M.filter (> 1) counts
          -- print $ Prelude.length counts
          -- 120813 is too high
          print "hi"

data Claim = C Int Int Int Int Int deriving (Show, Eq, Ord)

-- "#11 @ 569,720: 28x29"
pClaim = C <$ char '#'
  <*> decimal
  <* string " @ "
  <*> decimal
  <* char ','
  <*> decimal
  <* string ": "
  <*> decimal
  <* char 'x'
  <*> decimal


fromRight (Right x) = x

test :: Text
test = "#11 @ 569,720: 28x29"

parseM = parseOnly (pClaim `sepBy` char '\n')

-- which inches have overlap?
starts (C _ x y _ _) = (x,y)

lengths (C _ _ _ w h) = (w,h)

-- Data.Map to the rescue?

emptyCmap :: M.Map (Int,Int) Int
emptyCmap = M.empty

getCoords (x1,y1,x2,y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

myIns m k = M.insertWith (+) k 1 m

-- convert Claim to (x1,y1,x2,y2)
conv (C _ x1 y1 xoff yoff) = (x1+1, y1+1, x1+xoff, y1+yoff)
