module Tarea2 where

import Data.Bits
​
gtrPower2:: Int -> Int
gtrPower2 n = if popCount (n-1) == 1
              then n-1
              else  gtrPower2 (n-1)

​

inarow :: (Eq a) => [a] -> Int
inarow [] = 0
inarow (x:xs) = inarow_aux (1,1,x) xs
 
ramanujan :: Int -> [(Int,Int,Int,Int)]
ramanujan n = [(a,b,c,d) | a <- [1..n], b <- [a..n],
                            c <- [a+1..n], d <- [c..n],
                            a^3 + b^3 == c^3 + d^3]
                            