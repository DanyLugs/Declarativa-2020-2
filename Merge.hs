module Merge where
 
 import Data.List


 mergeSort :: (Ord a) => [a] -> [a]
 mergeSort [] = []
 mergeSort [x] =[x]
 mergeSort xs = mezcla (mergeSort f) (mergeSort s)
   where (f,s) = parte xs


 mezcla :: (Ord a) => [a] -> [a] -> [a]
 mezcla [] [] = []
 mezcla [] xs = xs
 mezcla xs [] = xs
 mezcla (x:xs) (y:ys)
   | x < y = x : mezcla xs (y:ys)
   | otherwise = y : mezcla (x:xs) ys

 parte :: [a] -> ([a],[a])
 parte xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)


 mezclaCon :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
 mezclaCon f [] [] = []
 mezclaCon f xs [] = xs
 mezclaCon f [] xs = xs
 mezclaCon f (x:xs) (y:ys)
   | c == LT = x : mezcla xs (y:ys)
   | otherwise = y : mezcla (x:xs) ys
  where c = f x y

 mergeSortCon:: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
 mergeSortCon f [] = []
 mergeSortCon f [x] =[x]
 mergeSortCon c xs = mezclaCon c (mergeSortCon c f) (mergeSortCon c s)
   where (f,s) = parte xs

