{-# LANGUAGE MultiWayIf #-}
module Homework1 where

import Data.List ( nub )
import Data.List ()
getNumber :: IO()
getNumber = do
        s <- getLine
        print $ f1_1 (read s :: Int)


f1_1 :: Int -> Int 
f1_1 i = i

f1_2 :: Int -> Bool
f1_2 n = abs n > 1 && null [ x | x <- [2 .. abs n - 1], abs n `mod` x == 0]

f1_3 :: Bool -> Bool -> Int
f1_3 False False = 0
f1_3 True True = 2
f1_3 _ _ = 1

f1_4 :: Int -> Int
f1_4 n = sum [x | x <- [1 .. abs n - 1], abs n `mod` x == 0]

f1_5 :: Int -> Int
f1_5 n = if f1_4 (n + 1) == n + 1 then n + 1
                        else f1_5 (n + 1)

f1_7 :: Int -> Int -> Int 
f1_7 m n = if
        | m == 0          -> n + 1
        | m > 0 && n == 0 -> f1_7 (m - 1) 1
        | m > 0 && n > 0  -> f1_7 (m - 1) (f1_7 m (n - 1))
        | otherwise       -> error "ne nado tak"

f1_8 :: Int -> Int -> Integer
f1_8 n m = toInteger $ f1_7 m n

equalization :: Double -> Double -> Double -> Double -> Double -> Bool
equalization a b c d xi = a**(1/3) * xi + d**(1/3) * (a**(2/3) * xi * xi - (a * d)**(1/3) * xi + d**(2/3)) == -xi * (c + b*xi) 

f1_9 :: Double -> Double -> Double -> Double -> (Double, Double, Double)
f1_9 a b c d = do
        let f = equalization a b c d
        head [ (x1,x2,x3) | x1 <- [1..], f x1, x2 <- [x1..], f x2, x3 <- [x2..], f x3 ]

f1_10 :: Double -> Double -> (Double, Double)
f1_10 a b = (a,b) 

-- f1_11 :: Int -> Double

-- f1_12 :: Int

-- f2_1 :: Ord a => [a] -> [a]\

-- f2_5 :: Eq a => [a] -> [(Int, a)]

antiprimes :: Int -> [Integer]
antiprimes k = take k [toInteger x | x <- [2 .. ], not $ f1_2 x]

f2_3 :: Eq a => [a] -> [a] -> [a]
f2_3 l1 l2 = [x| x <- l1, not (x `elem` l2)] ++ [x| x <- l2, not (x `elem` l1)]

f2_4 :: Eq a => [a] -> [(Int, a)]
f2_4 l = nub [ (length $ filter (== x) l, x) | x <- l ]


 

