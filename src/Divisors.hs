module Divisors (
    sumM
) where

divisors :: Int -> Int
divisors n = length [x | x <- [1..sqrtN], n `mod` x == 0]
    where sqrtN = floor (sqrt (fromIntegral n))

maxDivisorCount :: Int -> Int -> Int
maxDivisorCount n k = maximum [divisors j | j <- [n..n+k-1]]

sumM :: Int -> Int -> Int
sumM u k = sum [maxDivisorCount n k | n <- [1..u-k+1]]