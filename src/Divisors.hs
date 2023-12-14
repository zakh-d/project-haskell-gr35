module Divisors (
    sumM
) where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

divisors :: Int -> Int
divisors n = length [x | x <- [1..n], n `mod` x == 0]

maxDivisorCount :: Int -> Int -> Int
maxDivisorCount n k = maximum [divisors j | j <- [n..n+k-1]]

-- sumM :: Int -> Int -> Int
-- sumM u k = sum [maxDivisorCount n k | n <- [1..u-k+1]]

sumM :: Int -> Int -> Int
-- calculates sum using sliding window approach
sumM u k = sumM' 1 (maxDivisorCount 1 k)
    where
        sumM' :: Int -> Int -> Int
        sumM' n prev
            | n > u - k + 1 = 0
            | divisors (n + k - 1) > prev = prev + sumM' (n + 1) (divisors (n + k - 1))
            | divisors (n - 1) == prev = prev + sumM' (n + 1) (maxDivisorCount n k)
            | otherwise = prev + sumM' (n + 1) prev
