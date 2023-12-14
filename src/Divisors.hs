module Divisors (
    sumM
) where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

divisors :: Int -> Int
divisors n
    | (sqrt (fromIntegral n) - fromIntegral(isqrt n)) == 0 = 2 * length [x | x <- [1..isqrt(n)], n `mod` x == 0] - 1
    | otherwise = 2 * length [x | x <- [1..isqrt(n)], n `mod` x == 0]

maxDivisorCount :: Int -> Int -> Int
maxDivisorCount n k = maximum [divisors j | j <- [n..n+k-1]]

-- sumM :: Int -> Int -> Int
-- sumM u k = sum [maxDivisorCount n k | n <- [1..u-k+1]]

sumM :: Int -> Int -> Int
-- calculates sum using sliding window approach
sumM u k = sumM' 2 (maxDivisorCount 1 k) (maxDivisorCount 1 k)
  where
    sumM' n prev _sum
      | n >= u - k + 2 = _sum
      | divisors (n + k - 1) > prev = sumM' (n + 1) (divisors (n + k - 1)) (_sum + divisors (n + k - 1))
      | divisors (n - 1) == prev = sumM' (n + 1) (maxDivisorCount n k) (_sum + maxDivisorCount n k)
      | otherwise = sumM' (n + 1) prev (_sum + prev)