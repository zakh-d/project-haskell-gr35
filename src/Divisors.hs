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

sumM :: Int -> Int -> Int
-- calculates sum using sliding window approach
sumM u k = let start = maxDivisorCount 1 k in sumM' 2 (start) (start)
  where
    sumM' n prev _sum
      | n >= u - k + 2 = _sum
      | new > prev = sumM' (n + 1) (new) (_sum + new)
      | old == prev = let newDivisorCount = maxDivisorCount n k in sumM' (n + 1) newDivisorCount (_sum + newDivisorCount)
      | otherwise = sumM' (n + 1) prev (_sum + prev)
      where
          new = divisors (n + k - 1)
          old = divisors (n - 1)
