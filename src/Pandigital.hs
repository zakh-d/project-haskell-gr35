module Pandigital
    (  returnBiggestHexDoublePandigital
    ) where



returnBiggestHexDoublePandigital :: Integer -> Integer
returnBiggestHexDoublePandigital n
  | n > 15 = 0
  | otherwise = foldl (\acc i -> acc * 16 + (n - i)) 0 (replicate 2 =<< [0..n])