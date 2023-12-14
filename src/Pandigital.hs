module Pandigital
    (  returnBiggestHexDoublePandigital
    ) where



returnBiggestHexDoublePandigital :: Integer -> Integer
returnBiggestHexDoublePandigital n
  | n > 15 = 0
  | otherwise = foldl (\acc i -> acc * 16 + (n + 2 - i)) 0 (replicate 2 =<< [2..n + 2])