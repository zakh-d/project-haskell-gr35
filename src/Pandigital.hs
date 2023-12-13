module Pandigital
    (  returnBiggestHexDoublePandigital
    ) where



returnBiggestHexDoublePandigital :: Int -> Maybe Int
returnBiggestHexDoublePandigital n
  | n > 15 = Nothing
  | otherwise = Just $ foldl (\acc i -> acc * 16 + (n - i)) 0 (replicate 2 =<< [2..n])