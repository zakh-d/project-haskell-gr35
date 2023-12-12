module Main where

import Data.List

hasSubsetWithSum :: [Int] -> Int -> Bool
hasSubsetWithSum nums targetSum = any (hasSubsetWithSum' targetSum) (subsequences nums)

hasSubsetWithSum' :: Int -> [Int] -> Bool
hasSubsetWithSum' 0 _      = True
hasSubsetWithSum' _ []     = False
hasSubsetWithSum' n (x:xs) = hasSubsetWithSum' (n - x) xs || hasSubsetWithSum' n xs

main :: IO ()
main = do
    let a1 = [1, 2, 3, 4]
    let a2 = [5, 6, 7, 2]

    if hasSubsetWithSum a1 (sum a1 `div` 2)
        then putStrLn "Zbiór A1 zawiera równoliczne rozłączne podzbiory o równej sumie."
        else putStrLn "Zbiór A1 nie zawiera takich podzbiorów."

    if hasSubsetWithSum a2 (sum a2 `div` 2)
        then putStrLn "Zbiór A2 zawiera równoliczne rozłączne podzbiory o równej sumie."
        else putStrLn "Zbiór A2 nie zawiera takich podzbiorów."
