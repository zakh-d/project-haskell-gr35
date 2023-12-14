module Sets (
    hasSubsetWithSum
)where

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

hasSubsetWithSum :: [Int] -> Int -> Bool
hasSubsetWithSum nums targetSum = any (hasSubsetWithSum' targetSum) (subsequences nums)

hasSubsetWithSum' :: Int -> [Int] -> Bool
hasSubsetWithSum' 0 _      = True
hasSubsetWithSum' _ []     = False
hasSubsetWithSum' n (x:xs) = hasSubsetWithSum' (n - x) xs || hasSubsetWithSum' n xs
