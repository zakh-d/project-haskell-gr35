module Main (main) where

import Pandigital (returnBiggestHexDoublePandigital)

main :: IO ()
main = do
    n <- readLn
    m <- returnBiggestHexDoublePandigital (n :: Int)
    print m
