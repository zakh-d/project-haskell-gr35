module Main (main) where

import Pandigital (returnBiggestHexDoublePandigital)


program5 :: IO ()
program5 = do
    print "Enter n: "
    n <- readLn
    print(returnBiggestHexDoublePandigital (n :: Int))

main :: IO ()
main = do
    print("Choose a program to run: [5/26/32]")

    
