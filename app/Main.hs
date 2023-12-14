module Main (main) where

import Pandigital (returnBiggestHexDoublePandigital)
import Divisors (sumM)


program5 :: IO ()
program5 = do
    print "Enter n: "
    n <- readLn
    print(returnBiggestHexDoublePandigital (n :: Int))


program26 :: IO ()
program26 = do
    let result1 = sumM 1000 10
    let result2 = sumM 10000 100
    let result3 = sumM 100000 1000
    let result4 = sumM 1000000 10000
    let result5 = sumM 100000000 100000

    putStrLn $ "S(1000, 10) = " ++ show result1
    putStrLn $ "S(10000, 100) = " ++ show result2
    putStrLn $ "S(100000, 1000) = " ++ show result3
    putStrLn $ "S(1000000, 10000) = " ++ show result4
    putStrLn $ "S(100000000, 100000) = " ++ show result5

main :: IO ()
main = program26

    
