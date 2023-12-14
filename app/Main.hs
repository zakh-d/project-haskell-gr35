module Main (main) where

import Pandigital (returnBiggestHexDoublePandigital)
import Divisors (sumM)


program5 :: IO ()
program5 = do
    putStrLn $ "Enter n: "
    n <- readLn
    putStrLn $ show (returnBiggestHexDoublePandigital (n :: Integer))


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


program32 :: IO ()
program32 = do
    let a1 = [1, 22, 3, 4]
    let a2 = [5, 6, 7, 4]

    if hasSubsetWithSum a1 (sum a1 `div` 2)
        then putStrLn "Zbiór A1 zawiera równoliczne rozłączne podzbiory o równej sumie."
        else putStrLn "Zbiór A1 nie zawiera takich podzbiorów."

    if hasSubsetWithSum a2 (sum a2 `div` 2)
        then putStrLn "Zbiór A2 zawiera równoliczne rozłączne podzbiory o równej sumie."
        else putStrLn "Zbiór A2 nie zawiera takich podzbiorów."


main :: IO ()
main = do
    putStrLn $ "Enter program number: "
    n <- readLn
    case n of
        5 -> program5
        26 -> program26
        32 -> program32
        _ -> print "No such program"

    
