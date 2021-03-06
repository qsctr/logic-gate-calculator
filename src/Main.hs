module Main where

import BitsConvert
import Calculator
import Display
import Numeric
import Prelude hiding ((+), (-), (*), sum)
import System.Random (randomRIO)

main :: IO ()
main = do
    x <- randomRIO (0, 15)
    y <- randomRIO (0, 15)
    putStrLn $ "x = " ++ showHex x ""
    putStrLn $ "y = " ++ showHex y ""
    let xBits = intToBits x
        yBits = intToBits y
        sum = xBits + yBits
        diff = xBits - yBits
        prod = xBits * yBits
    putStrLn $ "x + y = " ++ bitsToHex sum
    putStrLn $ "x - y = " ++ bitsToHex diff
    putStrLn $ "x * y = " ++ bitsToHex prod
    displayDigits sum diff prod
