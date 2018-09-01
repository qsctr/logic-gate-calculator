module BitsConvert where

import Calculator (Bit (..))
import Numeric

intToBits :: Int -> [Bit]
intToBits = reverse . take 4 . map (convert . (`mod` 2)) . iterate (`div` 2)
  where convert 1 = I
        convert 0 = O

bitsToInt :: [Bit] -> Int
bitsToInt = sum . zipWith (*) (map (2^) [0..]) . map convert . reverse
  where convert I = 1
        convert O = 0

bitsToHex :: [Bit] -> String
bitsToHex = flip showHex "" . bitsToInt
