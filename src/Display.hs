module Display (displayDigits) where

import Calculator
import Graphics.Gloss.Interface.Pure.Display
import Prelude hiding ((&&), (||), not)

convert :: [Bit] -> [Bit]
convert [a, b, c, d] =
    [ not b && not d || not a && c || b && c || a && not d || not a && b && d
        || a && not b && not c
    , not b && not d || not b && not c || not a && not c && not d
        || a && not c && d || not a && c && d
    , a && not b || not c && d || not b && not c || not a && d || not a && b
    , not a && not b && not d || not b && c && d || b && not c && d
        || a && b && not c || a && not c && not d || b && c && not d
    , not b && not d || a && b || c && not d || a && c
    , not c && not d || a && not b || b && not d || a && c
        || not a && b && not c
    , a && not b || c && not d || a && d || not a && b && not c || not b && c ]

segments :: [Picture]
segments =
    [ translate 0 160 horizontal
    , translate 80 80 vertical
    , translate 80 (-80) vertical
    , translate 0 (-160) horizontal
    , translate (-80) (-80) vertical
    , translate (-80) 80 vertical
    , horizontal ]
  where horizontal = polygon
            [(-80, 0), (-70, 10), (70, 10), (80, 0), (70, -10), (-70, -10)]
        vertical = rotate 90 horizontal

led :: Bit -> Picture -> Picture
led I = color red
led O = color (greyN 0.8)

digit :: [Bit] -> Picture
digit bits = pictures $ zipWith led (convert bits) segments

displayDigits :: [Bit] -> [Bit] -> [Bit] -> IO ()
displayDigits d1 d2 d3 =
    display (InWindow "Segment LED" (620, 360) (0, 0)) white $ pictures $
        [ translate (-200) 0 $ digit d1
        , digit d2
        , translate 200 0 $ digit d3 ]
