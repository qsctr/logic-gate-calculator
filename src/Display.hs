module Display (displayDigits) where

import Calculator
import Graphics.Gloss.Interface.Pure.Display
import Prelude hiding ((&&), (||), not)

convert :: [Bit] -> [Bit]
convert [a, b, c, d] =
    [ not b && not d || a && not d || a && not b && not c || c && b
        || not a && c || not a && b && d
    , O
    , a && not b || b && c || not a && b || not c && not d && not a
    , not b && c || a && d || c && not d || a && not b || not a && b && not c
    , a && not c && not d || c && not d || a && b || a && c && d
    , not c && not a || d && not a || not c && d || not a && b || a && not b
    , b && (not c || d) || a && not (c || d) || b && not c && d
        || not b && c && d || b && c && not d || not (a || b) && c ]

segments :: [Picture]
segments =
    [ translate 0 160 horizontal
    , translate (-80) 80 vertical
    , translate 80 80 vertical
    , horizontal
    , translate (-80) (-80) vertical
    , translate 80 (-80) vertical
    , translate 0 (-160) horizontal ]
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
    display (InWindow "Segment LED" (600, 360) (0, 0)) white $ pictures $
        [ translate (-200) 0 $ digit d1
        , digit d2
        , translate 200 0 $ digit d3 ]
