module Calculator where

import Prelude ()

data Bit = I | O

nand :: Bit -> Bit -> Bit
O `nand` O = I
O `nand` I = I
I `nand` O = I
I `nand` I = O

(&&) :: Bit -> Bit -> Bit
p && q = (p `nand` q) `nand` (p `nand` q)
infixr 3 &&

(||) :: Bit -> Bit -> Bit
p || q = (p `nand` p) `nand` (q `nand` q)
infixr 2 ||

not :: Bit -> Bit
not p = p `nand` p

xor :: Bit -> Bit -> Bit
p `xor` q = (p || q) && (p `nand` q)

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (a `xor` b, a && b)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b c =
    let (s, c')   = halfAdder a b
        (s', c'') = halfAdder c s
    in  (s', c' || c'')

(+) :: [Bit] -> [Bit] -> [Bit]
[a3, a2, a1, a0] + [b3, b2, b1, b0] =
    let (s0, c0) = halfAdder a0 b0
        (s1, c1) = fullAdder a1 b1 c0
        (s2, c2) = fullAdder a2 b2 c1
        (s3, _)  = fullAdder a3 b3 c2
    in  [s3, s2, s1, s0]
infixl 6 +

inverse :: [Bit] -> [Bit]
inverse xs = [not x | x <- xs]

(-) :: [Bit] -> [Bit] -> [Bit]
a - b = a + (inverse b + [O, O, O, I])
infixl 6 -

(*) :: [Bit] -> [Bit] -> [Bit]
[a3, a2, a1, a0] * [b3, b2, b1, b0]
    = [b0 && a3, b0 && a2, b0 && a1, b0 && a0]
    + [b1 && a2, b1 && a1, b1 && a0, O       ]
    + [b2 && a1, b2 && a0, O       , O       ]
    + [b3 && a0, O       , O       , O       ]
infixl 7 *
