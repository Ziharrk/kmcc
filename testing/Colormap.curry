{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-overlapping #-}

-- Graph coloring with non-deterministic functions

-- This is our actual map:
--
-- --------------------------
-- |       |        |       |
-- |       |   L2   |       |
-- |       |        |       |
-- |  L1   |--------|  L4   |
-- |       |        |       |
-- |       |   L3   |       |
-- |       |        |       |
-- --------------------------
--

data Color = Red | Green | Blue

aColor :: Color
aColor = Red
aColor = Green
aColor = Blue

(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

cond :: Bool -> a -> a
cond True x = x

diff :: Color -> Color -> Bool
diff Red   Green = True
diff Red   Blue  = True
diff Green Red   = True
diff Green Blue  = True
diff Blue  Red   = True
diff Blue  Green = True

-- correct coloring where non-deterministic generators are provided
correct :: Color -> Color -> Color -> Color -> [Color]
correct l1 l2 l3 l4 =
  cond (diff l1 l2 && diff l1 l3 && diff l2 l3 && diff l2 l4 && diff l3 l4)
       [l1,l2,l3,l4]

main :: [Color]
main = correct aColor aColor aColor aColor
