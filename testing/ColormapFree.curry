{-# OPTIONS_FRONTEND -Wno-incomplete-patterns #-}
-- Graph coloring with free variables

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

-- correct coloring with free variables (and narrowing on `diff`)
correctFree :: [Color]
correctFree =
  cond (diff l1 l2 && diff l1 l3 && diff l2 l3 && diff l2 l4 && diff l3 l4)
       [l1,l2,l3,l4]
 where l1,l2,l3,l4 free

main :: [Color]
main = correctFree

