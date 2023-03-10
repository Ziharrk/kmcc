{-# OPTIONS_FRONTEND -Wno-overlapping #-}

--------------------------------------------------------------------------------
-- define a palindrome constraint with functional patterns:
pali :: Data a => [a] -> Bool
pali (xs ++ reverse xs) = True
pali (xs ++ _ : reverse xs) = True

test1 :: Bool
test1 = pali [True,False,True]         --> True
test2 :: Bool
test2 = (pali [True,False,False,True]) --> True
test3 :: Bool
test3 = (pali [True,False])            --> fail

longPali :: Int -> [Bool]
longPali n = take n (repeat True) ++ take n (repeat False) ++ [False] ++
             take n (repeat False) ++ take n (repeat True)

main :: Bool
main = pali (longPali 100)
