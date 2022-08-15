
someNumber :: Int -> Int
someNumber n | n <= 0 = 0
             | otherwise = n ? someNumber (n-1)

mkList :: Int -> [Int]
mkList n = [n]

test1, test2 :: Int -> [Int]
-- slow, because factorial will be applied to a list that is not known to have deterministic contents.
test1 n = factorialL $   mkList (someNumber n)
-- fast, because factorial will be applied to a list that is known to have deterministic contents due to normalForm.
test2 n = factorialL $!! mkList (someNumber n)
-- slow, because factorial will be applied to number that might be non-deterministic.
-- The container structure where we can mark the number as deterministic is missing.
-- But this should be fixable in theory.
test3 :: Int -> Int
test3 n = factorial $!! (someNumber n)

factorialL :: [Int] -> [Int]
factorialL [] = []
factorialL (x:xs) = factorial x : factorialL xs

factorial :: Int -> Int
factorial n | n <= 0    = 0
            | n == 1    = 1
            | otherwise = n * factorial (n-1)
