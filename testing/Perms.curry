-- Computing permutations where the first and last element is identical.

{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-overlapping #-}

-- computing permutations
ndinsert :: a -> [a] -> [a]
ndinsert x xs     = x : xs
ndinsert x (y:ys) = y : ndinsert x ys

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = ndinsert x (perm xs)

last :: [a] -> a
last [x]            = x
last (_ : xs@(_:_)) = last xs

firstEqLast :: Eq a => [a] -> [a]
firstEqLast (x:xs) | x == last xs = x:xs

main :: [Int]
main = firstEqLast (perm (1 : [1 .. 8]))
