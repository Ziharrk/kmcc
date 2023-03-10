{-# OPTIONS_FRONTEND -Wno-overlapping #-}

-- computing permutations
ndinsert :: a -> [a] -> [a]
ndinsert x xs     = x : xs
ndinsert x (y:ys) = y : ndinsert x ys

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = ndinsert x (perm xs)

main :: [Int]
main = perm [1,2,3,4]
