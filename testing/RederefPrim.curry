-- Regression test for the re-dereferencing of the first argument of a
-- primitive operation: evaluating the second argument binds the variable that
-- the first argument was dereferenced to.

bindTo3 :: Int -> Int
bindTo3 x | x =:= 3 = 5

-- Comparison (uses "primitive2Bool"):
leq :: Bool
leq = x <= bindTo3 x  where x free

-- Arithmetic (uses "primitive2"):
plus :: Int
plus = x + bindTo3 x  where x free

main :: (Bool, Int)
main = (leq, plus)
