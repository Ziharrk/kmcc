-- Example to show why it is neceesary to consider finger prints
-- for choice decisions only if the stack is empty (and not
-- in intermediate hnf computations).

not :: Bool -> Bool
not False = True
not True  = False

(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && x = x

f :: Bool -> Bool -> Bool
f x y = x && y ? y

g :: Bool -> Bool
g x = f x (not x)

main :: Bool
main = g (False ? True)
-- -> False ? False ? True ? False
