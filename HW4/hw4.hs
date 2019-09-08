import Data.List

-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . 
        iterate (\x -> if even x then x `div` 2 else 3 * x + 1) 

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

testFun2' :: Integer -> Bool
testFun2' n = (map fun2' [1..n]) == (map fun2 [1..n])

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

balInsert :: a -> Tree a -> Tree a
balInsert x Leaf = Node 0 Leaf x Leaf
balInsert x (Node h lt y rt)
  | height newLt > height newRt = Node h lt y newRt
  | otherwise = Node ((height newLt) + 1) newLt y rt
  where newLt = (balInsert x lt)
        newRt = (balInsert x rt)

foldTree :: [a] -> Tree a
foldTree = foldr balInsert Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then (not y) else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x) : xs) []

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) ([1..n] \\ (nub [x + y + 2 * x * y |
        x <- [1..n], y <- [1..n], x <= y && (x + y + 2 * x * y) <= n]))

