module Golf where

-- Exercise 1
-- Step 1: Zip input with [1..(length input)] (1-indexed indices).
-- Step 2: Filter elements such that (index mod n == 0). This can be
--         effectively done by chaining equality with 0, mod, and 
--         snd. Flip needs to be used on mod so that n can be made
--         the second argument.
-- Step 3: Unzip the filtered elements and apply fst to throw away
--         indices.
-- Loop through Steps 1-3 using list comprehension for n=1..(length input).
skips :: [a] -> [[a]]
skips xs = 
        [fst (unzip (filter ((0 == ) . flip mod n . snd) (zip xs [1..]))) | 
                n <- [1..(length xs)]]

-- Exercise 2
-- If the input has 3 or more elements, we compare the 2nd element to the
-- 1st and the 3rd, then recursively repeat this procedure with the
-- rest of the list. If the list has fewer than 3 elements, we return
-- an empty list.
localMaxima :: [Integer] -> [Integer]
localMaxima (a:r@(b:c:xs))
  | b > a && b > c = [b] ++ (localMaxima r)
  | otherwise = localMaxima r
localMaxima _ = []

-- Exercise 3
-- Step 1: Get counts of each digit by filtering on equality
--         and then getting the length of the filtered list.
-- Step 2: Create a list of Strings by iterating backwards from
--         the maximum encountered count and mapping counts to
--         either a space or an asterisk depending on whether
--         they're less than the current count or not.
-- Step 3: Combine the previously generated list of strings with
--         the '=' border and the digits border, then perform
--         unlines.
countToChar :: Int -> Int -> Char
countToChar m x
  | m <= x = '*'
  | otherwise = ' '

histogram :: [Integer] -> String
histogram xs = 
        let counts = [length (filter (n==) xs) | n <- [0..9]]
         in unlines ([map (countToChar m) counts | 
                 m <- reverse [1..(maximum counts)]] ++
                 ["==========", "0123456789"])
