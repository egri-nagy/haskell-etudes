-- Write a function that, given a list of integers, removes all values that
-- are a multiple of 5 or multiple of 7, but not those values which are a
-- multiple of 35.

-- return desired numbers boxed in a list, empty list for unwanted numbers
p :: Integer -> [Integer]
p n | n `mod` 35 == 0 = [n]
    | (n `mod` 5  == 0) || (n `mod` 7 == 0) = []
    | otherwise = [n]

remove_multiples :: [Integer] -> [Integer]
remove_multiples l = concat (map p l)
