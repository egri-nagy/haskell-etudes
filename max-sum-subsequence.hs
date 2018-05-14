-- Write a function that, given a list of integers, finds the subsequence
-- of those integers that sums to a value higher than any other subsequence.
-- For example, given the list:
-- [-2, 1, -3, 4, -1, 2, 1, -5, 4]
-- the subsequence with the maximum sum is [4, -1, 2, 1] which sums to 6.
-- There are no other subsequences in the list that sum to value that is 6
-- or greater.


-- provides all subsequence start-end pairs for length n
start_end_pairs :: Int -> [(Int,Int)]
start_end_pairs n = [(x,y) | x <- l, y <- l, x <= y]
  where l = [1..n]

-- extract a subsequence defined by a start-end pair
extract :: (Int,Int) -> [Int] -> [Int]
extract (start,end) l = drop (start - 1) (take end l)

-- map (\p -> extract p [1..10]) (start_end_pairs 10)
