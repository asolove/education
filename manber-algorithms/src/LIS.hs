-- Longest increasing subsequence
-- Manber ch. 1, Ex 1.3

-- naive recursive solution
-- recalculates many sub-problems excessively
-- but not obvious how to table it because of n
-- seems like you could cut off search somehow
-- maybe by also having a count of exta values <n
-- that could go on head of this solution?

lis_naive xs = lis_gt(xs, 0)

lis_gt([], n) = []
lis_gt(x:xs, n) | x > n = 
  let withx = x : lis_gt(xs, x)
      withoutx = lis_gt(xs, n)
      longer a b = if length a >= length b then a else b
  in longer withx withoutx
lis_gt(x:xs, n) = lis_gt(xs, n)

-- Patience sorting solution
-- Implemented with a list of stacks, this is O(n^2)
-- because we have to linear scan for the right stack each time and
-- do some silly reversing.
-- Could switch representation so we can binary search for right stack
-- to get an n log n solution.
type Stack = [(Int, Maybe Int)]

sort_into_stack :: Int -> [Stack] -> Maybe Int -> [Stack]
sort_into_stack n [] prev = [[(n, prev)]]
sort_into_stack n (s1@((n1,p1):_):ss) prev | n1 > n = ((n, prev):s1) : ss
sort_into_stack n (s1@((n1,p1):_):ss) prev = s1 : sort_into_stack n ss (Just n1)

add_item :: Int -> [[(Int, Maybe Int)]] -> [[(Int, Maybe Int)]]
add_item n stacks = sort_into_stack n stacks Nothing

read_path :: [Stack] -> Maybe Int -> [Int]
read_path _ Nothing = []
read_path (((n,p):xs):ss) (Just n1) | n == n1 = n : read_path ss p
read_path ((_:xs):ss) n = read_path (xs:ss) n

lis_smart xs = reverse $ read_path stacks (Just n1)
  where stacks = reverse $ foldr add_item [] (reverse xs)
        (((n1,_):_):_) = stacks

-- Instances of the problem

small_problem = [3, 1, 2, 4, 5, 0]

problem = [9, 44, 32,12,7,42, 34,92,35,37,41,8,20,27,83,64,61,28,39,93,29,17,13,14,55,21,66,72,23,73,99,1,2,88,77,3,65,83,84,62,5,11,74,68,76,78,67,75,69,70,22,71,24,25,26, 9, 44, 32,12,7,42,34,92,35,37,41,8,20,27,83,64,61,28,39,93,29,17,13,14,55,21,66,72,23,73,99,1,2,88,77,3,65,83,84,62,5,11,74,68,76,78,67,75,69,70,22,71,24,25,26]

-- Note that the answers here are not always the same,
-- but do have the same length!
-- The naive version returns the highest-ending sequence,
-- while the patience sorting version prefers lower-starting sequences.

main = do
  putStrLn $ show $ lis_smart problem
  putStrLn $ show $ lis_naive problem

