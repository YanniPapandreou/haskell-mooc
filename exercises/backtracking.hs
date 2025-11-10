-- There are N integers with 77 digits such that the sum of any three consecutive digits within the integer is at most 7.
-- Find N and input the last three digits as your answer.

sumsOfN :: Int -> [Int] -> [Int]
sumsOfN n xs
  | n <= 0 = []
  | length xs < n = []
  | otherwise = map (sum . take n) (take (length xs - n + 1) (tails xs))
  where
    tails [] = []
    tails ys@(_ : ys') = ys : tails ys'

isValid :: [Int] -> Bool
isValid (0:_) = False
isValid xs = all (<= 7) (sumsOfN 3 xs)

solve :: Int -> Maybe [[Int]]
solve n
  | n <= 0 = Nothing
  | otherwise = Just (backtrack n [])
  where
    backtrack 0 acc = [reverse acc]
    backtrack k acc = concatMap (\d -> if isValid (d : acc) then backtrack (k - 1) (d : acc) else []) [0 .. 9]
