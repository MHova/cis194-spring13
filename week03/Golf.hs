module Golf where

-- TODO: add comments

skips :: [a] -> [[a]]
skips i =
    map (f $ zip i [1..]) [1..length i]
  where
    f l m = map fst $ filter ((0 ==) . flip mod m . snd) l

localMaxima :: [Integer] -> [Integer]
localMaxima l =
    map (snd . fst) . filter f $ zip (zip l (tail l)) (tail $ tail l)
  where
    f ((a, b), c) = b > a && b > c

histogram :: [Integer] -> String
histogram = undefined
