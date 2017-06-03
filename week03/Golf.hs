module Golf where

import Data.List (group, sort)

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
histogram l =
    concat $ reverse (map f [0..m - 1]) ++ ["==========\n0123456789\n"]
  where
    -- read from the bottom

    f i = map (!! i) v ++ "\n"
    -- ^ construct a String from the i-th character in each of the Strings in v. Also
    --   append a linebreak.
    v = map (\a -> take m $ replicate a '*' ++ repeat ' ') x
    -- ^ convert each item in x into astericks strings with enough spaces padded on the
    --   right to reach size m
    m = maximum x
    -- ^ the highest frequency out of all the digits
    x = map (flip (-) 1 . length) . group . sort $ [0..9] ++ l
    {- ^ create a 10-item list of frequencies of the digits from 0 to 9. First append
         the list [0..9] to the input to guarantee that each digit is represented.
         Then `sort` the list and `group` the items. Convert each resulting sublist to
         its length - 1 (length - 1 because we tacked on an extra [0..9] at the beginning)
    -}
