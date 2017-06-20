module Golf where

import Data.List (group, sort)

skips :: [a] -> [[a]]
skips i =
    map (f $ zip i [1..]) [1..length i]
  where
    f l m = map fst $ filter ((0 ==) . flip mod m . snd) l

localMaxima :: [Integer] -> [Integer]
localMaxima l =
    -- 1) `(tail $ tail l)` is l with the first two elements dropped

    -- 2) `tail l` is l with the first element dropped

    -- 3) `zip l (tail l)` produces a list of the form [(a, b)] where each element
    -- a is paired with the element b that comes after it in l. The size of
    -- `tail l` is 1 smaller than the size of `l`, so the last element of `l` is
    -- dropped altogether from the zip operation.

    -- 4) `zip (zip l (tail l)) (tail $ tail l)` takes the output from the last
    -- comment and zips it again with (tail $ tail l), producing a list of the
    -- form [((a, b), c)] where a, b, and c are 3 consecutive elements from the
    -- original list l. The size of this final list will be (length l - 2).

    -- 5) Apply the `f` filter (annotated below) to this final list to find the
    -- nested triples that contain a local maxima.

    -- 6) map (snd . fst) on this filtered list to pull out all the b's out of
    -- the [((a, b), c)]
    map (snd . fst) . filter f $ zip (zip l (tail l)) (tail $ tail l)
  where
    -- Given a nested Integer triple in the form of ((a, b), c), return
    -- whether b is greater than both a and c. If so, then this triple contains
    -- a local maxima!
    f :: ((Integer, Integer), Integer) -> Bool
    f ((a, b), c) = b > a && b > c


histogram :: [Integer] -> String
histogram l =
    {- [0..m - 1] gives us the 0-based indexes from 0 to m - 1 where m, as
       defined below, is the height of the histogram.

       `map f [0..m - 1]` creates the thing described in f, below. We're mapping
       over the list of *indices*. Note that `v`, which is the giant astericks list
       we have, is used inside of `f`.
    -}
    concat $ reverse (map f [0..m - 1]) ++ ["==========\n0123456789\n"]

  where
    {- Create a 10-item list of frequencies of the digits from 0 to 9. First append
       the list [0..9] to the input to guarantee that each digit is represented.
       Then `sort` the list and `group` the items. Convert each resulting sublist to
       its length - 1 (length - 1 because we tacked on an extra [0..9] at the beginning)
    -}
    x :: [Int]
    x = map ((+ (-1)) . length) . group . sort $ [0..9] ++ l

    -- The highest frequency out of all the digits. This determines how high the
    -- histogram will need to be and therefore how many lines we have.
    m :: Int
    m = maximum x

    -- Convert each item in x into astericks strings with enough spaces padded on the
    -- right to reach size m
    v :: [String]
    v = map (\a -> take m $ replicate a '*' ++ repeat ' ') x

    -- Construct a String from the i-th character in each of the Strings in v. Also
    -- append a linebreak. This essentially flips v 90 degrees.
    f :: Int -> String
    f i = map (!! i) v ++ "\n"

