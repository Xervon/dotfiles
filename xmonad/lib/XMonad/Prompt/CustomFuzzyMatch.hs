module XMonad.Prompt.CustomFuzzyMatch
( customFuzzyMatch
, customFuzzySort
) where

import Data.Char (toLower)
import Data.Function (on)
import Data.List (foldl', groupBy, sort)

customFuzzyMatch :: String -> String -> Bool
customFuzzyMatch q ys = and . map (flip fuzzyMatch ys) $ words q

-- | Returns True if the first argument is a subsequence of the second argument,
-- that is, it can be obtained from the second sequence by deleting elements.
fuzzyMatch :: String -> String -> Bool
fuzzyMatch []         _      = True
fuzzyMatch _          []     = False
fuzzyMatch xxs@(x:xs) (y:ys) | toLower x == toLower y = fuzzyMatch xs  ys
                             | otherwise              = fuzzyMatch xxs ys

customFuzzySort :: String -> [String] -> [String]
customFuzzySort q = map snd . sort . map (flip helper q)
  where
    helper o = (rankAccumulator o) . map (flip rankMatch o) . words
    rankAccumulator o ([]) = ((0, 0), o)
    rankAccumulator o (r:[]) = r
    rankAccumulator _ (((l, r), o):rs) = ((l+lr, min r rr), o)
      where ((lr, rr), _) = rankAccumulator o rs

rankMatch :: String -> String -> ((Int, Int), String)
rankMatch q s = (minimum $ rankMatches q s, s)

rankMatches :: String -> String -> [(Int, Int)]
rankMatches [] _ = [(0, 0)]
rankMatches q  s = map (\(l, r) -> (r - l, l)) $ findShortestMatches q s

findShortestMatches :: String -> String -> [(Int, Int)]
findShortestMatches q s = foldl' extendMatches spans oss
  where (os:oss) = map (findOccurrences s) q
        spans    = [(o, o) | o <- os]

findOccurrences :: String -> Char -> [Int]
findOccurrences s c = map snd $ filter ((toLower c ==) . toLower . fst) $ zip s [0..]

extendMatches :: [(Int, Int)] -> [Int] -> [(Int, Int)]
extendMatches spans = map last . groupBy ((==) `on` snd) . extendMatches' spans

extendMatches' :: [(Int, Int)] -> [Int] -> [(Int, Int)]
extendMatches' []                    _          = []
extendMatches' _                     []         = []
extendMatches' spans@((l, r):spans') xs@(x:xs') | r < x     = (l, x) : extendMatches' spans' xs
                                                | otherwise = extendMatches' spans xs'
