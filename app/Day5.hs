module Day5 where
import AocTypes
import Data.Map (Map, empty, findWithDefault, insert)
import Data.List (sortBy)

day5 :: RunFunction (PageRules, Updates)
day5 = RunFunction parseInput day5a day5b

day5a input = show $ sum $ median <$> filterInvalidUpdates input
day5b input =
    let (prs, us) = input
    in show $ sum $ median . makeUpdateValid prs <$> filter (not . isValidUpdate prs) us

type Page = Int
type PageRules = Map Int [Int]
type Update = [Page]
type Updates = [Update]

parseInput :: String -> Maybe (PageRules, Updates)
parseInput s =
    let
        offset = zip s (tail s ++ ['\n'])
        (rulesList, updates) = break (\(a, b) -> a == '\n' && b == '\n') offset
    in
        Just (parseRules $ fst <$> rulesList, parseUpdates $ fst <$> updates)

parseRules :: String -> PageRules
parseRules s =
    let
        ls = lines s
        parsedLs = parseSingleRule <$> ls

        parseSingleRule :: String -> (Int, Int)
        parseSingleRule ss = (read $ takeWhile (/='|') ss, read . tail $ dropWhile (/='|') ss)
        go :: (Int, Int) -> Map Int [Int] -> Map Int [Int]
        go (a, b) ms = case findWithDefault [] a ms of
            [] -> insert a [b] ms
            xs -> insert a (b:xs) ms
    in
        foldr go empty parsedLs

parseSingleUpdate :: String -> [Int]
parseSingleUpdate s = reverse $ go s []
    where
        go :: String -> [Int] -> [Int]
        go ss xs = if ss == "" then xs else
            let (num, rest) = break (== ',') ss
            in go (if rest == "" then rest else tail rest) (read num:xs)

parseUpdates :: String -> Updates
parseUpdates s = parseSingleUpdate <$> lines (dropWhile (=='\n') s)

orderedPairs :: [a] -> [(a, a)]
orderedPairs [] = []
orderedPairs (x:xs) = ((x,) <$> xs) ++ orderedPairs xs

isValidUpdate :: PageRules -> Update -> Bool
isValidUpdate prs u =
    let
        pairIsValid :: (Int, Int) -> Bool
        pairIsValid (a, b) = elem b $ findWithDefault [] a prs
    in
        all pairIsValid $ orderedPairs u

makeUpdateValid :: PageRules -> Update -> Update
makeUpdateValid prs = sortBy comp
    where comp a b
            | elem b $ findWithDefault [] a prs = LT
            | elem a $ findWithDefault [] b prs = GT
            | otherwise = EQ

filterInvalidUpdates :: (PageRules, Updates) -> Updates
filterInvalidUpdates (prs, us) = filter (isValidUpdate prs) us

median :: [Int] -> Int
median xs = let l = length xs in xs!!(l `div` 2)