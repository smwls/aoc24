module Day2 where
import AocTypes
import Text.Read (readMaybe)

day2 :: RunFunction ReportList
day2 = RunFunction parseInput day2a day2b
day2a input = show $ foldr ((\v a -> if satisfiesRequirements v then a + 1 else a) . diffs) 0 input
day2b input = show $ foldr (\v a -> if satisfiesRequirements (diffs v) || any ((satisfiesRequirements . diffs) . removeAt v) [0..length v - 1] then a + 1 else a) 0 input
    where
        removeAt :: [Int] -> Int -> [Int]
        removeAt [] _ = []
        removeAt xs n
          | n == 0 = tail xs
          | length xs >= n = take n xs ++ drop (n + 1) xs
          | otherwise = xs

type Report = [Int]
type ReportList = [Report]
parseInput :: String -> Maybe ReportList
parseInput input = mapM maybeReadWords (lines input)
    where maybeReadWords s = mapM readMaybe (words s)

diffs :: [Int] -> [Int]
diffs [] = []
diffs (x:xs) = subPair <$> zip xs (x:xs)
    where subPair (a, b) = a - b

satisfiesRequirements :: [Int] -> Bool
satisfiesRequirements xs = allSameSign xs && bounded xs
    where
        bounded = all (\y -> abs y >= 1 && abs y < 4)
        allSameSign xss = let (flipped, _) = foldr flips (False, 0) xss in not flipped
        flips :: Int -> (Bool, Int) -> (Bool, Int)
        flips val (signFlipped, lastVal) = if val /= 0 && val * lastVal < 0 then (True, val) else (signFlipped, val)