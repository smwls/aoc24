module Day4 where
import AocTypes
import Data.Maybe (listToMaybe)
day4 :: RunFunction
day4 ab input = do
    parsed <- parseInput input
    return $ case ab of
        A -> day4a parsed
        B -> day4b parsed

day4a input = show $ xmasCount input
day4b input = show $ xmasCount2 input
type XmasSearch = [String]

parseInput :: String -> Maybe XmasSearch
parseInput = Just . lines

grid xs = [(i, j) | i <- [0..length xs - 1], j <- [0..length (head xs) - 1]]

xmasCount :: XmasSearch -> Int
xmasCount xs = foldr countAt 0 (grid xs)
    where
        countAt :: (Int, Int) -> Int -> Int
        countAt (i, j) acc = acc + xmasAround xs i j

xmasCount2 :: XmasSearch -> Int
xmasCount2 xs = foldr countAt 0 (grid xs)
    where
        countAt :: (Int, Int) -> Int -> Int
        countAt (i, j) acc = acc + masXAt xs i j
safeAt :: XmasSearch -> Int -> Int -> Maybe Char
safeAt xs i j = do
    firstLength <- length <$> listToMaybe xs
    let canIndex = 0 <= j && j < firstLength && 0 <= i && i < length xs
    if canIndex then Just $ (xs!!i)!!j else Nothing

xmasAround :: XmasSearch -> Int -> Int -> Int
xmasAround xs i j = horizontal xs i j + vertical xs i j + diagonal1 xs i j + diagonal2 xs i j

alongLine :: (Int, Int, Int, Int, Int, Int) -> XmasSearch -> Int -> Int -> Int
alongLine (i1, i2, i3, j1, j2, j3) xs i j = case (
    sequence [safeAt xs i j, safeAt xs (i + i1) (j + j1), safeAt xs (i + i2) (j + j2), safeAt xs (i + i3) (j + j3)], 
    sequence [safeAt xs i j, safeAt xs (i - i1) (j - j1), safeAt xs (i - i2) (j - j2), safeAt xs (i - i3) (j - j3)]) of
        (Just "XMAS", Just "XMAS") -> 2
        (Just "XMAS", _) -> 1
        (_, Just "XMAS") -> 1
        _ -> 0

vertical = alongLine (1, 2, 3, 0, 0, 0)
horizontal = alongLine (0, 0, 0, 1, 2, 3)
diagonal1 = alongLine (1, 2, 3, -1, -2, -3)
diagonal2 = alongLine (1, 2, 3, 1, 2, 3)

masXAt :: XmasSearch -> Int -> Int -> Int
masXAt xs i j = case sequence [safeAt xs i j, safeAt xs (i - 1) (j - 1), safeAt xs (i + 1) (j + 1), safeAt xs (i + 1) (j - 1), safeAt xs (i - 1) (j + 1)] of
    Just "AMSMS" -> 1
    Just "ASMSM" -> 1
    Just "ASMMS" -> 1
    Just "AMSSM" -> 1
    _ -> 0