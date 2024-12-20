module Day8 where

import AocTypes
import Data.List (nub, sort)
import Data.Maybe (isJust, fromMaybe)
import Data.Map (Map, empty, fromList, singleton, findWithDefault, member, insert, keys)

type Antennae = Map Char [(Int, Int)]

data Antenna = Antenna Int Int Char deriving (Eq, Show)

day8 :: RunFunction (Antennae, Int)
day8 = RunFunction parseInput day8a day8b

day8a (antennae, gs) = show $ length $ sort $ nub $ concat $ (\k -> findAntinodes gs (findWithDefault [] k antennae)) <$> (keys antennae)
day8b (antennae, gs) = show $ length $ sort $ nub $ concat $ (\k -> findNewAntinodes gs (findWithDefault [] k antennae)) <$> (keys antennae)

parseInput :: String -> Maybe (Antennae, Int)
parseInput s = 
    let ls = lines s
        gs = length ls - 1
        coords = [(x, y) | x <- [0..gs], y <- [0..gs]]
        antennae = map (\((x, y), c) -> Antenna x y (fromMaybe '.' c)) $ filter (isJust . snd) $ zip coords $ concat $ (map . map) (\x -> if x == '.' then Nothing else Just x) $ ls
    in
        Just $ (foldr (\(Antenna x y c) ms -> let coords = findWithDefault [] c ms in insert c ((x, y):coords) ms) empty antennae, gs)

distAlongLine :: (Int, Int) -> (Int, Int) -> (Int, Int)
distAlongLine (i1, j1) (i2, j2) = 
    let (di, dj) = (i2 - i1, j2 - j1) 
    in (i2 + round (fromIntegral di) , j2 + round (fromIntegral dj))

findAntinodesForPair :: Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
findAntinodesForPair gridSize ((i1, j1), (i2, j2)) = let
    candidates = [distAlongLine (i1, j1) (i2, j2), distAlongLine (i2, j2) (i1, j1)]
    in filter (\(x, y) -> 0 <= x && x <= gridSize && 0 <= y && y <= gridSize) candidates

findNewAntinodesForPair :: Int -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
findNewAntinodesForPair gridSize ((i1, j1), (i2, j2)) = let
    candidates = [(i, j) | i <- [0..gridSize], j <- [0..gridSize], (i1 - i2)*(j - j1) == (i - i1)*(j1 - j2)]
    in filter (\(x, y) -> 0 <= x && x <= gridSize && 0 <= y && y <= gridSize) candidates

findAntinodes gs [] = []
findAntinodes gs xs = let
    pairs :: [((Int, Int), (Int, Int))]
    pairs = [((i, j), (k, l)) | (i, j) <- xs, (k, l) <- xs, i /= k && j /= l]
    in nub $ concat $ (findAntinodesForPair gs) <$> pairs

findNewAntinodes gs [] = []
findNewAntinodes gs xs = let
    pairs :: [((Int, Int), (Int, Int))]
    pairs = [((i, j), (k, l)) | (i, j) <- xs, (k, l) <- xs, i /= k && j /= l]
    in nub $ concat $ (findNewAntinodesForPair gs) <$> pairs

