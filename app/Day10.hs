module Day10 where

import AocTypes
import Data.List (nub)
type TopographicMap = [[Int]]

day10 = RunFunction parseInput day10a day10b

day10a input = show $ sum $ ((length . findTrailCounts input) <$> (startingPoints input))
day10b input = show $ length $ (concat ((findDistinctTrailCounts input) <$> (startingPoints input)))

parseInput :: String -> Maybe TopographicMap
parseInput s = Just $ (map (read @Int . return)) <$> lines s

getAt :: (Int, Int) -> TopographicMap -> Maybe Int
getAt (i, j) tm = 
    let bounds = length tm 
    in if i < bounds && j < bounds then Just $ (tm!!i)!!j else Nothing

getAllNeighbours :: (Int, Int) -> TopographicMap -> [(Int, Int)]
getAllNeighbours (i, j) tm =
    let bounds = length tm
    in [(k, l) | k <- [0..bounds - 1], l <- [0..bounds - 1], abs (k - i) + abs (l - j) == 1]

getRisingNeighbours :: (Int, Int) -> TopographicMap -> [(Int, Int)]
getRisingNeighbours (i, j) tm = 
    let bounds = length tm
        atThis = getAt (i, j) tm
    in case atThis of 
        Nothing -> []; 
        Just this -> [n | n <- getAllNeighbours (i, j) tm, getAt n tm == Just (this + 1)]

startingPoints :: TopographicMap -> [(Int, Int)]
startingPoints tm = 
    let bounds = length tm 
    in [(i, j) | i <- [0..bounds - 1], j <- [0..bounds - 1], getAt (i, j) tm == Just 0]

findTrailCounts :: TopographicMap -> (Int, Int) -> [(Int,Int)]
findTrailCounts tm (i, j) = 
    let bounds = length tm
        atThis = getAt (i, j) tm
    in case atThis of
        Nothing -> []
        Just 9 -> [(i, j)]
        Just _ -> nub (concat ((findTrailCounts tm) <$> (getRisingNeighbours (i, j) tm)))

findDistinctTrailCounts :: TopographicMap -> (Int, Int) -> [(Int,Int)]
findDistinctTrailCounts tm (i, j) = 
    let bounds = length tm
        atThis = getAt (i, j) tm
    in case atThis of
        Nothing -> []
        Just 9 -> [(i, j)]
        Just _ -> (concat ((findDistinctTrailCounts tm) <$> (getRisingNeighbours (i, j) tm)))




