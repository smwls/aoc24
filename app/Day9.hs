module Day9 where

import AocTypes
import Data.List (sortBy, find)
import Data.Maybe (isJust)
import Control.Monad (join)
type DiskMap = [Block]
data Block = Free Int | File Int Int deriving (Eq, Show)
day9 :: RunFunction DiskMap
day9 = RunFunction parseInput day9a day9b

day9a input = show $ checksum $ moveFiles input
day9b input = show $ checksum $ moveFiles2 input

parseInput :: String -> Maybe [Block]
parseInput s = filter (\case (Free 0) -> False; (File 0 _) -> False; _ -> True) <$> go 0 s where
    go _ "" = Just []
    go n (s':"") = Just [File (read @Int [s']) n]
    go n (s1:s2:ss) = do
        res <- go (n + 1) ss
        return $ File (read @Int [s1]) n:Free (read @Int [s2]):res

display :: [Block] -> String
display = foldr
      (\ b
         -> (++)
              (case b of
                 Free size -> replicate size '.'
                 File size i -> replicate size (head $ show i)))
      ""
checksum :: [Block] -> Int
checksum bs = fst $ foldr go (0, 0) $ reverse bs
    where go x (acc, index) = case x of
            Free size -> (acc, index + size)
            File size i -> (acc + sum ((*i) <$> [index..(index + size - 1)]), index + size)

isFree :: Block -> Bool
isFree x = case x of (Free _) -> True; _ -> False
moveFiles :: [Block] -> [Block]
moveFiles [] = []
moveFiles bs = let
    fileSum = sum ((\case (Free s) -> s; (File s _) -> s) <$> filter (not.isFree) bs)
    go :: [Block] -> [Block] -> [Block] -> Int -> [Block]
    go [] _ n _ = n
    go _ [] n _ = n
    go ((Free _):_) _ n _ = n
    go ff@(f@(File fileSize i):fs) (b:bs') n m = case (b, fileSum <= m) of
        (_, True) -> n
        (File size _, _) -> if m + size <= fileSum then go ff bs' (b:n) (m + size) else f:n
        (Free blockSize, _) ->
            case compare blockSize fileSize of
                LT -> let
                    fInBlock = File blockSize i
                    leftoverF = File (fileSize - blockSize) i
                    in if m + blockSize <= fileSum then go (leftoverF:fs) bs' (fInBlock:n) (m + blockSize) else n
                EQ -> if m + blockSize <= fileSum then go fs bs' (f:n) (m + blockSize) else n
                GT -> let newB = Free (blockSize - fileSize) in if m + fileSize <= fileSum then go fs (newB:bs') (f:n) (m + fileSize) else n
    in reverse $ go (reverse (filter (not . isFree) bs)) bs [] 0

getSize :: Block -> Int
getSize b = case b of Free size -> size; File size _ -> size;

reverseComparator :: (Int, Block) -> (Int, Block) -> Ordering
reverseComparator (_, x) (_, y) = case (x, y) of (File _ id1, File _ id2) -> compare id1 id2; _ -> EQ
reversedFiles :: [Block] -> [(Int, Block)]
reversedFiles bs = filter (not . isFree . snd) $ sortBy reverseComparator (getBlocksWithIndices bs)
getBlocksWithIndices :: [Block] -> [(Int, Block)]
getBlocksWithIndices = go 0 where
    go :: Int -> [Block] -> [(Int, Block)]
    go _ [] = []
    go n [b] = [(n, b)]
    go n (b:bs') = (n, b):go (n + getSize b) bs'

getFreeForFile :: (Int, Block) -> (Int, Block) -> Maybe (Block, [(Int, Block)])
getFreeForFile (_, Free _) _ = Nothing
getFreeForFile (_, File _ _) (_, File _ _) = Nothing
getFreeForFile (i, f) (j, b)
    | i < j = Nothing
    | getSize b == getSize f = Just (b, [(j, f)])
    | getSize b > getSize f = Just (b, [(j, f), (j + getSize f, Free (getSize b - getSize f))])
    | otherwise = Nothing
moveBlockInList :: (Int, Block) -> [(Int, Block)] -> [(Int, Block)]
moveBlockInList b bs' = let
    firstFree = join $ find isJust (getFreeForFile b <$> bs')
    newBlocks = case firstFree of
        Nothing -> bs'
        Just (oldB, fbs) -> takeWhile ( (/=oldB) . snd) bs' ++ fbs ++ map (\(i, c) -> if c == snd b && (not . isFree) c then (i, Free (getSize c)) else (i, c)) (drop 1 $ dropWhile ((/=oldB).snd) bs')
    in newBlocks
moveFiles2 :: [Block] -> [Block]
moveFiles2 [] = []
moveFiles2 bs = snd <$> foldr moveBlockInList (getBlocksWithIndices bs) (reversedFiles bs)