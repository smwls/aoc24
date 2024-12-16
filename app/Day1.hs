module Day1 where
import AocTypes
import Text.Read (readMaybe)
import Data.List (sort)
import Data.Map (Map, empty, findWithDefault)
import Data.Map.Strict (insertWith)

data ListPair = ListPair [Int] [Int]
day1 :: RunFunction
day1 ab input = do
    parsed <- parseInput input
    return $ case ab of
        A -> day1a parsed
        B -> day1b parsed

day1a :: SolutionInstance ListPair
day1a (ListPair a b) = show $ sum $ dists <$> zip (sort a) (sort b)
    where
        dists :: (Int, Int) -> Int
        dists (x, y) = abs (x - y)

day1b :: SolutionInstance ListPair
day1b (ListPair a b) =
        show $ sum $ scores a
    where
        counts = getCounts b

        getCounts :: [Int] -> Map Int Int
        getCounts = foldr updateMap empty

        updateMap :: Int -> Map Int Int -> Map Int Int
        updateMap k = insertWith (+) k 1

        scoreFor :: Map Int Int -> Int -> Int
        scoreFor m x = x * findWithDefault 0 x m

        scores :: [Int] -> [Int]
        scores xs = scoreFor counts <$> xs

parseInput :: String -> Maybe ListPair
parseInput input = do
    ls <- sequence $ pairList
    return $ case foldr f (ListPair [] []) ls of
        ListPair left right -> ListPair (reverse left) (reverse right)
    where
        pairList :: [Maybe (Int, Int)]
        pairList = wordListToPair . words <$> lines input

        f :: (Int, Int) -> ListPair -> ListPair
        f (x, y) (ListPair left right) = ListPair (x:left) (y:right)

        wordListToPair :: [String] -> Maybe (Int, Int)
        wordListToPair ws = case ws of
            (w1:(w2:_)) -> do
                n1 <- readMaybe w1
                n2 <- readMaybe w2
                Just (n1, n2)
            _ -> Nothing

