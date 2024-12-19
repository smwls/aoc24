module Day6 where

import AocTypes
import Data.Maybe (isJust, fromMaybe, isJust)
import Data.List (find, transpose, sort)
import Data.Set (empty, union, fromList, Set, singleton, size)
import qualified Data.Set as Set
day6 :: RunFunction
day6 ab input = do
    parsed <- parseInput input
    return $ case ab of
        A -> day6a parsed
        B -> day6b parsed

day6a (input, pt) = show $ size $ fst $ snd $ iterateUntilLeft (input, pt)
day6b (input, pt) = show $ countAllLoopingObstacles (input, pt)--do
    -- newGrid <- gridWithObstacle input (4, 5)
    -- iterateUntilLoopOrLeft (newGrid, pt)
parseInput :: String -> Maybe (Grid, Point)
parseInput s =
    let
        ixs :: [String] -> [[Int]]
        ixs ls = ((fst <$>) . filter ((=='#') . snd) . zip [0..] <$> ls)
        rows = uncurry Row <$> zip [0..] ((ixs . lines) s)
        cols = uncurry Col <$> zip [0..] ((ixs . transpose . lines) s)

        findPoint :: [String] -> Maybe Point
        findPoint s' = case filter ((/= []) . snd) $ zip [0..] (filter (isJust . snd) . zip [0..] <$> (map . map) getDir s') of
                [(x, [(y, Just z)])] -> Just $ Point x y z
                _ -> Nothing
        lss = lines s
    in do
            pt <- findPoint lss
            return $ (Grid (length lss - 1) rows cols, pt)
getDir :: Char -> Maybe Direction
getDir c = case c of
    '<' -> Just L
    '>' -> Just R
    'v' -> Just D
    '^' -> Just U
    _ -> Nothing

data Row = Row Int [Int] deriving (Eq, Show)
data Col = Col Int [Int] deriving (Eq, Show)
data Direction = U | D | L | R deriving (Eq, Ord, Show)
data Point = Point Int Int Direction deriving (Eq, Ord, Show)
data Grid = Grid Int [Row] [Col] deriving (Show)

update :: Grid -> Point -> Maybe Point
update (Grid bounds rows cols) (Point r c dir)
    | (c, dir) == (0, L) = Nothing
    | (c, dir) == (bounds, R) = Nothing
    | (r, dir) == (0, U) = Nothing
    | (r, dir) == (bounds, D) = Nothing
    | otherwise = Just rest
    where rest =
            case sequenceTriple $ case dir of
                L -> (Just r, (+1) <$> (maxFor (<c) . (\(Row _ rs) -> rs) =<< find (\(Row r' _) -> r' == r) rows), Just U)
                R -> (Just r, (+(-1)) <$> (minFor (>c) . (\(Row _ rs) -> rs) =<< find (\(Row r' _) -> r' == r) rows), Just D)
                U -> ((+1) <$> (maxFor (<r) . (\(Col _ cs) -> cs) =<< find (\(Col c' _) -> c' == c) cols), Just c, Just R)
                D -> ((+(-1)) <$> (minFor (>r) . (\(Col _ cs) -> cs) =<< find (\(Col c' _) -> c' == c) cols), Just c, Just L)
            of
                Just (row, col, d) -> Point row col d
                Nothing -> case dir of
                    L -> Point r 0 dir
                    R -> Point r bounds dir
                    U -> Point 0 c dir
                    D -> Point bounds c dir

maxFor :: (Ord a) => (a -> Bool) -> [a] -> Maybe a
maxFor _ [] = Nothing
maxFor p as =
    let pas = filter p as
    in case pas of
        [] -> Nothing
        _ -> Just $ foldr max (head pas) pas



minFor :: (Ord a) => (a -> Bool) -> [a] -> Maybe a
minFor _ [] = Nothing
minFor p as =
    let pas = filter p as
    in case pas of
        [] -> Nothing
        _ -> Just $ foldr min (head pas) pas

getSpan :: Point -> Point -> (Set (Int, Int), Set Point)
getSpan p@(Point i j d) q@(Point k l _) = case (i == k, j == l) of
    (True, True) -> (singleton (k, l), fromList [p, q])
    (True, False) -> let f = fromList $ map (i,) [(min j l)..(max j l)] in (f, Set.map (\(x,y) -> Point x y d) f `union` singleton q)
    (False, True) -> let f = fromList $ map (, j) [(min i k)..(max i k)] in (f, Set.map (\(x,y) -> Point x y d) f `union` singleton q)
    (False, False) -> (empty, empty)

iterateAndAddSpan :: Grid -> Point -> (Set (Int, Int), Set Point) -> Maybe (Point, (Set (Int, Int), Set Point))
iterateAndAddSpan g p (si, sp) = do
    q <- update g p
    let (si', sp') = getSpan p q
    return (q, (si `union` si', sp `union` sp'))

iterateUntilLeft :: (Grid, Point) -> (Point, (Set (Int, Int), Set Point))
iterateUntilLeft (g, p) = go (p, (empty, empty))
    where
        go (p', s) = case iterateAndAddSpan g p' s of
            Just (q, s') -> go (q, s')
            Nothing -> (p', s)

iterateUntilLoopOrLeft :: (Grid, Point) -> Maybe (Point, (Set (Int, Int), Set Point))
iterateUntilLoopOrLeft (g, p) = go (p, (empty, singleton p))
    where
        go (p', s@(_, sp)) = case iterateAndAddSpan g p' s of
            Just (q, s'@(_, sp')) -> if sp == sp' then Just (q, s') else go (q, s')
            Nothing -> Nothing

gridWithObstacle :: Grid -> (Int, Int) -> Maybe Grid
gridWithObstacle (Grid b rows cols) (r, c) = do
        let newRows = (\(Row r' rs) -> if r' /= r then Row r' rs else if c `elem` rs then Row r rs else Row r $ sort (c:rs)) <$> rows
        let newCols = (\(Col c' cs) -> if c' /= c then Col c' cs else if r `elem` cs then Col c cs else Col c $ sort (r:cs)) <$> cols
        if newRows == rows || newCols == cols then Nothing else return $ Grid b newRows newCols

-- countAllLoopingObstacles :: (Grid, Point) -> Int
countAllLoopingObstacles (g@(Grid b _ _), p@(Point i j _)) =
    let pairs = filter (\(x, y) -> x /= i || y /= j) $ [(x, y) | x <- [0..b], y <- [0..b]]
        grids = fromMaybe [] $ sequence $ filter isJust $ gridWithObstacle g <$> pairs
        loops = fromMaybe [] $ sequence $ filter isJust $ (\gr -> iterateUntilLoopOrLeft (gr, p)) <$> grids
    in
        length loops
sequenceTriple :: (Maybe a, Maybe b, Maybe c) -> Maybe (a, b, c)
sequenceTriple x = case x of
    (Just i, Just j, Just k) -> Just (i, j, k)
    _ -> Nothing

iterateNTimes :: (Grid, Point) -> Int -> ([Point], (Set (Int, Int), Set Point))
iterateNTimes (g, p) = go ([p], (empty, singleton p))
    where
        go r 0 = r
        go ([], s) _ = ([], s)
        go (p':ps, s) n = case iterateAndAddSpan g p' s of
            Just (q, s') -> go (q:(p':ps), s') (n - 1)
            Nothing -> (p':ps, s)
