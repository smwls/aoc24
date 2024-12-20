module Day3 where
import AocTypes
import Text.Read (readMaybe)

day3 :: RunFunction Commands
day3 = RunFunction parseInput day3a day3b

day3a :: SolutionInstance Commands
day3a input = show $ sum $ uncurry (*) <$> parseMuls input

day3b :: SolutionInstance Commands
day3b input = show $ sum $ uncurry (*) <$> parseToggledMuls input

type Commands = String
data Toggle = Do | Dont
parseInput :: String -> Maybe Commands
parseInput = Just
parseSingleMul :: Commands -> (Maybe (Int, Int), Commands)
parseSingleMul "" = (Nothing, "")
parseSingleMul (x:"") = (Nothing, "")
parseSingleMul xs@(x:xss) = case splitAt 4 xs of
    ("mul(", rest) -> 
        let (my, rs) = span (/=',') rest in 
            case readMaybe my of
                Just y -> 
                    let (num, rest') = span (/=')') (drop 1 rs) in 
                        case (readMaybe num, splitAt 1 rest') of
                            (Just z, (")", rs')) -> (Just (y, z), rs')
                            (_, (_, rs')) -> (Nothing, xss)
                Nothing -> (Nothing, xss)
    (_, "") -> (Nothing, "")
    (_, rest) -> (Nothing, xss)
parseMuls :: Commands -> [(Int, Int)]
parseMuls c = fst $ go [] c
    where 
        go :: [(Int, Int)] -> Commands -> ([(Int, Int)], Commands)
        go ps "" = (ps, "")
        go ps x = case parseSingleMul x of
            (Just (a, b), cs) -> go ((a, b):ps) cs
            (Nothing, cs) -> go ps cs

parseSingleToggle :: Commands -> (Maybe Toggle, Commands)
parseSingleToggle "" = (Nothing, "")
parseSingleToggle ('d':'o':'n':'\'':'t':'(':')':xs) = (Just Dont, xs)
parseSingleToggle ('d':'o':'(':')':xs) = (Just Do, xs)
parseSingleToggle (x:xs) = (Nothing, xs)

parseToggledMuls :: Commands -> [(Int, Int)]
parseToggledMuls c = fst $ go [] Do c
    where
        go :: [(Int, Int)] -> Toggle -> Commands -> ([(Int, Int)], Commands)
        go ps _ "" = (ps, "")
        go ps toggle x = case (toggle, parseSingleToggle x) of
            (Do, (Just Dont, xs)) -> go ps Dont xs
            (Do, (Nothing, _)) -> goMul ps Do x
            (Dont, (Just Do, xs)) -> goMul ps Do xs
            (Dont, (Nothing, xs)) -> go ps Dont xs
            _ -> goMul ps toggle x

        goMul :: [(Int, Int)] -> Toggle -> Commands -> ([(Int, Int)], Commands)
        goMul ps _ "" = (ps, "")
        goMul ps toggle x = case (toggle, parseSingleMul x) of
            (Do, (Just (a, b), cs)) -> go ((a, b):ps) toggle cs
            (_, (_, cs)) -> go ps toggle cs



