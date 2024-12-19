module Day7 where

import AocTypes

day7 :: RunFunction
day7 ab input = do
    parsed <- parseInput input
    return $ case ab of
        A -> day7a parsed
        B -> day7b parsed

day7a input = show $ sum $ (\(Equation test _) -> test) <$> filter isEquationSolvable1 input
day7b input = show $ sum $ (\(Equation test _) -> test) <$> filter isEquationSolvable2 input

parseInput :: String -> Maybe [Equation]
parseInput s = Just $ parseEquation <$> lines s

data Equation = Equation Int [Int] deriving (Eq, Show)

parseEquation :: String -> Equation
parseEquation s =
    let target = read $ takeWhile (/=':') s
        nums = read @Int <$> lines ((\x -> if x == ' ' then '\n' else x) <$> drop 1 (dropWhile (/=' ') s))
    in Equation target nums

isEquationSolvable1 :: Equation -> Bool
isEquationSolvable1 equation = case equation of
    (Equation _ []) -> False;
    (Equation target [num]) -> target == num;
    (Equation target (num1:nums)) ->
        isEquationSolvable1 (Equation (target - num1) nums)
            || target `rem` num1 == 0 && isEquationSolvable1 (Equation (target `div` num1) nums)

isEquationSolvable2 :: Equation -> Bool
isEquationSolvable2 equation = case equation of
    (Equation _ []) -> False;
    (Equation target [num]) -> target == num;
    (Equation target (num1:(num2:ns))) ->
        isEquationSolvable2 (Equation target ((num1 + num2):ns))
            || isEquationSolvable2 (Equation target (read @Int (show num1 ++ show num2):ns))
            || isEquationSolvable2 (Equation target ((num1 * num2):ns))