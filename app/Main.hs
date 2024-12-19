module Main where
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)
import Control.Exception (IOException, catch)
import Day1 (day1)
import Day2 (day2)
import Day3 (day3)
import Day4 (day4)
import Day5 (day5)
import Day6 (day6)
import Day7 (day7)
import AocTypes
main :: IO ()
main = do 
    args <- getArgs
    let maybeOpts = parseOptions args
    result <- case maybeOpts of 
        Just opts -> runAoc opts
        Nothing -> return "invalid options"
    print result

parseDay :: String -> Maybe Day
parseDay d = do
    x <- readMaybe d
    if x > 0 && x <= 25 then Just $ Day x else Nothing

parseAB :: String -> Maybe AB
parseAB "a" = Just A
parseAB "b" = Just B
parseAB _ = Nothing

parseMode :: String -> Maybe Mode
parseMode "t" = Just Test
parseMode "r" = Just Run
parseMode _ = Nothing

readTextFile :: Day -> Mode -> IO (Maybe InputData)
readTextFile (Day day) mode = safeReadFile $ "data/" ++ show day ++ case mode of Run -> ""; Test -> "test"
    where safeReadFile p = (Just <$> readFile p) `catch` ((\_ -> pure Nothing) :: IOException -> IO (Maybe InputData))
parseOptions :: [String] -> Maybe Options
parseOptions (d:a:os) = do
    day <- parseDay d
    ab <- parseAB a
    mode <- case listToMaybe os of
        Just o -> parseMode o
        Nothing -> Just Run
    Just $ Options day ab mode
parseOptions _ = Nothing

getAocFunction :: Day -> RunFunction
getAocFunction (Day day) = case day of 
    1 -> day1
    2 -> day2
    3 -> day3
    4 -> day4
    5 -> day5
    6 -> day6
    7 -> day7
    _ -> \_ _ -> Just "not yet"
runAoc :: Options -> IO String
runAoc (Options day ab mode) = do
    inputData <- readTextFile day mode
    return $ case inputData of
        Just input -> case getAocFunction day ab input of (Just result) -> result; Nothing -> "failed"
        Nothing -> "Input file not present"