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

getAndRunFunction :: AB -> Day -> String -> Maybe String
getAndRunFunction ab (Day day) input = case day of 
            1 -> f day1
            2 -> f day2
            3 -> f day3
            4 -> f day4
            5 -> f day5
            6 -> f day6
            7 -> f day7
            _ -> Nothing
        where 
            f :: RunFunction a -> Maybe String
            f = runRunFunction ab input

runRunFunction :: AB -> String -> RunFunction a -> Maybe String
runRunFunction ab s (RunFunction parser dayA dayB) = do
    parsed <- parser s
    return $ case ab of
        A -> dayA parsed
        B -> dayB parsed
runAoc :: Options -> IO String
runAoc (Options day ab mode) = do
    inputData <- readTextFile day mode
    return $ case inputData of
        Just input -> 
            case getAndRunFunction ab day input of
                (Just result) -> result 
                Nothing -> "failed"
        Nothing -> "Input file not present"