module AocTypes where

newtype Day = Day Int
data AB = A | B
data Mode = Test | Run
data Options = Options Day AB Mode
type InputData = String
type Result = String
type Parser a = (String -> Maybe a)
data RunFunction a = RunFunction (Parser a) (SolutionInstance a) (SolutionInstance a)
type SolutionInstance a = (a -> String)