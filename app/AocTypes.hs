module AocTypes where

newtype Day = Day Int
data AB = A | B
data Mode = Test | Run
data Options = Options Day AB Mode
type InputData = String
type Result = String
type RunFunction = AB -> InputData -> Maybe Result
type SolutionInstance a = (a -> String)