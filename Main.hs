-- Main.hs
--

module Main where 
import System.Environment

main :: IO ()
main = getArgs >>= print . greetings   

greetings :: [String] -> String
greetings s = "Hello! " ++ who s

who :: [String] -> String
who [] = "unknown"
who s = head s
