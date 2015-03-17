module Common (
    subString,
    replace,
    duplicate,
    toLowerCase,
    toUpperCase
  )where

import Data.Char

subString :: String -> Int -> Int -> String
subString [] _ _ = []
subString string 0 len = take len string
subString string start len =
  take len (drop start string)

replace :: String -> Int -> Int -> String -> String
replace string start len str =
  (take start string) ++ str ++ (drop (start + len) string)

duplicate :: Char -> Int -> String
duplicate _ 0 = []
duplicate char times =
  char : duplicate char (times - 1)

toLowerCase :: String -> String
toLowerCase [] = []
toLowerCase (x : xs) = toLower x : (toLowerCase xs)

toUpperCase :: String -> String
toUpperCase [] = []
toUpperCase (x : xs) = toUpper x : (toLowerCase xs)