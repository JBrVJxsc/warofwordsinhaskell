{-# LANGUAGE OverloadedStrings #-}

import Common
import Filter
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "Enter the key word:"
    input <- getContents
    mapM_ query (lines input)

query :: String -> IO ()
query string = do
  let word = toLowerCase string
  putStrLn ""
  wordFilter word 0
--  wordFilter word 1
--  wordFilter word 2
--  wordFilter word 3
--  wordFilter word 4
--  wordFilter word 5
--  wordFilter word 6
--  wordFilter word 7
  wordFilter word 8
  putStrLn ""