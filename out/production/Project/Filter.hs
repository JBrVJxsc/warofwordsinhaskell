module Filter (
    wordFilter
  ) where

import DataBase
import Module

wordFilter :: String -> Int -> IO ()
wordFilter word index =
  if (index == 0)
    then do
      dataSet <- getDataSetFilter0 word
      putStr $ extractWordMeaning dataSet
  else if (index == 1)
    then do
      dataSet <- getDataSetFilter1 word
      putStr $ extractWordMeaning dataSet
  else if (index == 2)
    then do
      dataSet <- getDataSetFilter2 word
      putStr $ extractWordMeaning dataSet
  else if (index == 3)
    then do
      dataSet <- getDataSetFilter3 word
      putStr $ extractWordMeaning dataSet
  else if (index == 4)
    then do
      dataSet <- getDataSetFilter4 word
      putStr $ extractWordMeaningList dataSet
  else if (index == 5)
    then do
      dataSet <- getDataSetFilter5 word
      putStr $ extractWordMeaningList dataSet
  else if (index == 6)
    then do
      dataSet <- getDataSetFilter6 word
      putStr $ extractWordMeaningListList dataSet
  else if (index == 7)
    then do
      dataSet <- getDataSetFilter7 word
      putStr $ extractWordMeaningListList dataSet
  else if (index == 8)
    then do
      dataSet <- getDataSetFilter8 word
      putStr $ extractWordMeaningListList dataSet
  else do
    putStr "END"