{-# LANGUAGE OverloadedStrings #-}

module DataBase (
    getMaxWordLength,
    getWordLengthList,
    getDataSetFilter0,
    getDataSetFilter1,
    getDataSetFilter2,
    getDataSetFilter3,
    getDataSetFilter4,
    getDataSetFilter5,
    getDataSetFilter6,
    getDataSetFilter7,
    getDataSetFilter8
  ) where

import Common
import Database.SQLite.Simple
import Module

getDBURL :: String
getDBURL = "dic.db"

getMaxWordLength :: IO [MaxWordLengthField]
getMaxWordLength = do
  conn <- open getDBURL
  dataSet <- query_ conn "SELECT MAX(LENGTH(WORD)) FROM DICTIONARY"
  close conn
  return dataSet

getWordLengthList :: IO [Int]
getWordLengthList = do
  conn <- open getDBURL
  dataSet <- query_ conn "SELECT DISTINCT LENGTH(WORD) FROM DICTIONARY"
  close conn
  return (extractWordLengthList dataSet)

getDataSetFilter0 :: String -> IO [WordMeaningField]
getDataSetFilter0 word = do
  conn <- open getDBURL
  dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD = :word" [":word" := word]
  close conn
  return dataSet

getDataSetFilter1 :: String -> IO [WordMeaningField]
getDataSetFilter1 word = do
  conn <- open getDBURL
  dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD LIKE :word AND LENGTH(WORD) > :len" [":word" := word ++ "%", ":len" := length word]
  close conn
  return dataSet

getDataSetFilter2 :: String -> IO [WordMeaningField]
getDataSetFilter2 word = do
  conn <- open getDBURL
  dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD LIKE :word AND LENGTH(WORD) > :len" [":word" := "%" ++ word, ":len" := length word]
  close conn
  return dataSet

getDataSetFilter3 :: String -> IO [WordMeaningField]
getDataSetFilter3 word = do
  conn <- open getDBURL
  dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD LIKE :word AND LENGTH(WORD) > :len" [":word" := "%" ++ word ++ "%", ":len" := length word]
  close conn
  return dataSet

getDataSetFilter4 :: String -> IO [[WordMeaningField]]
getDataSetFilter4 word =
  if (length word) < 3
    then do
    return []
  else do
    conn <- open getDBURL
    dataSet <- helper word (length word) conn
    close conn
    return dataSet
  where
    helper :: String -> Int -> Connection -> IO [[WordMeaningField]]
    helper [] _ _ = do
      return []
    helper word len conn =
      if ((length (tail word) * 2) < len)
        then do
          return []
      else do
        let t = tail word
        dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD = :word" [":word" := t] :: IO [WordMeaningField]
        remain <- helper t len conn
        return (dataSet : remain)

getDataSetFilter5 :: String -> IO [[WordMeaningField]]
getDataSetFilter5 word =
  if (length word) < 3
    then do
      return []
  else do
    conn <- open getDBURL
    dataSet <- helper word (length word) (length word - 1) conn
    close conn
    return dataSet
  where
    helper :: String -> Int -> Int -> Connection -> IO [[WordMeaningField]]
    helper word len num conn =
      if (num == 0 || num * 2 < len)
        then do
          return []
      else do
        let t = take num word
        dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD = :word" [":word" := t] :: IO [WordMeaningField]
        remain <- helper word len (num - 1) conn
        return (dataSet : remain)

getDataSetFilter6 :: String -> IO [[[WordMeaningField]]]
getDataSetFilter6 word =
  if (length word) < 4
    then do
      return []
  else do
    wordLengthList <- getWordLengthList
    conn <- open getDBURL
    let wordLen = (length word) - 2
    let t = subString word 1 wordLen
    dataSet <- helper t wordLen wordLen wordLengthList conn
    close conn
    return dataSet
  where
    helper :: String -> Int -> Int -> [Int] -> Connection -> IO [[[WordMeaningField]]]
    helper word wordLen len lenList conn =
      if (len * 2 < wordLen)
        then do
          return []
      else if (elem len lenList)
        then do
        dataSet <- subHelper word wordLen len 0 conn
        remain <- helper word wordLen (len - 1) lenList conn
        return (dataSet : remain)
      else do
        remain <- helper word wordLen (len - 1) lenList conn
        return remain
      where
      subHelper :: String -> Int -> Int -> Int -> Connection -> IO [[WordMeaningField]]
      subHelper word wordLen len start conn =
        if ((wordLen - start) < len)
          then do
            return []
        else do
          let t = subString word start len
          dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD = :word" [":word" := t] :: IO [WordMeaningField]
          remain <- subHelper word wordLen len (start + 1) conn
          return (dataSet : remain)

getDataSetFilter7 :: String -> IO [[[WordMeaningField]]]
getDataSetFilter7 word = do
  conn <- open getDBURL
  dataSet <- helper word (length word) 1 conn
  close conn
  return dataSet
  where
  helper :: String -> Int -> Int -> Connection -> IO [[[WordMeaningField]]]
  helper word wordLen wildcardLen conn =
    if (wildcardLen * 2 > wordLen)
      then do
        return []
    else do
      let str = duplicate '_' wildcardLen
      dataSet <- subHelper word wordLen wildcardLen 0 str conn
      remain <- helper word wordLen (wildcardLen + 1) conn
      return (dataSet : remain)
    where
    subHelper :: String -> Int -> Int -> Int -> String -> Connection -> IO [[WordMeaningField]]
    subHelper word wordLen wildcardLen start str conn =
      if ((wordLen - start) < wildcardLen)
        then do
          return []
      else do
        let t = replace word start wildcardLen str
        dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD LIKE :word AND WORD <> :key" [":word" := t, ":key" := word] :: IO [WordMeaningField]
        remain <- subHelper word wordLen wildcardLen (start + 1) str conn
        return (dataSet : remain)

getDataSetFilter8 :: String -> IO [[[WordMeaningField]]]
getDataSetFilter8 word = do
  conn <- open getDBURL
  dataSet <- helper word (length word) 1 conn
  close conn
  return dataSet
  where
  helper :: String -> Int -> Int -> Connection -> IO [[[WordMeaningField]]]
  helper word wordLen wildcardLen conn =
    if (wildcardLen * 2 > wordLen)
      then do
        return []
    else do
      let str = duplicate '%' wildcardLen
      dataSet <- subHelper word wordLen wildcardLen 0 str conn
      remain <- helper word wordLen (wildcardLen + 1) conn
      return (dataSet : remain)
    where
    subHelper :: String -> Int -> Int -> Int -> String -> Connection -> IO [[WordMeaningField]]
    subHelper word wordLen wildcardLen start str conn =
      if ((wordLen - start) < wildcardLen)
        then do
          return []
      else do
        let t = replace word start wildcardLen str
        dataSet <- queryNamed conn "SELECT WORD, MEANING FROM DICTIONARY WHERE WORD LIKE :word AND WORD <> :key" [":word" := t, ":key" := word] :: IO [WordMeaningField]
        remain <- subHelper word wordLen wildcardLen (start + 1) str conn
        return (dataSet : remain)