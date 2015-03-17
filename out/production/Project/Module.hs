module Module (
    WordLengthListField,
    MaxWordLengthField,
    WordMeaningField,
    extractWordMeaning,
    extractWordMeaningList,
    extractWordMeaningListList,
    extractWordLengthList
  )where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data WordLengthListField = WordLength Int deriving (Show)
data MaxWordLengthField = MaxWordLength Int deriving (Show)
data WordMeaningField = WordMeaning String String deriving (Show)

instance FromRow WordLengthListField where
  fromRow = WordLength <$> field

instance FromRow MaxWordLengthField where
  fromRow = MaxWordLength <$> field

instance FromRow WordMeaningField where
  fromRow = WordMeaning <$> field <*> field

extractWordMeaning :: [WordMeaningField] -> String
extractWordMeaning [] = ""
extractWordMeaning ((WordMeaning word meaning):xs) = word ++ " " ++ meaning ++ "\n" ++ extractWordMeaning xs

extractWordMeaningList :: [[WordMeaningField]] -> String
extractWordMeaningList [] = ""
extractWordMeaningList (wordMeaning : xs) = extractWordMeaning wordMeaning ++ extractWordMeaningList xs

extractWordMeaningListList :: [[[WordMeaningField]]] -> String
extractWordMeaningListList [] = ""
extractWordMeaningListList (wordMeaning : xs) = extractWordMeaningList wordMeaning ++ extractWordMeaningListList xs

extractWordLengthList :: [WordLengthListField] -> [Int]
extractWordLengthList [] = []
extractWordLengthList ((WordLength len) : xs) = len : (extractWordLengthList xs)

