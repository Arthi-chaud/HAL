--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- FileManager
--

module FileManager where

getFilesContents :: [FilePath] -> IO [String]
getFilesContents list = sequence (readFile <$> list)
