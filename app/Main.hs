--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Main
--

module Main where
import System.Environment
import AdvancedParser
import System.Exit


type FilePath = String

getFilesContents :: [String] -> IO [String]
getFilesContents list = sequence (readFile <$> list)

main :: IO Int
main = do
    args <- getArgs
    let repl = "-i" `elem` args || null args
    let args2 = filter (/= "-i") args
    filesContent <- getFilesContents args2
    exitWith (ExitSuccess)