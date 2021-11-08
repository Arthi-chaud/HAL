--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Main
--

module Main where
import System.Environment
import System.Console.Haskeline
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
    if repl then
        runInputT defaultSettings loop
    else
        return ()
    exitWith (ExitSuccess)
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "> "
            case minput of
                Nothing -> return ()
                Just "exit" -> return ()
                Just input -> do
                    outputStrLn input
                    loop