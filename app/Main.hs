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

mainFiles :: [String] -> IO Int
mainFiles filesContent = do
    exitWith ExitSuccess


loopREPL :: InputT IO ()
loopREPL = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
            outputStrLn input
            loopREPL

mainREPL :: [String] -> IO Int
mainREPL filesContent = do
        runInputT defaultSettings loopREPL
        exitWith ExitSuccess

main :: IO Int
main = do
    args <- getArgs
    let repl = "-i" `elem` args || null args
    let argsNoFlags = filter (/= "-i") args
    filesContent <- getFilesContents argsNoFlags
    if repl then
        mainREPL filesContent
    else
        mainFiles filesContent