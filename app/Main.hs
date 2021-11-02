--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Main
--

module Main where
import System.Environment
import AdvancedParser

type FilePath = String

fromFiles :: [FilePath] -> IO Int

repl :: IO Int

main :: IO Int
main = do
    args <- getArgs
    case args of
        [] -> repl
        list -> 