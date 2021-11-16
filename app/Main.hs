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
import Data.Maybe
import System.Exit
import Evaluator
import HALData
import HALParser
import HAL (evaluateAll)

type FilePath = String

getFilesContents :: [String] -> IO [String]
getFilesContents list = sequence (readFile <$> list)

evaluateFilesContents :: [String] -> Either ErrorMessage ([Expr], Env)
evaluateFilesContents filesContent
    | errorExpression = Left "Parsing Error"
    | otherwise = evaluateAll (expressions, [])
    where
        expressionsMaybe = runParser parseAllExpr <$> filesContent
        errorExpression = any isNothing expressionsMaybe
        expressions = concat $ fst . fromJust <$> filter isJust expressionsMaybe

mainFiles :: [String] -> IO Int
mainFiles filesContent = case evaluateFilesContents filesContent of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right (a, env) -> print (last a) >> exitSuccess


loopREPL :: Env -> InputT IO ()
loopREPL env = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> case runParser parseAllExpr input of
            Just (res, _) -> case HAL.evaluateAll (res, env) of
                Left x -> outputStrLn x >> loopREPL env
                Right (res1, env) -> outputStrLn (show (last res1)) >> loopREPL env
            Nothing -> outputStrLn "Parsing Error" >> loopREPL env

mainREPL :: [String] -> IO Int
mainREPL filesContent = case evaluateFilesContents filesContent of
    Left err -> putStrLn err >> exitWith (ExitFailure 84)
    Right (_, env) -> runInputT defaultSettings (loopREPL env) >> exitSuccess

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