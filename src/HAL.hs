--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HAL where

import HALData
import HALError
import HALParser

evaluate :: Expr -> MaybeExpr
evaluate expr = case expr of
    Leaf x -> Right $ Leaf x
    List x -> Right $ List x
    Procedure (Leaf (Symbol "quote"):args) -> quote args
    Procedure (Leaf (Symbol "cons"):args) -> cons args
    _ -> Left "Not implemented yet"

evaluateAll :: [Expr] -> Either ErrorMessage [Expr]
evaluateAll [] = Right []
evaluateAll list = mapM evaluate list

checkParamCount :: Int -> [Expr] -> Either ErrorMessage [Expr]
checkParamCount nb list = if length list == nb then Right (take nb list)
                          else Left "Invalid argument count"

cons :: [Expr] -> MaybeExpr 
cons expr = case evaluateAll expr of
    Left message -> Left message
    Right args -> case checkParamCount 2 args of
        Right (args1:args2:_) -> Right $ List (args1 : [args2])
        Left msg -> Left ("cons: " ++ msg)
        _ -> Left "error"

quote :: [Expr] -> MaybeExpr
quote args = case checkParamCount 1 args of
        Left msg -> Left ("quote: " ++ msg)
        Right (args1:_) -> case args1 of
            Procedure (Leaf (Symbol "quote") : rest) -> Right $ Leaf $ Symbol $ '\'' : show rest
            x -> Right $ Leaf $ Symbol $ show x
        _ -> Left "error"