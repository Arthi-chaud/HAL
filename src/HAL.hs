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

type Env = [(Expr, Expr)]

evaluate :: Expr -> MaybeExpr
evaluate expr = case expr of
    Leaf x -> Right $ Leaf x
    List x -> Right $ List x
    Procedure (Leaf (Symbol name):args) -> evaluateProcedure name args
    _ -> Left "Not implemented yet"

evaluateProcedure :: String -> [Expr] -> MaybeExpr
evaluateProcedure name args = case name of
    "quote" -> quote args
    "cons" -> cons args
    "car" -> car args
    "cdr" -> cdr args
    _ -> Left $ "Not implemented: " ++ name

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
        Right (args1:args2:_) -> case args2 of
            List x -> Right $ List (args1 : x)
            Procedure x -> Left $ show args2
            x -> Right $ List (args1 : [args2])
        Left msg -> Left ("cons: " ++ msg)
        _ -> Left "error"

quote :: [Expr] -> MaybeExpr
quote args = case checkParamCount 1 args of
        Left msg -> Left ("quote: " ++ msg)
        Right (args1:_) -> case args1 of
            Procedure (Leaf (Symbol "quote") : rest) -> case quote rest of
                Left msg -> Left msg
                Right res -> Right $ Leaf (Symbol ('\'' : show res))
            Procedure x -> Right $ List x
            x -> Right x
        _ -> Left "error"

car :: [Expr] -> MaybeExpr
car args =  case evaluateAll args of
    Left message -> Left message
    Right args -> case checkParamCount 1 args of
        Left msg -> Left ("car: " ++ msg)
        Right args1 -> case args1 of
            [List (a:b)] -> Right a
            _ -> Left "car: Invalid argument type"

cdr :: [Expr] -> MaybeExpr
cdr args =  case evaluateAll args of
    Left message -> Left message
    Right args -> case checkParamCount 1 args of
        Left msg -> Left ("cdr: " ++ msg)
        Right args1 -> case args1 of
            [List (a:b:c)] -> Right b
            [List (a:_)] -> Right $ Leaf Nil
            _ -> Left "cdr: Invalid argument type"