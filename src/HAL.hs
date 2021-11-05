--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HAL where

import Evaluator
import HALData
import HALParser

evaluate :: EvaluatorFunction Expr
evaluate ([expr], env) = case expr of
    Leaf x -> Right (Leaf x, env)
    List x -> Right (List x, env)
    Procedure (Leaf (Symbol name):args) -> evaluateProcedure name (args, env)
    _ -> Left "Not implemented yet"

evaluateProcedure :: String -> EvaluatorFunction Expr
evaluateProcedure name args = case name of
    "quote" -> run (Evaluator quote name $ Expected 1) args
    "cons" -> run (Evaluator cons name $ Expected 2) args
    "car" -> run (Evaluator car name $ Expected 1) args
    "cdr" -> run (Evaluator cdr name $ Expected 1) args
    _ -> Left $ name ++ ": Not implemented"

evaluateAll :: (Args, Env) -> Either ErrorMessage ([Expr], Env)
evaluateAll ([], env) = Right ([], env)
evaluateAll (first:rest, env) = do
    (evaluated, env2) <- evaluate ([first], env)
    (evaluatedRest, env3) <- evaluateAll (rest, env2)
    return (evaluated : evaluatedRest, env3)

cons :: EvaluatorFunction Expr 
cons expr = case evaluateAll expr of
    Right (args1:args2:_, env) -> case args2 of
        List x -> Right (List (args1 : x), env)
        Procedure x -> Left $ show args2
        x -> Right (List (args1 : [args2]), env)
    Left message -> Left message

quote :: EvaluatorFunction Expr 
quote (args, env) = case head args of
    Procedure (Leaf (Symbol "quote") : rest) -> do
        (res, env) <- run (Evaluator quote "quote" $ Expected 1) (rest, env)
        return (Leaf (Symbol ('\'' : show res)), env)
    Procedure x -> Right (List x, env)
    x -> return (x, env)

car :: EvaluatorFunction Expr 
car args =  do
    (args1, env) <- evaluateAll args
    case args1 of
        [List (a:b)] -> Right (a, env)
        _ -> Left "car: Invalid argument type"

cdr :: EvaluatorFunction Expr
cdr args =  do
    (args1, env) <- evaluateAll args
    case args1 of
        [List (a:b:c)] -> Right (b, env)
        [List (a:_)] -> Right (Leaf Nil, env)
        _ -> Left "cdr: Invalid argument type"