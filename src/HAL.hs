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
import Evaluator (Evaluator(Evaluator))

evaluate :: EvaluatorFunction Expr
evaluate ([expr], env) = case expr of
    Leaf (Symbol x) -> do
        defined <- getDefine (Leaf $ Symbol x) env
        return (defined, env)
    Leaf x -> Right $ (Leaf x, env)
    List x -> Right (List x, env)
    Procedure (Leaf (Symbol name):args) -> evaluateProcedure name (args, env)
    _ -> Left "Not implemented yet"

evaluateProcedure :: String -> EvaluatorFunction Expr
evaluateProcedure name args = case name of
    "define" -> run (Evaluator define name $ Expected 2) args
    "quote" -> run (Evaluator quote name $ Expected 1) args
    "cons" -> run (Evaluator cons name $ Expected 2) args
    "car" -> run (Evaluator car name $ Expected 1) args
    "cdr" -> run (Evaluator cdr name $ Expected 1) args
    "eq?" -> run (Evaluator eq name $ Expected 2) args
    _ -> Left $ name ++ ": Not implemented"

evaluateAll :: (Args, Env) -> Either ErrorMessage ([Expr], Env)
evaluateAll ([], env) = Right ([], env)
evaluateAll (first:rest, env) = do
    (evaluated, env2) <- HAL.evaluate ([first], env)
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
    Procedure (Leaf (Symbol "quote") : rest) ->
        run quoteOnQuote (rest, env)
    Procedure x -> Right (List x, env)
    x -> return (x, env)
    where
        quoteToSymbol = \res -> Leaf (Symbol ('\'' : show res))
        quoteOnQuote = quoteToSymbol <$> Evaluator quote "quote" (Expected 1)

car :: EvaluatorFunction Expr 
car args =  do
    (args1, env) <- evaluateAll args
    case args1 of
        [List (a:b)] -> Right (a, env)
        [err] -> Left ("car: '" ++ show err ++ "' Invalid argument type")

cdr :: EvaluatorFunction Expr
cdr args =  do
    (args1, env) <- evaluateAll args
    case args1 of
        [List (a:b:c)] -> Right (b, env)
        [List (a:_)] -> Right (Leaf Nil, env)
        [err] -> Left ("cdr: '" ++ show err ++ "' Invalid argument type")

getDefine :: Expr -> Env -> Either ErrorMessage Expr 
getDefine key env = case filter ((==key).fst) env of
    [] -> Left (show key ++ ": Undefined")
    (a:_) -> Right (snd a)

define :: EvaluatorFunction Expr
define (args, env) = case args of
    (Leaf (Symbol name) : x : _) -> Right (Leaf (Symbol name), (Leaf $ Symbol name, x) : env)
    (Procedure a: Procedure b :_) -> case a of
        (Leaf (Symbol aname) : arest) -> Right (Leaf (Symbol aname), (Procedure a , Procedure b) : env)
        [err] -> Left ("define: '" ++ show err ++ "' Invalid argument type")
    [err] -> Left ("define: '" ++ show err ++ "' Invalid argument type")

eq :: EvaluatorFunction Expr
eq args = do
    (args1, env) <- evaluateAll args
    case args1 of
        (Leaf x: Leaf y: _) -> if x == y then Right (Leaf ATrue, env)
                               else Right (Leaf AFalse, env)
        _-> Right (Leaf AFalse, env)