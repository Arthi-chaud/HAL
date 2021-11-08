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
evaluate ([], _) = Left "Empty Expression"
evaluate (expr, env) = case head expr of
    Leaf (Symbol x) -> do
        defined <- getDefine (Leaf $ Symbol x) env
        return (defined, env)
    Leaf x -> Right (Leaf x, env)
    List x -> Right (List x, env)
    Procedure (Leaf (Symbol name):args) -> evaluateProcedure name (args, env)
    x -> Left (show x ++ ": Not implemented yet")

evaluateProcedure :: String -> EvaluatorFunction Expr
evaluateProcedure name args = case name of
    "define" -> run (Evaluator define name $ Expected 2) args
    "quote" -> run (Evaluator quote name $ Expected 1) args
    "cons" -> run (Evaluator cons name $ Expected 2) args
    "car" -> run (Evaluator car name $ Expected 1) args
    "cdr" -> run (Evaluator cdr name $ Expected 1) args
    "eq?" -> run (Evaluator eq name $ Expected 2) args
    "atom?" -> run (Evaluator atom name $ Expected 1) args
    "cond" -> run (Evaluator cond name Illimited) args
    ">" -> run (Evaluator (HAL.compare (>)) name $ Expected 2) args
    "<" -> run (Evaluator (HAL.compare (<)) name $ Expected 2) args
    _ -> evaluateMathematicalProcedure name args

evaluateMathematicalProcedure :: String -> EvaluatorFunction Expr
evaluateMathematicalProcedure name args = case name of
    "+" -> run (Evaluator (operate addHAL) name Illimited) args
    "-" -> run (Evaluator (operate subHAL) name Illimited) args
    "*" -> run (Evaluator (operate mulHAL) name Illimited) args
    "%" -> run (Evaluator (operate modHAL) name $ Expected 2) args
    "/" -> run (Evaluator (operate divHAL) name $ Expected 2) args
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
        x -> Left ("car: '" ++ show (head x) ++ "' Invalid argument type")

cdr :: EvaluatorFunction Expr
cdr args =  do
    (args1, env) <- evaluateAll args
    case args1 of
        [List (a:b:c)] -> Right (b, env)
        [List (a:_)] -> Right (Leaf Nil, env)
        x -> Left ("cdr: '" ++ show (head x) ++ "' Invalid argument type")

getDefine :: Expr -> Env -> Either ErrorMessage Expr 
getDefine key env = case filter ((==key).fst) env of
    [] -> Left (show key ++ ": Undefined")
    (a:_) -> Right (snd a)

define :: EvaluatorFunction Expr
define (args, env) = case args of
    (Leaf (Symbol name) : x : _) -> Right (Leaf (Symbol name), (Leaf $ Symbol name, x) : env)
    (Procedure a: Procedure b :_) -> case a of
        (Leaf (Symbol aname) : arest) -> Right (Leaf (Symbol aname), (Procedure a , Procedure b) : env)
        x -> Left ("define: '" ++ show x ++ "' Invalid argument type")
    x -> Left ("define: '" ++ show (head x) ++ "' Invalid argument type")

eq :: EvaluatorFunction Expr
eq args = do
    (args1, env) <- evaluateAll args
    case args1 of
        (Leaf x: Leaf y: _) -> if x == y then Right (Leaf ATrue, env)
                               else Right (Leaf AFalse, env)
        _-> Right (Leaf AFalse, env)

addHAL :: Integer -> Integer -> Maybe Integer
addHAL a b = Just (a + b)

subHAL :: Integer -> Integer -> Maybe Integer
subHAL a b = Just (a - b)

mulHAL :: Integer -> Integer -> Maybe Integer
mulHAL a b = Just (a * b)

modHAL :: Integer -> Integer -> Maybe Integer
modHAL a 0 = Nothing
modHAL a b = Just (mod a b)

divHAL :: Integer -> Integer -> Maybe Integer
divHAL a 0 = Nothing
divHAL a b = Just (div a b)

operate :: (Integer -> Integer -> Maybe Integer) -> EvaluatorFunction Expr
operate func args = do
    (args1, env) <- evaluateAll args
    case args1 of
        ((Leaf (Int a)) : (Leaf (Int b)): (Leaf (Int c)) : _) -> do
            case operate func (tail args1, env) of
                Right (Leaf (Int res), env) -> operate func ([Leaf $ Int a, Leaf $ Int res], env)
                s -> Left (show s)
        ((Leaf (Int a)) : (Leaf (Int b)): _) -> case func a b of
            Nothing ->  Left "Operation: Division by Zero"
            Just res -> Right (Leaf (Int res), env)
        [a] -> Left "Operation: Invalid argument count"
        x -> Left ("Operation: " ++ show x ++ " Invalid argument type")

compare :: (Integer -> Integer -> Bool) -> EvaluatorFunction Expr
compare func args = do
    (args1, env) <- evaluateAll args
    case args1 of
        [Leaf (Int a), Leaf (Int b)] -> if func a b then Right (Leaf ATrue, env)
                                        else Right (Leaf AFalse, env)
        x ->  Left ("Operation: '" ++ show (head x) ++ "' Invalid argument type")

atom :: EvaluatorFunction Expr
atom args = do
    (args1, env) <- evaluateAll args
    case head args1 of
        Leaf x -> Right (Leaf ATrue, env)
        _ -> Right (Leaf AFalse, env)

cond :: EvaluatorFunction Expr
cond ([], _) = Left "cond: Invalid argument count"
cond (args, env) = case args of
    (Procedure a:Procedure b:x) -> case cond ([Procedure a], env) of
        Right (Leaf ANothing, _) -> cond (Procedure b:x, env)
        x -> x
    [Procedure (c:r:_)] -> do
        (res, env1) <- HAL.evaluate ([c], env)
        case res of
            Leaf ATrue -> HAL.evaluate ([r], env)
            Leaf AFalse -> Right (Leaf ANothing, env1)
            x ->  Left "cond: Invalid condition return type"
    [Procedure (c:_)] -> Left "cond: Invalid argument type"
    x -> Left $ "cond: " ++ show x ++ " Invalid argument type"