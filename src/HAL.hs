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
import Data.List
import Text.Printf

import Data.Either

evaluate :: EvaluatorFunction Expr
evaluate ([], _) = Left "Empty Expression"
evaluate ((Lambda x):args, env) = evaluateLamba (Lambda x) (args, env)
evaluate (expr, env) = case head expr of
    Leaf (Symbol x) -> do
        defined <- getDefine (Leaf $ Symbol x) env
        return (defined, env)
    Leaf x -> Right (Leaf x, env)
    List x -> Right (List x, env)
    Procedure (Leaf (Symbol "lambda"):args) -> do
        (res, newenv) <- evaluateProcedure "lambda" (args, env)
        case tail expr of
            [] -> return (res, newenv)
            _ -> HAL.evaluate (res : tail expr, newenv)
    Procedure (Leaf (Symbol name):args) -> evaluateProcedure name (args, env)
    Procedure x -> HAL.evaluate (x, env)
    x -> Left (show x ++ ": Not implemented yet")

evaluateAll :: (Args, Env) -> Either ErrorMessage ([Expr], Env)
evaluateAll ([], env) = Right ([], env)
evaluateAll (first:rest, env) = do
    (evaluated, env2) <- HAL.evaluate ([first], env)
    (evaluatedRest, env3) <- evaluateAll (rest, env2)
    return (evaluated : evaluatedRest, env3)

evaluateLamba :: Expr -> EvaluatorFunction Expr
evaluateLamba (Lambda (body, count)) (args, env) = if length args /= count
    then Left (printf "lambda: Expected %d arguments, got %d" count (length args))
    else do
        (res, env) <- lambdaInsertArgs body (args, env)
        HAL.evaluate ([Procedure res], env)
evaluateLamba _ (_, env) = Left "lambda evaluation: invalid argument"

evaluateProcedure :: String -> EvaluatorFunction Expr
evaluateProcedure name args = case name of
    "define" -> run (Evaluator define name $ Expected 2) args
    "quote" -> run (Evaluator quote name $ Expected 1) args
    "lambda" -> run (Evaluator lambda name $ Expected 2) args
    "let" -> run (Evaluator letFct name $ Expected 2) args
    "cons" -> run (Evaluator cons name $ Expected 2) args
    "car" -> run (Evaluator car name $ Expected 1) args
    "cdr" -> run (Evaluator cdr name $ Expected 1) args
    "eq?" -> run (Evaluator eq name $ Expected 2) args
    "atom?" -> run (Evaluator atom name $ Expected 1) args
    "cond" -> run (Evaluator cond name Illimited) args
    ">" -> run (Evaluator (HAL.compare (>)) name $ Expected 2) args
    "<" -> run (Evaluator (HAL.compare (<)) name $ Expected 2) args
    _ -> case getDefine (Leaf (Symbol name)) (snd args) of
        Right x -> HAL.evaluate (x : fst args, snd args) 
        _ -> evaluateMathematicalProcedure name args 


evaluateMathematicalProcedure :: String -> EvaluatorFunction Expr
evaluateMathematicalProcedure "+" ([Leaf (Int a)], env) = Right (Leaf (Int a), env)
evaluateMathematicalProcedure "-" ([Leaf (Int a)], env) = Right (Leaf (Int (a * (-1))), env)
evaluateMathematicalProcedure "*" ([Leaf (Int a)], env) = Right (Leaf (Int a), env)
evaluateMathematicalProcedure name args = case name of
    "+" -> run (Evaluator (operate addHAL) name Illimited) args
    "-" -> run (Evaluator (operate subHAL) name Illimited) args
    "*" -> run (Evaluator (operate mulHAL) name Illimited) args
    "mod" -> run (Evaluator (operate modHAL) name $ Expected 2) args
    "div" -> run (Evaluator (operate divHAL) name $ Expected 2) args
    _ -> Left $ name ++ ": Not a function"

cons :: EvaluatorFunction Expr 
cons expr = case evaluateAll expr of
    Right ([args1, args2], env) -> case args2 of
        List x -> if not (null x) && last x == Leaf Nil
            then Right (List (args1 : x), env)
            else Right (List (args1 : x ++ [Leaf Nil]), env)
        Procedure x -> Left $ show args2
        x -> Right (List [args1, args2], env)
    Right (_, _) -> Left "cons: Invalid arguments"
    Left message -> Left message

quote :: EvaluatorFunction Expr 
quote (args, env) = case head args of
    Procedure (Leaf (Symbol "quote") : rest) ->
        run quoteOnQuote (rest, env)
    Procedure x -> if length x <= 2 then Right (List x, env)
                   else case last x of
                       Leaf Nil -> Right (List x, env)
                       _ -> Right (List (x ++ [Leaf Nil]), env)
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
        [List [a, b]] -> Right (b, env)
        [List (a:b)] -> Right (List b, env)
        --[List (a:_)] -> Right (Leaf Nil, env)
        x -> Left ("cdr: '" ++ show (head x) ++ "' Invalid argument type")

getDefine :: Expr -> Env -> Either ErrorMessage Expr 
getDefine  (Leaf (Int x)) env = Right (Leaf $ Int x)
getDefine  (Leaf AFalse) env = Right (Leaf AFalse)
getDefine  (Leaf ATrue ) env = Right (Leaf ATrue)
getDefine  (Leaf Nil) env = Right (Leaf Nil)
getDefine key env = case filter ((==key).fst) env of
    [] -> Left (show key ++ ": Undefined")
    (a:_) -> Right (snd a)

define :: EvaluatorFunction Expr
define ((Procedure ((Leaf (Symbol name)):keys)): Procedure body : _, env) = do
    (l, _)  <- lambda ([Procedure keys, Procedure body], env)
    return (Leaf $ Symbol name, nub (env ++ [(Leaf $ Symbol name, l)]))
define (key:value, env) = do
    (newValue, _) <- HAL.evaluate (value, env)
    case [key, newValue] of
        (Leaf (Symbol name) : x : _) -> Right (Leaf (Symbol name), nub (env ++ [(Leaf $ Symbol name, x)]))
        (Procedure a: Procedure b :_) -> case a of
            (Leaf (Symbol aname) : arest) -> Right (Leaf (Symbol aname), nub (env ++ [(Procedure a , Procedure b)]))
            x -> Left ("define: '" ++ show x ++ "' Invalid argument type")
        x -> Left ("define: '" ++ show (head x) ++ "' Invalid argument type")
define _ = Left "define: Invalid argument count"

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
divHAL a b = Just (quot a b)

operate :: (Integer -> Integer -> Maybe Integer) -> EvaluatorFunction Expr
operate func args = do
    (args1, env) <- evaluateAll args
    case args1 of
        ((Leaf (Int a)) : (Leaf (Int b)): (Leaf (Int c)) : _) -> case operate func (tail args1, env) of
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

lambda :: EvaluatorFunction Expr
lambda (args, env) = case args of
    (Procedure vars:Procedure body:_) -> if vars == nub vars
        then case lambdaReplaceAllArgs vars body 0 of
            Nothing -> Left "lambda: Invalid parameter type"
            Just computedBody -> Right (Lambda (computedBody, length vars), env)
        else Left "lambda: Duplicate in parameter list"
    x -> Left "lambda: Invalid argument count"

lambdaReplaceAllArgs :: [Expr] -> [Expr] -> Int -> Maybe [Expr]
lambdaReplaceAllArgs [] body _ = Just body
lambdaReplaceAllArgs (Leaf (Symbol x):r) body argIndex = do
    newBody <- lambdaReplaceAllArgs r body (argIndex + 1)
    return (lambdaReplaceArgs x newBody argIndex)
lambdaReplaceAllArgs _ _ _ = Nothing
       

lambdaReplaceArgs :: String -> [Expr] -> Int -> [Expr]
lambdaReplaceArgs argName [] _ = []
lambdaReplaceArgs argName (a:rest) index = if a == leafArgName
    then Leaf (Index index) : computedRest
    else a : computedRest
    where
        leafArgName = Leaf (Symbol argName)
        computedRest = lambdaReplaceArgs argName rest index

lambdaInsertArgs :: Args -> EvaluatorFunction [Expr]
lambdaInsertArgs [] (_, env) = Right ([], env)
lambdaInsertArgs (first:body) (args, env) = case first of
    Leaf (Index x) -> if x >= length args
        then Left "lambda evaluation: invalid argument count"
        else do
            (replaced, newEnv) <- lambdaInsertArgs body (args, env)
            return ((args !! x : replaced), newEnv)
    x -> do
        (replaced, newEnv) <- lambdaInsertArgs body (args, env)
        return ((first : replaced), newEnv)

letFct :: EvaluatorFunction Expr
letFct ([Procedure exprList, body], env) =
        if not (null errorParams) then
            Left $ head errorParams
        else case lambda ([Procedure keys, body], env) of
            Right (l, newenv) -> HAL.evaluate ([Procedure (l:values)], newenv)
            Left err -> Left err
        where
            (errorParams, params) = partitionEithers $ letFctGetParams <$> exprList
            keys = fst <$> params
            values = snd <$> params
letFct _ = Left "let: Invalid argument count"

letFctGetParams :: Expr -> Either ErrorMessage (Expr, Expr)
letFctGetParams (Procedure a)
    | length a == 2 = Right (head a, last a)
    | otherwise = Left "let: Invalid argument type3"
letFctGetParams _ = Left "let: Invalid argument type4"