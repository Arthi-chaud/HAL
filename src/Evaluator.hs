--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Evalutate
--

module Evaluator where

import HALData
import Data.List
import Data.Maybe (isNothing)

import Control.Applicative ( Alternative((<|>), empty), some )

type ErrorMessage = String

type Env = [(Expr, Expr)]

type Args = [Expr]

data ArgumentCount = Expected Int | Illimited

instance Eq ArgumentCount where
    (==) (Expected a) (Expected b) = a == b
    (==) Illimited Illimited = True
    (==) _ _ = False 

type EvaluatorFunction a = (Args, Env) -> Either ErrorMessage (a, Env)

data Evaluator a = Evaluator {
    function :: EvaluatorFunction a,
    name :: String,
    requiredArg :: ArgumentCount
}

run :: Evaluator a -> EvaluatorFunction a
run eval (args, env)
    | requiredArg eval == Expected (length args) = function eval (args, env)
    | requiredArg eval == Illimited = function eval (args, env)
    | otherwise = Left $ name eval ++ ": Invalid argument count"


instance Functor Evaluator where
    fmap fct evaluator = evaluator {
        function = \params -> do
            (res, env) <- run evaluator params
            return (fct res, env)
    }

instance Applicative Evaluator where
    pure a = Evaluator (\(_, env) -> Right (a, env)) "" Illimited
    
    (<*>) fp p = p {
        function = \(args, env) -> do
            (res, env2) <- run fp (args, env)
            run (res <$> p) (args, env)
    }

instance Alternative Evaluator where
    empty = Evaluator (\s -> Left "Empty") "" Illimited

    (<|>) p1 p2 = p1 {
        function = \s ->
            case run p1 s of
                Left _ -> run p2 s
                res -> res,
        name = name p1 ++ "|" ++ name p2
    }

(<&>) :: Evaluator Expr -> Evaluator Expr -> Evaluator (Expr, Expr)
p1 <&> p2 = p1 {
    function = \(args, env1) -> do
        (res, env2) <- run p1 (args, env1)
        (res1, env3) <- run p2 (args, env2)
        Right ((res, res1), env3),
    name = name p1 ++ " & " ++ name p2
}

(<%>) :: Evaluator Expr -> Evaluator Expr -> Evaluator Expr
p1 <%> p2 = p1 {
    function = \(args, env) -> do
        (res1, env1) <- run p1 (args, env)
        (res2, env2) <- run p2 ([res1], env1)
        Right (res2, env2),
    name = name p1 ++ " % " ++ name p2
}