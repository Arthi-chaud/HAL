--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HAL where
import AdvancedParser
import Data.List

data Atom = Symbol String | Int Integer | Nil

data Sexpr = Node [Sexpr] | Leaf Atom

instance Show Atom where
    show (Symbol x) = show x
    show (Int x) = show x
    show Nil = "Nil"

instance Show Sexpr where
    show (Leaf x) = show x
    show (Node x) = '[' : (intercalate ", " (map (show) x)) ++ "]"

parseAtom :: Parser Atom
parseAtom = Parser $ \s -> do
        (parsed, rest) <- runParser firstParser s
        case runParser parseInt parsed of
                Just (nb, "") -> return (Int nb, rest)
                _ -> return (Symbol parsed, rest)
    
    where
        firstParser = parseWhiteSpaces *> parseSome (parseAnyChar tokens)
        tokens = digits ++ letters ++ maj
        digits = "0123456789"
        letters = "abcdefghijklmnopqrstuvwxyz"
        maj = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseSexprContent :: Parser Sexpr
parseSexprContent = Parser $ \s -> case runParser parseWhiteSpaces s of
        Just (_, "") -> Just (Leaf Nil, "")
        _ -> do
        (parsed, rest) <- runParser (parseOr parseSexpr (Leaf <$> parseAtom)) s
        (rest1, rest2) <- runParser parseSexprContent rest
        case rest1 of
                Leaf x -> return (Node (parsed: [Leaf x]), rest2)
                Node x -> return (Node (parsed : x), rest2)

parseSexpr :: Parser Sexpr
parseSexpr = parseWhiteSpaces *> (parseParenthesis <%> parseSexprContent)