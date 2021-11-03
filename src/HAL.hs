--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HAL where
import AdvancedParser
import Data.List
import Control.Applicative ( Alternative((<|>), empty) )

data Atom = Symbol String | Int Integer | Nil

data Expr = Node [Expr] | Leaf Atom

instance Show Atom where
    show (Symbol x) = show x
    show (Int x) = show x
    show Nil = "Nil"

instance Show Expr where
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

parseExprContent :: Parser Expr
parseExprContent = Parser $ \s -> case runParser parseWhiteSpaces s of
        Just (_, "") -> Just (Leaf Nil, "")
        _ -> do
        (parsed, rest) <- runParser (parseExpr  <|> (Leaf <$> parseAtom)) s
        (res, rest2) <- runParser parseExprContent rest
        case res of
            Leaf x -> return (Node (parsed: [Leaf x]), rest2)
            Node x -> return (Node (parsed: x), rest2)

parseExpr :: Parser Expr
parseExpr = parseWhiteSpaces *> (parseParenthesis <%> parseExprContent)

parseQuoteExpr :: Parser Expr
parseQuoteExpr = Parser $ \s -> do
    (r, rest) <- runParser (parseChar '\'' *> parseExpr) s
    return (Node [Leaf $ Symbol "quote", r], rest)

{--
(foo 2 3)
(define 4 2)

parseExpr -> (foo 2 3) -> Expr Node [Symbol "foo", Int 2, int 3]
parseExpr -> (define 4 2) -> Expr Node [Symbol "define", Int 4, Int 2]
--}