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

instance Eq Atom where
    (==) (Symbol a) (Symbol b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) Nil Nil = True
    (==) _  _ = False

instance Eq Expr where
    (==) (Leaf a) (Leaf b) = a == b
    (==) (Node a) (Node b) = a == b
    (==) _ _ = False

instance Show Atom where
    show (Symbol x) = show x
    show (Int x) = show x
    show Nil = "Nil"

instance Show Expr where
    show (Leaf x) = show x
    show (Node x) = '[' : intercalate ", " (map (show) x) ++ "]"

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
    
parseQuote :: Parser Expr
parseQuote = Parser $ \s -> do
    (r, rest) <- runParser (parseChar '\'' *> parseExpr) s
    return (Node [Leaf $ Symbol "quote", r], rest)

parseExprContent :: Parser Expr
parseExprContent = Parser $ \s -> do
    (parsed, rest) <- runParser (parseQuoteExpr <|> (Leaf <$> parseAtom)) s
    case runParser parseExprContent rest of
        Nothing -> return (parsed, "")
        Just (Leaf x, rest2) -> return (Node (parsed: [Leaf x]), rest2)
        Just (Node x, rest2) -> return (Node (parsed: x), rest2)


parseExpr :: Parser Expr
parseExpr = parseWhiteSpaces *> (parseParenthesis <%> parseExprContent)


parseQuoteExpr :: Parser Expr
parseQuoteExpr = parseQuote <|> parseExpr
{--
(foo 2 3)
(define 4 2)

parseExpr -> (foo 2 3) -> Expr Node [Symbol "foo", Int 2, int 3]
parseExpr -> (define 4 2) -> Expr Node [Symbol "define", Int 4, Int 2]
--}