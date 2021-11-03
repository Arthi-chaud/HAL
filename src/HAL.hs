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
    (parsed, rest) <- runParser (parseWhitespaces *> parseWord) s
    case runParser parseInt parsed of
        Just (int, "") -> Just (Int int, rest)
        _ -> return (Symbol parsed, rest)

parseNil :: Parser Atom
parseNil =  Nil <$ (parseWhitespaces *> (parseChar '(' <&> parseChar ')'))

parseQuote :: Parser Expr
parseQuote = Parser $ \s -> do
    (r, rest) <- runParser (parseWhiteSpaces *> parseChar '\'' *> parser) s
    return (Node [Leaf $ Symbol "quote", r], rest)
    where
        parser = Leaf <$> parseNil <|> parseExpr <|> (Leaf <$> parseAtom)

parseQuoteExpr :: Parser Expr
parseQuoteExpr = parseQuote <|> parseExpr

parseExprContent :: Parser Expr
parseExprContent = Parser $ \s -> do
    (parsed, rest) <- runParser parser s
    case runParser parseWhitespaces rest of
        Just (_, "") -> return (Node [parsed], rest)
        _ -> case runParser parseExprContent rest of
            Just (Leaf x, rest2) -> return (Node (parsed: [Leaf x]), rest2)
            Just (Node x, rest2) -> return (Node (parsed: x), rest2)
            Nothing -> Nothing
    where
        parser = parseQuoteExpr <|> (Leaf <$> parseAtom)

parseExpr :: Parser Expr
parseExpr = parseWhiteSpaces *> (parseParenthesis <%> parseExprContent)


{--
(foo 2 3)
(define 4 2)

parseExpr -> (foo 2 3) -> Expr Node [Symbol "foo", Int 2, int 3]
parseExpr -> (define 4 2) -> Expr Node [Symbol "define", Int 4, Int 2]
--}