--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HALParser where
import AdvancedParser
import Data.List
import Control.Applicative ( Alternative((<|>), empty) )
import HALData

parseAtom :: Parser Atom
parseAtom = Parser $ \s -> do
    (parsed, rest) <- runParser (parseWhitespaces *> parseWord) s
    case parsed of
        "#f" -> return (AFalse, rest)
        "#t" -> return (ATrue, rest)
        _ -> case runParser parseInt parsed of
            Just (int, "") -> Just (Int int, rest)
            _ -> return (Symbol parsed, rest)

parseNil :: Parser Atom
parseNil =  Nil <$ (parseWhitespaces *> (parseChar '(' <&> parseChar ')'))

parseQuote :: Parser Expr
parseQuote = Parser $ \s -> do
    (r, rest) <- runParser (parseWhiteSpaces *> parseChar '\'' *> parser) s
    case r of
        Procedure ((Leaf (Symbol "quote"):a)) -> return (Procedure (Leaf (Symbol "quote"):[r]), rest)
        Procedure a -> return (Procedure [Leaf $ Symbol "quote", Procedure (a ++ [Leaf Nil]) ], rest)
        _ -> return (Procedure [Leaf $ Symbol "quote", r], rest)
    where
        parser = Leaf <$> parseNil <|> parseQuoteExpr <|> (Leaf <$> parseAtom)

parseQuoteExpr :: Parser Expr
parseQuoteExpr = parseQuote <|> parseExpr

parseExprContent :: Parser Expr
parseExprContent = Parser $ \s -> do
    (parsed, rest) <- runParser parser s
    case runParser parseWhitespaces rest of
        Just (_, "") -> return (Procedure [parsed], rest)
        _ -> case runParser parseExprContent rest of
            Just (Leaf x, rest2) -> return (Procedure (parsed: [Leaf x]), rest2)
            Just (Procedure x, rest2) -> return (Procedure (parsed: x), rest2)
            _ -> Nothing
    where
        parser = parseQuoteExpr <|> (Leaf <$> parseAtom)

parseExpr :: Parser Expr
parseExpr = (parseWhiteSpaces *> (parseProc <|> parseQuoteLAtom)) <|> parseLAtom
    where
        parseProc =  parseParenthesis <%> parseExprContent
        parseQuoteLAtom =  parseQuote <|> parseLAtom
        parseLAtom = Leaf <$> parseAtom

parseAllExpr :: Parser [Expr]
parseAllExpr = Parser parseAll
    where
        parseAll :: String -> Maybe ([Expr], String)
        parseAll s = do
            (parsed, rest) <- runParser parseExpr s
            case rest of
                [] -> Just ([parsed], rest)
                _ -> runParser ((parsed:) <$> parseAllExpr) rest