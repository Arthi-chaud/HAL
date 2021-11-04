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
    case runParser parseInt parsed of
        Just (int, "") -> Just (Int int, rest)
        _ -> return (Symbol parsed, rest)

parseNil :: Parser Atom
parseNil =  Nil <$ (parseWhitespaces *> (parseChar '(' <&> parseChar ')'))

parseQuote :: Parser Expr
parseQuote = Parser $ \s -> do
    (r, rest) <- runParser (parseWhiteSpaces *> parseChar '\'' *> parser) s
    return (Procedure [Leaf $ Symbol "quote", r], rest)
    where
        parser = Leaf <$> parseNil <|> parseExpr <|> (Leaf <$> parseAtom)

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
parseExpr = parseWhiteSpaces *> (parseProcedure <|> parseQuoteLeafAtom)
    where
        parseProcedure =  parseParenthesis <%> parseExprContent
        parseQuoteLeafAtom =  parseQuote <|> (Leaf <$> parseAtom)