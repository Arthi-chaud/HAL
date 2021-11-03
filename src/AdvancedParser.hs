--
-- EPITECH PROJECT, 2021
-- evalExprBootstrap
-- File description:
-- AlternativeParser
--

module AdvancedParser where

import Data.Maybe (isNothing)
import Text.Read
import Data.Char
import Control.Applicative ( Alternative((<|>), empty), some )

type ParserFunction a = String -> Maybe (a, String)

data Parser a = Parser {
    runParser :: ParserFunction a
}

instance Functor Parser where
    fmap fct parser = Parser (\s -> do
        (parsed, rest)    <- runParser parser s
        Just (fct parsed, rest)
        )

instance Applicative Parser where
    pure a = Parser (\s -> Just(a, s))
    
    (<*>) fp p = Parser (\string -> do
        (f, rest) <- runParser fp string
        runParser (f <$> p) rest
        )

instance Alternative Parser where
    empty = Parser(const Nothing)

    (<|>) p1 p2 = Parser (\s ->
        case runParser p1 s of
            Nothing -> runParser p2 s
            res -> res
        )
instance Monad Parser where
    return = pure
    (>>=) parser1 parser2 = Parser (\s -> do
        (x,rest) <- runParser parser1 s
        runParser (parser2 x) rest
        )

(<&>) :: Parser a -> Parser b -> Parser (a, b)
(<&>) p1 p2 = Parser (\s -> do
            (c1, rest) <- runParser p1 s
            (c2, finalRest) <- runParser p2 rest
            Just((c1, c2), finalRest)
    )

(<%>) :: Parser String -> Parser a -> Parser a
p1 <%> p2 = Parser (\s -> do
    (parsed1, rest1) <- runParser p1 s
    (parsed2, rest2) <- runParser p2 parsed1
    Just (parsed2, rest2++rest1)
    )

-- parseChar 'a'"abcd"
--Just ('a', "bcd")
-- parseChar 'z'"abcd"
--Nothing
-- parseChar 'b'"abcd"
--Nothing
-- parseChar 'a'"aaaa"
--Just ('a', "aaa")
parseChar :: Char -> Parser Char
parseChar expected = Parser charParser
    where
        charParser :: ParserFunction Char
        charParser s
            | null s = Nothing
            | head s == expected = Just (expected, tail s)
            | otherwise = Nothing

-- parseAnyChar "bca" "abcd"
--Just ('a', "bcd")
-- parseAnyChar "xyz" "abcd"
--Nothing
-- parseAnyChar "bca" "cdef"
--Just ('c', "def")
parseAnyChar :: String -> Parser Char
parseAnyChar needles = Parser anyCharParser
    where
        anyCharParser :: ParserFunction Char 
        anyCharParser s
            | null s = Nothing
            | head s `elem` needles = Just (head s, tail s)
            | otherwise = Nothing 
-- parseOr (parseChar 'a') (parseChar 'b') "abcd"
--Just ('a', "bcd")
-- parseOr (parseChar 'a') (parseChar 'b') "bcda"
--Just ('b', "cda")
-- parseOr (parseChar 'a') (parseChar 'b') "xyz"
--Nothing
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

-- parseAnd (parseChar 'a') (parseChar 'b') "abcd"
--Just (('a','b'), "cd")
-- parseAnd (parseChar 'a') (parseChar 'b') "bcda"
--Nothing
-- parseAnd (parseChar 'a') (parseChar 'b') "acd"
--Nothing
parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd p1 p2 = p1 <&> p2

-- parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b') "abcd"
--Just ("ab", "cd")

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 = Parser (\s -> do
        ((a, b), rest) <- runParser (p1 <&> p2) s
        Just (func a b, rest)
    )
-- parseMany (parseChar ' ') " foobar"
--Just (" ", "foobar")
-- parseMany (parseChar ' ') "foobar "
--Just ("", "foobar ")
parseMany :: Parser a -> Parser [a]
parseMany p = Parser {runParser = \s -> 
    case runParser p s of
        Nothing -> Just ([], s)
        Just (p1, rest) -> runParser ((\p2 -> p1 : p2) <$> parseMany p) rest
    }

-- parseSome (parseAnyChar ['0'..'9']) "42 foobar"
--Just ("42", "foobar")
-- parseSome (parseAnyChar ['0'..'9']) "foobar42"
--Nothing
parseSome :: Parser a -> Parser [a]
parseSome p1 = (\(a,b) -> a:b) <$> (p1 <&> parseMany p1)

parseUInt :: Parser Integer
parseUInt = Parser $ \string -> 
    runParser (read <$> (parseSome (parseAnyChar ['0'..'9']))) string

parseInt :: Parser Integer
parseInt = Parser intParser
    where
        intParser :: ParserFunction Integer
        intParser s = do
            (signs, rest) <- runParser (parseMany (parseChar '-')) s
            runParser ((\nb-> nb * ((-1) ^ length signs)) <$> parseUInt) rest

parseWhiteSpaces :: Parser String
parseWhiteSpaces = parseMany (parseAnyChar " \t")

-- Extracting parenthesis

_getDepthOffset :: Char -> Int 
_getDepthOffset '(' = 1
_getDepthOffset ')' = -1
_getDepthOffset _ = 0

-- If the string doesnt start by parenthesis, returns nothing
_extractParenthesis ::  String -> Int -> Maybe (String, String)
_extractParenthesis [] 0 = Just ([], [])
_extractParenthesis [] _ = Nothing 
_extractParenthesis (')':rest) 0 = Nothing
_extractParenthesis ('(':rest) 0 =  do
        (parsed, newRest) <- _extractParenthesis rest 1
        return ('(':parsed, newRest)
_extractParenthesis (c:rest) 0 = Just ("", c:rest) 
_extractParenthesis (c:rest) depth = if depth < 0 then Nothing else do
        (parsed, newRest) <- _extractParenthesis rest (depth + _getDepthOffset c) 
        return (c:parsed, newRest)

parseParenthesis :: Parser String
parseParenthesis = Parser $ \s -> do
    (_, str) <- runParser parseWhiteSpaces s
    res <- _extractParenthesis str 0
    case res of
        ('(':parsed, rest) -> Just (init parsed, rest)
        _ -> Nothing

parseWhitespaces :: Parser String
parseWhitespaces = parseMany (parseAnyChar " \t")
        

--parseWord :: Parser String
--parseWord = Parser $ \s -> case runParser parseWhitespaces s of
--    Just ("", "") -> Just ([], "")
--    Just ("", a:b) -> runParser ((a :) <$> parseWord) b
--    Just (a, rest) -> Just ([], a ++ rest)
--    _ -> Nothing

parseNotSpace :: Parser Char
parseNotSpace = Parser anyCharParser
    where
        anyCharParser :: ParserFunction Char 
        anyCharParser s
            | null s = Nothing
            | isSpace (head s) = Nothing
            | otherwise = Just (head s, tail s)

parseWord :: Parser String
parseWord = some parseNotSpace