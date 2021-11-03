--
-- EPITECH PROJECT, 2021
-- evalExprBootstrap
-- File description:
-- TestAdvancedParser
--

--
-- EPITECH PROJECT, 2021
-- haskellParser
-- File description:
-- TestParser2
--

module TestAdvancedParser where

import AdvancedParser
import Test.HUnit (Assertion, assertEqual)

case_parseChar_example_1 :: Assertion
case_parseChar_example_1 =  assertEqual "Example 1" expected actual
    where 
      expected = Just ('a', "bcd")
      actual = runParser (parseChar 'a') "abcd"

case_parseChar_example_2 :: Assertion
case_parseChar_example_2 =  assertEqual "Example 2" expected actual
  where 
      expected = Nothing
      actual = runParser (parseChar 'z') "ybcd"

case_parseChar_example_3 :: Assertion
case_parseChar_example_3 =  assertEqual "Example 3" expected actual
  where 
      expected = Nothing
      actual = runParser (parseChar 'b') "abcd"

case_parseChar_example_4 :: Assertion
case_parseChar_example_4 =  assertEqual "Example 4" expected actual
    where 
        expected = Just ('a', "aaa")
        actual = runParser (parseChar 'a') "aaaa"
    
case_parseAnyChar_example_1 :: Assertion
case_parseAnyChar_example_1 = assertEqual "Example 1" expected actual
    where
        expected = Just ('a', "bcd")
        actual = runParser (parseAnyChar "bca") "abcd"

case_parseAnyChar_example_2 :: Assertion
case_parseAnyChar_example_2 = assertEqual "Example 2" expected actual
    where
        expected = Nothing
        actual = runParser (parseAnyChar "xyz") "abcd"

case_parseAnyChar_example_3 :: Assertion
case_parseAnyChar_example_3 = assertEqual "Example 3" expected actual
    where
        expected = Just ('c', "def")
        actual = runParser (parseAnyChar "bca") "cdef"

case_parseOr_example_1 :: Assertion
case_parseOr_example_1 = assertEqual "Example 1" expected actual
    where
        actual = runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Just ('a', "bcd")

case_parseOr_example_2 :: Assertion
case_parseOr_example_2 = assertEqual "Example 2" expected actual
    where
        actual = runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda"
        expected = Just ('b', "cda")

case_parseOr_example_3 :: Assertion
case_parseOr_example_3 = assertEqual "Example 3" expected actual
    where
        actual = runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz"
        expected = Nothing

case_parseAnd_example_1 :: Assertion
case_parseAnd_example_1 = assertEqual "Example 1" expected actual
    where
        actual = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Just (('a','b'), "cd")

case_parseAnd_example_2 :: Assertion
case_parseAnd_example_2 = assertEqual "Example 2" expected actual
    where
        actual = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda"
        expected = Nothing

case_parseAnd_example_3 :: Assertion
case_parseAnd_example_3 = assertEqual "Example 3" expected actual
    where
        actual = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"
        expected = Nothing

case_parseAndWith_example_1 :: Assertion
case_parseAndWith_example_1 = assertEqual "Example 1" expected actual
    where
        actual = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Just ("ab", "cd")

case_parseMany_example_1 :: Assertion
case_parseMany_example_1 = assertEqual "Example 1" expected actual
    where
        actual = runParser (parseMany (parseChar ' ')) "  foobar"
        expected = Just ("  ", "foobar")

case_parseMany_example_2 :: Assertion
case_parseMany_example_2 = assertEqual "Example 2" expected actual
    where
        actual = runParser (parseMany (parseChar ' ')) "foobar  "
        expected = Just ("", "foobar  ")

case_parseSome_example_1 :: Assertion
case_parseSome_example_1 = assertEqual "Example 1" expected actual
    where
        actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"
        expected = Just ("42", "foobar")

case_parseSome_example_2 :: Assertion
case_parseSome_example_2 = assertEqual "Example 2" expected actual
    where
        actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"
        expected = Nothing

case_parseInt_example :: Assertion
case_parseInt_example = assertEqual "Example 1" expected actual
    where
        actual  = runParser parseInt "123 ,456" 
        expected = Just (123, " ,456")

case_parseWord :: Assertion
case_parseWord =  assertEqual "Example 1" expected actual
    where 
      expected = Just ("1", " 2")
      actual = runParser parseWord "1 2"

case_parseWord_frontSpace :: Assertion
case_parseWord_frontSpace =  assertEqual "Example 1" expected actual
    where 
      expected = Nothing
      actual = runParser parseWord "  Hello  "