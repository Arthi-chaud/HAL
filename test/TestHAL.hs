--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- TestHAL
--

module TestHAL where

import HAL
import Test.HUnit (Assertion, assertEqual)

case_parseQuote :: Assertion
case_parseQuote =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Symbol "quote", Node [Int 1, Int 2, Int 3]], "")
      actual = runParser parseQuote "'(1 2 3)"

case_parseExpr :: Assertion
case_parseExpr =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Symbol "+", Int 1, Int 2], "")
      actual = runParser parseExpr "(+ 1 2)"

case_parseExpr_quoted_params :: Assertion
case_parseExpr_quoted_params =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Symbol "append", Node [Symbol "quote", Node [Int 1]], Node [Symbol "quote", Node [Int 2, Int 3]]], "")
      actual = runParser parseExpr "(append '(1) '(2 3))"

case_parseExpr_nil :: Assertion
case_parseExpr_nil =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Symbol "cons", Int 1, Node [Symbol "quote", Nil]], "")
      actual = runParser parseExpr "(cons 1 '())"