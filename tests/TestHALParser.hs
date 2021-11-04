--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- TestHAL
--

module TestHALParser where

import HALParser
import HAL
import AdvancedParser
import Test.HUnit (Assertion, assertEqual)

case_parseQuote :: Assertion
case_parseQuote =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Leaf (Symbol "quote"), Node [Leaf (Int 1), Leaf (Int 2), Leaf (Int 3)]], " ")
      actual = runParser parseQuote "'(1 2 3) "
  
case_parseQuote_sizeOneList :: Assertion
case_parseQuote_sizeOneList =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Leaf (Symbol "quote"), Node [Leaf (Int 1)]], " ")
      actual = runParser parseQuote "'(1) "

case_parseAtom :: Assertion
case_parseAtom =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Int 1, " 2 3")
      actual = runParser parseAtom "1 2 3"

case_parseExpr :: Assertion
case_parseExpr =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Leaf $ Symbol "+", Leaf (Int 1), Leaf (Int 2)], "  ")
      actual = runParser parseExpr  "(+ 1 2)  "

case_parseExpr_quoted_params :: Assertion
case_parseExpr_quoted_params =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Leaf $ Symbol "append", Node [Leaf $ Symbol "quote", Leaf $ Int 1], Node [Leaf $ Symbol "quote", Node [Leaf $ Int 2]], Node [Leaf $ Symbol "quote", Node [Leaf $ Int 3, Leaf $ Int 4]]], "")
      actual = runParser parseExpr "(append '1 '(2) '(3 4))"

case_parseExpr_nil :: Assertion
case_parseExpr_nil =  assertEqual "Example 1" expected actual
    where 
      expected = Just (Node [Leaf $ Symbol "cons", Leaf $ Int 1, Node [Leaf $ Symbol "quote", Leaf Nil]], "")
      actual = runParser parseExpr "(cons 1 '())"