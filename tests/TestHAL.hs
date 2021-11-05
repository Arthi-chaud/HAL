--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- TestHAL
--

module TestHAL where

import HAL
import HALData
import Test.HUnit (Assertion, assertEqual)


case_HALCons_Leafs :: Assertion
case_HALCons_Leafs =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2], [])
      actual = cons ([Leaf $ Int 1, Leaf $ Int 2], [])

case_HALCons_LeafAndList :: Assertion
case_HALCons_LeafAndList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3], [])
      actual = cons ([Leaf $ Int 1, Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]], [])

case_HALCons_ListAndLeaf :: Assertion
case_HALCons_ListAndLeaf =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [List[Leaf $ Int 2], Leaf $ Int 1], [])
      actual = cons ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2]], Leaf $ Int 1], [])


case_HALCons_ListAndList :: Assertion
case_HALCons_ListAndList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [List[Leaf $ Int 1], Leaf $ Int 2, Leaf $ Int 3], [])
      actual = cons ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1]], Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]], [])

case_HALQuote_List :: Assertion
case_HALQuote_List =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3], [])
      actual = quote ([Procedure [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3]], [])

case_HALQuote_Int :: Assertion
case_HALQuote_Int =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (Leaf $ Int 1, [])
      actual = quote ([Leaf $ Int 1], [])

case_HALQuote_QuoteOnQuote :: Assertion
case_HALQuote_QuoteOnQuote =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (Leaf $ Symbol "'1", [])
      actual = quote ([Procedure [Leaf $ Symbol "quote", Leaf $ Int 1]], [])

case_HALQuote_QuoteOnQuoteonQuote :: Assertion
case_HALQuote_QuoteOnQuoteonQuote =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (Leaf $ Symbol "''1", [])
      actual = quote ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Symbol "quote", Leaf $ Int 1]]], [])

case_HALCar_List :: Assertion
case_HALCar_List =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (Leaf $ Int 1, [])
      actual = car ([List [Leaf $ Int 1, Leaf $ Int 2]], [])

case_HALCar_EmptyList :: Assertion
case_HALCar_EmptyList =  assertEqual "Example 1" expected actual
    where 
      expected = Left "car: Invalid argument type"
      actual = car ([Procedure [Leaf $ Symbol "quote", Leaf Nil]], [])

case_HALCar_Int :: Assertion
case_HALCar_Int =  assertEqual "Example 1" expected actual
    where 
      expected = Left "car: Invalid argument type"
      actual = car ([Leaf $ Int 1], [])

case_HALCar_NestedList :: Assertion
case_HALCar_NestedList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int  2, Leaf $ Int  3], [])
      actual = car ([List [List [Leaf $ Int  1, Leaf $ Int  2, Leaf $ Int  3], List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6]]], [])

case_HALCdr_List :: Assertion
case_HALCdr_List =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (Leaf $ Int 2, [])
      actual = cdr ([List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3]], [])

case_HALCdr_OneElemList :: Assertion
case_HALCdr_OneElemList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (Leaf Nil, [])
      actual = cdr ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1]]], [])

case_HALCdr_EmptyList :: Assertion
case_HALCdr_EmptyList =  assertEqual "Example 1" expected actual
    where 
      expected = Left "cdr: Invalid argument type"
      actual = cdr ([Procedure [Leaf $ Symbol "quote", Leaf Nil]], [])

case_HALCdr_Int :: Assertion
case_HALCdr_Int =  assertEqual "Example 1" expected actual
    where 
      expected = Left "cdr: Invalid argument type"
      actual = cdr ([Leaf $ Int 1], [])

case_HALCdr_NestedList :: Assertion
case_HALCdr_NestedList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ (List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6], [])
      actual = cdr ([List [List [Leaf $ Int  1, Leaf $ Int  2, Leaf $ Int  3], List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6]]], [])