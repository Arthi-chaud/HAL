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
case_HALCons_Leafs =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2], [])
      actual = cons ([Leaf $ Int 1, Leaf $ Int 2], [])

case_HALCons_LeafAndList :: Assertion
case_HALCons_LeafAndList =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3], [])
      actual = cons ([Leaf $ Int 1, Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]], [])

case_HALCons_ListAndLeaf :: Assertion
case_HALCons_ListAndLeaf =  assertEqual "" expected actual
    where 
      expected = Right $ (List [List[Leaf $ Int 2], Leaf $ Int 1], [])
      actual = cons ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2]], Leaf $ Int 1], [])


case_HALCons_ListAndList :: Assertion
case_HALCons_ListAndList =  assertEqual "" expected actual
    where 
      expected = Right $ (List [List[Leaf $ Int 1], Leaf $ Int 2, Leaf $ Int 3], [])
      actual = cons ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1]], Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]], [])

case_HALQuote_List :: Assertion
case_HALQuote_List =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3], [])
      actual = quote ([Procedure [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3]], [])

case_HALQuote_Int :: Assertion
case_HALQuote_Int =  assertEqual "" expected actual
    where 
      expected = Right $ (Leaf $ Int 1, [])
      actual = quote ([Leaf $ Int 1], [])

case_HALQuote_QuoteOnQuote :: Assertion
case_HALQuote_QuoteOnQuote =  assertEqual "" expected actual
    where 
      expected = Right $ (Leaf $ Symbol "'1", [])
      actual = quote ([Procedure [Leaf $ Symbol "quote", Leaf $ Int 1]], [])

case_HALQuote_QuoteOnQuoteonQuote :: Assertion
case_HALQuote_QuoteOnQuoteonQuote =  assertEqual "" expected actual
    where 
      expected = Right $ (Leaf $ Symbol "''1", [])
      actual = quote ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Symbol "quote", Leaf $ Int 1]]], [])

case_HALCar_List :: Assertion
case_HALCar_List =  assertEqual "" expected actual
    where 
      expected = Right $ (Leaf $ Int 1, [])
      actual = car ([List [Leaf $ Int 1, Leaf $ Int 2]], [])

case_HALCar_EmptyList :: Assertion
case_HALCar_EmptyList =  assertEqual "" expected actual
    where 
      expected = Left "car: 'Nil' Invalid argument type"
      actual = car ([Procedure [Leaf $ Symbol "quote", Leaf Nil]], [])

case_HALCar_Int :: Assertion
case_HALCar_Int =  assertEqual "" expected actual
    where 
      expected = Left "car: '1' Invalid argument type"
      actual = car ([Leaf $ Int 1], [])

case_HALCar_NestedList :: Assertion
case_HALCar_NestedList =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int  2, Leaf $ Int  3], [])
      actual = car ([List [List [Leaf $ Int  1, Leaf $ Int  2, Leaf $ Int  3], List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6]]], [])

case_HALCdr_List :: Assertion
case_HALCdr_List =  assertEqual "" expected actual
    where 
      expected = Right $ (Leaf $ Int 2, [])
      actual = cdr ([List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3]], [])

case_HALCdr_OneElemList :: Assertion
case_HALCdr_OneElemList =  assertEqual "" expected actual
    where 
      expected = Right $ (Leaf Nil, [])
      actual = cdr ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1]]], [])

case_HALCdr_EmptyList :: Assertion
case_HALCdr_EmptyList =  assertEqual "" expected actual
    where 
      expected = Left "cdr: 'Nil' Invalid argument type"
      actual = cdr ([Procedure [Leaf $ Symbol "quote", Leaf Nil]], [])

case_HALCdr_Int :: Assertion
case_HALCdr_Int =  assertEqual "" expected actual
    where 
      expected = Left "cdr: '1' Invalid argument type"
      actual = cdr ([Leaf $ Int 1], [])

case_HALCdr_NestedList :: Assertion
case_HALCdr_NestedList =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6], [])
      actual = cdr ([List [List [Leaf $ Int  1, Leaf $ Int  2, Leaf $ Int  3], List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6]]], [])

case_HALDefine_Ex1 :: Assertion
case_HALDefine_Ex1 =  assertEqual "" expected actual
    where 
      expected = Right (Leaf (Symbol "foo"), [(Leaf (Symbol "foo"), Leaf (Int 2))])
      actual = define ([Leaf (Symbol "foo"), Leaf (Int 2)], [])

case_HALDefine_Ex2 :: Assertion
case_HALDefine_Ex2 =  assertEqual "" expected actual
    where 
      expected = Right (Leaf (Symbol "add"), [(Leaf (Symbol "add"), lambda)])
      actual = define ([Leaf (Symbol "add"), lambda], [])
      lambda = Procedure [Leaf $ Symbol "lambda", List [Leaf $ Symbol "a", Leaf $ Symbol "b"], Procedure [Leaf $ Symbol "+", Leaf $ Symbol "a", Leaf $ Symbol "b"]]

case_HALEq_Ex1 :: Assertion
case_HALEq_Ex1 =  assertEqual "" expected actual
    where 
      expected = Right (Leaf $ ATrue, []) 
      actual = eq ([Leaf (Int 1), Leaf (Int 1)], [])

case_HALEq_Ex2 :: Assertion
case_HALEq_Ex2 =  assertEqual "" expected actual
    where 
      expected = Right (Leaf $ ATrue, []) 
      actual = eq ([Procedure [Leaf $ Symbol "+", Leaf (Int 2), Leaf (Int 2)] , Leaf (Int 4)], [])

case_HALEq_UsingDefine :: Assertion
case_HALEq_UsingDefine =  assertEqual "" expected actual
    where 
      expected = Right (Leaf $ ATrue, [(Leaf $ Symbol "foo", Leaf $ Int 42)]) 
      actual = eq ([Leaf (Symbol "foo"), Leaf (Int 42)], [(Leaf $ Symbol "foo", Leaf $ Int 42)])

case_HALEq_DifferentStrings :: Assertion
case_HALEq_DifferentStrings =  assertEqual "" expected actual
    where 
      expected = Right (Leaf $ AFalse, [])
      actual = eq ([Procedure [Leaf $ Symbol "quote", Leaf $ Symbol "foo"], Procedure [Leaf $ Symbol "quote", Leaf $ Symbol "bar"]], [])

case_HALEq_EmptyLists :: Assertion
case_HALEq_EmptyLists =  assertEqual "" expected actual
    where 
      expected = Right (Leaf $ ATrue, []) 
      actual = eq ([Procedure [Leaf $ Symbol "quote", Leaf $ Symbol "()"] , Procedure [Leaf $ Symbol "quote", Leaf $ Symbol "()"]], [])

case_HALOperation_DivByZero :: Assertion 
case_HALOperation_DivByZero = assertEqual "" expected actual
    where
      expected = Left "Operation: Division by Zero"
      actual = evaluate ([Procedure [Leaf $ Symbol "/", Leaf (Int 2), Procedure [Leaf $ Symbol "-", Leaf (Int 4), Leaf (Int 4)]]], [])

case_HALOperation_MultipleArgs :: Assertion 
case_HALOperation_MultipleArgs = assertEqual "" expected actual
    where
      expected = Right $ (Leaf (Int 36), [])
      actual = evaluate ([Procedure [Leaf $ Symbol "*", Leaf (Int 2), Leaf (Int 3), Leaf (Int 6)]], [])

case_HALOperation_Negative :: Assertion 
case_HALOperation_Negative = assertEqual "" expected actual
    where
      expected = Right $ (Leaf (Int (-120)), [])
      actual = evaluate ([Procedure [Leaf (Symbol "+"), Leaf (Int 0), Leaf (Int (- 150)), Leaf (Int 30)]], [])