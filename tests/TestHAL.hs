--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- TestHAL
--

module TestHAL where

import HAL
import HALData
import HALParser
import AdvancedParser
import Test.HUnit (Assertion, assertEqual, assertFailure)


case_HALCons_Leafs :: Assertion
case_HALCons_Leafs =  assertEqual "" expected actual
    where 
      expected = Right (List [Leaf $ Int 1, Leaf $ Int 2], [])
      actual = cons ([Leaf $ Int 1, Leaf $ Int 2], [])

case_HALCons_LeafAndLeaf:: Assertion
case_HALCons_LeafAndLeaf =  assertEqual "" expected actual
    where 
      expected = Right (List [Leaf $ Int 1, Leaf Nil], [])
      actual = HAL.evaluate ([Procedure [Leaf $ Symbol "cons", Leaf $ Int 1, Procedure [Leaf $ Symbol "quote", Leaf Nil]]], [])

case_HALCons_LeafAndList :: Assertion
case_HALCons_LeafAndList =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3, Leaf Nil], [])
      actual = cons ([Leaf $ Int 1, Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]], [])

case_HALCons_ListAndList :: Assertion
case_HALCons_ListAndList =  case runParser parseExpr "(cons '(1 2 3) '(4 5 6))" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right (List [List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3, Leaf Nil], Leaf $ Int 4, Leaf $ Int 5, Leaf $ Int 6 , Leaf Nil], [])
        actual = HAL.evaluate ([expr], [])

case_HALCons_ListAndLeaf :: Assertion
case_HALCons_ListAndLeaf =  assertEqual "" expected actual
    where 
      expected = Right $ (List [List[Leaf $ Int 2], Leaf $ Int 1], [])
      actual = cons ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2]], Leaf $ Int 1], [])


case_HALCons_ListAndList2 :: Assertion
case_HALCons_ListAndList2 =  assertEqual "" expected actual
    where 
      expected = Right $ (List [List[Leaf $ Int 1], Leaf $ Int 2, Leaf $ Int 3, Leaf $ Nil], [])
      actual = cons ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1]], Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]], [])

case_HALQuote_List :: Assertion
case_HALQuote_List =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3, Leaf $ Nil], [])
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
      expected = Left "car: '()' Invalid argument type"
      actual = car ([Procedure [Leaf $ Symbol "quote", Leaf Nil]], [])

case_HALCar_Int :: Assertion
case_HALCar_Int =  assertEqual "" expected actual
    where 
      expected = Left "car: '1' Invalid argument type"
      actual = car ([Leaf $ Int 1], [])

case_HALCar_NestedList :: Assertion
case_HALCar_NestedList =  case runParser parseExpr "(car (cons '(1 2 3) '(4 5 6)))" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right (List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3, Leaf Nil], [])
        actual = HAL.evaluate ([expr], [])

case_HALCdr_List :: Assertion
case_HALCdr_List =  assertEqual "" expected actual
    where 
      expected = Right (List [Leaf $ Int 2, Leaf $ Int 3], [])
      actual = cdr ([List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3]], [])

case_HALCdr_OneElemList :: Assertion
case_HALCdr_OneElemList =  assertEqual "" expected actual
    where 
      expected = Right (Leaf Nil, [])
      actual = cdr ([Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1, Leaf Nil]]], [])

case_HALCdr_EmptyList :: Assertion
case_HALCdr_EmptyList =  assertEqual "" expected actual
    where 
      expected = Left "cdr: '()' Invalid argument type"
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

case_HALCdr_NestedList2 :: Assertion
case_HALCdr_NestedList2 =  assertEqual "" expected actual
    where 
      expected = Right $ (List [Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6], [])
      actual = cdr ([List [List [Leaf $ Int  1, Leaf $ Int  2, Leaf $ Int  3], Leaf $ Int  4, Leaf $ Int  5, Leaf $ Int  6]], [])


case_HALDefine_Ex1 :: Assertion
case_HALDefine_Ex1 =  assertEqual "" expected actual
    where 
      expected = Right (Leaf (Symbol "foo"), [(Leaf (Symbol "foo"), Leaf (Int 2))])
      actual = define ([Leaf (Symbol "foo"), Leaf (Int 2)], [])

case_HALDefine_Ex2 :: Assertion
case_HALDefine_Ex2 =  assertEqual "" expected actual
    where 
      expected = Right (Leaf (Symbol "add"), [(Leaf (Symbol "add"), Lambda ([Leaf $ Symbol "+", Leaf $ Index 0, Leaf $ Index 1], 2))])
      actual = define ([Leaf (Symbol "add"), lambda], [])
      lambda = Procedure [Leaf $ Symbol "lambda", Procedure [Leaf $ Symbol "a", Leaf $ Symbol "b"], Procedure [Leaf $ Symbol "+", Leaf $ Symbol "a", Leaf $ Symbol "b"]]

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
      actual = evaluate ([Procedure [Leaf $ Symbol "div", Leaf (Int 2), Procedure [Leaf $ Symbol "-", Leaf (Int 4), Leaf (Int 4)]]], [])

case_HALDiv_FromString :: Assertion 
case_HALDiv_FromString = case runParser parseAllExpr "(div 4 2)" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right ([Leaf $ Int 2], [])
        actual = HAL.evaluateAll (expr, [])

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

case_HALComparison :: Assertion 
case_HALComparison = assertEqual "" expected actual
    where
      expected = Right $ (Leaf ATrue, [])
      actual = evaluate ([Procedure [Leaf (Symbol ">"), Leaf (Int 0), Leaf (Int (- 150))]], [])

case_HALComparison2 :: Assertion 
case_HALComparison2 = assertEqual "" expected actual
    where
      expected = Right (Leaf AFalse, [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "<"), Leaf (Int 0), Leaf (Int (- 150))]], [])

case_HALComparisonWrongType :: Assertion 
case_HALComparisonWrongType = assertEqual "" expected actual
    where
      expected = Left "Operation: '12' Invalid argument type"
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "<"), Procedure [Leaf $ Symbol "quote", Leaf (Symbol "12")], Leaf (ATrue )]], [])

case_HALComparisonDefineValue :: Assertion 
case_HALComparisonDefineValue = assertEqual "" expected actual
    where
      expected = Right (Leaf ATrue, [(Leaf (Symbol "foo"), Leaf (Int 1))])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "<"), Leaf (Symbol "foo"), Leaf (Int 3)]], [(Leaf (Symbol "foo"), Leaf (Int 1))])


case_HALAtomEx1 :: Assertion 
case_HALAtomEx1 = assertEqual "" expected actual
    where
      expected = Right (Leaf ATrue, [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "atom?"), Procedure [Leaf (Symbol "quote"), Leaf (Symbol "foo")]]], [])

case_HALAtomEx2 :: Assertion 
case_HALAtomEx2 = assertEqual "" expected actual
    where
      expected = Right (Leaf AFalse , [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "atom?"), Procedure [Leaf (Symbol "quote"), Procedure [Leaf $ Symbol "1",  Leaf $ Symbol "2",  Leaf $ Symbol "3"]]]], [])

case_HALAtomEx3 :: Assertion 
case_HALAtomEx3 = assertEqual "" expected actual
    where
      expected = Right (Leaf ATrue, [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "atom?"), Procedure [Leaf (Symbol "quote"), Leaf (Symbol "()")]]], [])

case_HALCond_NothingTrue :: Assertion 
case_HALCond_NothingTrue = assertEqual "" expected actual
    where
      expected = Right (Leaf ANothing, [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "cond"), expr]], [])
      expr = Procedure [Leaf AFalse, Leaf $ Int 1]

case_HALCond_OneTrue :: Assertion 
case_HALCond_OneTrue = assertEqual "" expected actual
    where
      expected = Right (Leaf $ Int 3, [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "cond"), expr]], [])
      expr = Procedure [Leaf ATrue, Leaf $ Int 3]

case_HALCond_NoParam :: Assertion 
case_HALCond_NoParam = assertEqual "" expected actual
    where
      expected = Left "cond: Invalid argument count"
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "cond")]], [])

case_HALCond_InvalidParam :: Assertion 
case_HALCond_InvalidParam = assertEqual "" expected actual
    where
      expected = Left "cond: [OK] Invalid argument type"
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "cond"), Leaf $ Symbol "OK"]], [])

case_HALCond_Ex1 :: Assertion 
case_HALCond_Ex1 = assertEqual "" expected actual
    where
      expected = Right (Leaf $ Int 2, [])
      actual = HAL.evaluate ([Procedure [Leaf (Symbol "cond"), expr1, expr2]], [])
      expr1 = Procedure [Leaf AFalse, Leaf $ Int 1]
      expr2 = Procedure [Leaf ATrue, Procedure [Leaf $ Symbol "+", Leaf $ Int 1, Leaf $ Int 1]]

case_HALCond_Ex2 :: Assertion 
case_HALCond_Ex2 = case runParser parseExpr "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right (Leaf $ Symbol "here", [])
        actual = HAL.evaluate ([expr], [])

case_HALLambda_Ex1 :: Assertion 
case_HALLambda_Ex1 = assertEqual "" expected actual
      where
        expected = Right (Lambda ([Leaf $ Symbol "+", Leaf $ Index 0, Leaf $ Index 1], 2), [])
        actual = lambda ([Procedure [Leaf $ Symbol "a", Leaf $ Symbol "b"], Procedure [Leaf $ Symbol "+", Leaf $ Symbol "a", Leaf $ Symbol "b"]], [])

case_HALLambda_Ex1Evaluation :: Assertion 
case_HALLambda_Ex1Evaluation = assertEqual "" expected actual
      where
        expected = Right (Leaf $ Int 7, [])
        actual =  HAL.evaluate ([Lambda ([Leaf $ Symbol "+", Leaf $ Index 0, Leaf $ Index 1], 2), Leaf $ Int 5, Leaf $ Int 2], [])

case_HALLambda_Ex1EvaluationFromString :: Assertion 
case_HALLambda_Ex1EvaluationFromString = case runParser parseExpr "((lambda (a) (+ a foo)) bar)" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Procedure [
                      Procedure [Leaf $ Symbol "lambda", 
                          Procedure [Leaf $ Symbol "a"],
                          Procedure [Leaf $ Symbol "+", Leaf $ Symbol "a", Leaf $ Symbol "foo"]],
                      Leaf $ Symbol "bar"
                  ]
        actual = expr

case_HALLambda_DefinedParam :: Assertion 
case_HALLambda_DefinedParam = case runParser parseExpr "((lambda (a) (+ a foo)) bar)" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right (Leaf $ Int 63, env)
        actual = HAL.evaluate ([expr], env)
        env =  [(Leaf $ Symbol "foo", Leaf $ Int 42), (Leaf $ Symbol "bar", Leaf $ Int 21)]

case_HALLambda_DefinedParamFull :: Assertion 
case_HALLambda_DefinedParamFull = case runParser parseAllExpr "(define foo 42) (define bar 21) ((lambda (a) (+ a foo)) bar)" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right ([Leaf $ Symbol "foo", Leaf $ Symbol "bar", Leaf $ Int 63], env)
        actual = HAL.evaluateAll (expr, [])
        env =  [(Leaf $ Symbol "foo", Leaf $ Int 42), (Leaf $ Symbol "bar", Leaf $ Int 21)]

case_HALDefinedLambda1 :: Assertion 
case_HALDefinedLambda1 = case runParser parseAllExpr "(define foo (lambda (a b) (+ a b)))" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right ([Leaf $ Symbol "foo"], [(Leaf $ Symbol "foo", Lambda ([Leaf $ Symbol "+", Leaf $ Index 0, Leaf $ Index 1], 2))])
        actual = HAL.evaluateAll (expr, [])


case_HALCallDefinedLambda :: Assertion 
case_HALCallDefinedLambda = case runParser parseAllExpr "(add 1 2)" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right ([Leaf $ Int 3], env)
        actual = HAL.evaluateAll (expr, env)
        env = [(Leaf $ Symbol "add", Lambda ([Leaf $ Symbol "+", Leaf $ Index 0, Leaf $ Index 1], 2))]

case_HALLet :: Assertion 
case_HALLet = case runParser parseAllExpr "(let ((a 2) (b (+ 1 2))) (+ a b))" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right ([Leaf $ Int 5], [])
        actual = HAL.evaluateAll (expr, [])

case_HALDefineLambda :: Assertion 
case_HALDefineLambda = case runParser parseAllExpr "(define (sub a b) (- a b))" of
    Nothing -> assertFailure "Parsing failed"
    Just (expr, _) -> assertEqual "" expected actual
      where
        expected = Right ([Leaf $ Symbol "sub"], [(Leaf $ Symbol "sub",  Lambda ([Leaf $ Symbol "-", Leaf $ Index 0, Leaf $ Index 1], 2))])
        actual = HAL.evaluateAll (expr, [])

-- case_HALCaro4TheWin :: Assertion
-- case_HALCaro4TheWin = case runParser parseAllExpr "(define (append l1 l2) (cond ((eq? l1 '()) l2) (#t (cons (car l1) (append (cdr l1) l2)))))" of
--     Nothing -> assertFailure "Parsing failed"
--     Just (expr, _) -> assertEqual "" expected actual
--       where
--         expected = Right ([Leaf $ Symbol "append"], [(Leaf $ Symbol "sub",  Lambda ([Leaf $ Symbol "-", Leaf $ Index 0, Leaf $ Index 1], 2))])
--         actual = HAL.evaluateAll (expr, [])