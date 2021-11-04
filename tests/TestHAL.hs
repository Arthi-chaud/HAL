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
import HALError


case_HALCons_Leafs :: Assertion
case_HALCons_Leafs =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ List [Leaf $ Int 1, Leaf $ Int 2]
      actual = cons [Leaf $ Int 1, Leaf $ Int 2]

case_HALCons_LeafAndList :: Assertion
case_HALCons_LeafAndList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ List [Leaf $ Int 1, Leaf $ Int 2, Leaf $ Int 3]
      actual = cons [Leaf $ Int 1, Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]]


case_HALCons_ListAndList :: Assertion
case_HALCons_ListAndList =  assertEqual "Example 1" expected actual
    where 
      expected = Right $ List [List[Leaf $ Int 1], Leaf $ Int 2, Leaf $ Int 3]
      actual = cons [Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 1]], Procedure [Leaf $ Symbol "quote", Procedure [Leaf $ Int 2, Leaf $ Int 3]]]