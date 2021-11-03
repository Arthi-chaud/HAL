--
-- EPITECH PROJECT, 2021
-- haskellParser
-- File description:
-- Spec
--

import TestHAL
import Test.HUnit (Assertion, assertEqual, Testable (test))
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
main :: IO ()
main = defaultMain
    [ specs ]

specs :: Test
specs = testGroup "Campaign-related functions"
    [
        testCase "TestHAL.case_parseQuote" TestHAL.case_parseQuote,
        testCase "TestHAL.case_parseExpr" TestHAL.case_parseExpr
        --testCase "TestHAL.case_parseExpr_quoted_params" TestHAL.case_parseExpr_quoted_params,
        --testCase "TestHAL.case_parseExpr_nil" TestHAL.case_parseExpr_nil
    ]