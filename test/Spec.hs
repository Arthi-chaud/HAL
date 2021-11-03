--
-- EPITECH PROJECT, 2021
-- haskellParser
-- File description:
-- Spec
--

import TestHAL
import TestAdvancedParser
import Test.HUnit (Assertion, assertEqual, Testable (test))
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
main :: IO ()
main = defaultMain
    [ specs ]

specs :: Test
specs = testGroup "Campaign-related functions"
    [
        testCase "AdvancedParser: case_parseChar_example_1" TestAdvancedParser.case_parseChar_example_1,
        testCase "AdvancedParser: case_parseChar_example_2" TestAdvancedParser.case_parseChar_example_2,
        testCase "AdvancedParser: case_parseChar_example_3" TestAdvancedParser.case_parseChar_example_3,
        testCase "AdvancedParser: case_parseChar_example_4" TestAdvancedParser.case_parseChar_example_4,
        testCase "AdvancedParser: case_parseAnyChar_example_1" TestAdvancedParser.case_parseAnyChar_example_1,
        testCase "AdvancedParser: case_parseAnyChar_example_2" TestAdvancedParser.case_parseAnyChar_example_2,
        testCase "AdvancedParser: case_parseAnyChar_example_3" TestAdvancedParser.case_parseAnyChar_example_3,
        testCase "AdvancedParser: case_parseOr_example_1" TestAdvancedParser.case_parseOr_example_1,
        testCase "AdvancedParser: case_parseOr_example_2" TestAdvancedParser.case_parseOr_example_2,
        testCase "AdvancedParser: case_parseOr_example_3" TestAdvancedParser.case_parseOr_example_3,
        testCase "AdvancedParser: case_parseAnd_example_1" TestAdvancedParser.case_parseAnd_example_1,
        testCase "AdvancedParser: case_parseAnd_example_2" TestAdvancedParser.case_parseAnd_example_2,
        testCase "AdvancedParser: case_parseAnd_example_3" TestAdvancedParser.case_parseAnd_example_3,
        testCase "AdvancedParser: case_parseAndWith_example_1" TestAdvancedParser.case_parseAndWith_example_1,
        testCase "AdvancedParser: case_parseMany_example_1" TestAdvancedParser.case_parseMany_example_1,
        testCase "AdvancedParser: case_parseMany_example_2" TestAdvancedParser.case_parseMany_example_2,
        testCase "AdvancedParser: case_parseSome_example_1" TestAdvancedParser.case_parseSome_example_1,
        testCase "AdvancedParser: case_parseSome_example_2" TestAdvancedParser.case_parseSome_example_2,
        testCase "AdvancedParser: case_parseInt_example" TestAdvancedParser.case_parseInt_example,
        testCase "AdvancedParser: case_parseWord" TestAdvancedParser.case_parseWord,
        testCase "AdvancedParser: case_parseWord_frontSpace," TestAdvancedParser.case_parseWord_frontSpace,

        --testCase "TestHAL.case_parseQuote" TestHAL.case_parseQuote,
        testCase "TestHAL.case_parseExpr" TestHAL.case_parseExpr,
        testCase "TestHAL.case_parseAtom" TestHAL.case_parseAtom
        --testCase "TestHAL.case_parseExpr_quoted_params" TestHAL.case_parseExpr_quoted_params,
        --testCase "TestHAL.case_parseExpr_nil" TestHAL.case_parseExpr_nil
    ]