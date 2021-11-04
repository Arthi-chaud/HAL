--
-- EPITECH PROJECT, 2021
-- haskellParser
-- File description:
-- Spec
--

import TestHALParser
import TestAdvancedParser
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

        testCase "TestHALParser: case_parseQuote" TestHALParser.case_parseQuote,
        testCase "TestHALParser: case_parseQuote_sizeOneList" TestHALParser.case_parseQuote_sizeOneList,
        testCase "TestHALParser: case_parseExpr" TestHALParser.case_parseExpr,
        testCase "TestHALParser: case_parseAtom" TestHALParser.case_parseAtom,
        testCase "TestHALParser: case_parseExpr_quoted_params" TestHALParser.case_parseExpr_quoted_params,
        testCase "TestHALParser: case_parseExpr_nil" TestHALParser.case_parseExpr_nil,
        testCase "TestHALParser: case_parseExpr_quoteOnQuote" TestHALParser.case_parseExpr_quoteOnQuote,
        testCase "TestHALParser: case_parseExpr_quoteOnQuote2" TestHALParser.case_parseExpr_quoteOnQuote2,
        testCase "TestHALParser: case_parseExpr_quoteOnQuote3" TestHALParser.case_parseExpr_quoteOnQuote3,
        testCase "TestHALParser: case_parseExpr_quoteOnQuote3bis" TestHALParser.case_parseExpr_quoteOnQuote3bis,

        testCase "TestHAL: case_HALCons_Leafs" TestHAL.case_HALCons_Leafs,
        testCase "TestHAL: case_HALCons_LeafAndList" TestHAL.case_HALCons_LeafAndList,
        testCase "TestHAL: case_HALCons_ListAndLeaf" TestHAL.case_HALCons_ListAndLeaf,
        testCase "TestHAL: case_HALCons_ListAndList" TestHAL.case_HALCons_ListAndList,
        testCase "TestHAL: case_HALQuote_List" TestHAL.case_HALQuote_List, 
        testCase "TestHAL: case_HALQuote_Int" TestHAL.case_HALQuote_Int, 
        testCase "TestHAL: case_HALQuote_QuoteOnQuote" TestHAL.case_HALQuote_QuoteOnQuote,
        testCase "TestHAL: case_HALQuote_QuoteOnQuoteonQuote" TestHAL.case_HALQuote_QuoteOnQuoteonQuote,
        testCase "TestHAL: case_HALCar_List" TestHAL.case_HALCar_List,  
        testCase "TestHAL: case_HALCar_EmptyList" TestHAL.case_HALCar_EmptyList, 
        testCase "TestHAL: case_HALCar_Int" TestHAL.case_HALCar_Int ,
        testCase "TestHAL: case_HALCar_NestedList" TestHAL.case_HALCar_NestedList,
        testCase "TestHAL: case_HALCdr_OneElemList" TestHAL.case_HALCdr_OneElemList,
        testCase "TestHAL: case_HALCdr_List" TestHAL.case_HALCdr_List,  
        testCase "TestHAL: case_HALCdr_EmptyList" TestHAL.case_HALCdr_EmptyList, 
        testCase "TestHAL: case_HALCdr_Int" TestHAL.case_HALCdr_Int,
        testCase "TestHAL: case_HALCdr_NestedList" TestHAL.case_HALCdr_NestedList
    ]