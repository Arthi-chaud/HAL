--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HALData where
import Data.List

data Atom = Symbol String | Int Integer | Nil | ATrue | AFalse

data Expr = Procedure [Expr] | Leaf Atom | List [Expr]

instance Eq Atom where
    (==) (Symbol a) (Symbol b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) Nil Nil = True
    (==) ATrue ATrue = True
    (==) AFalse AFalse = True
    (==) _  _ = False

instance Eq Expr where
    (==) (Leaf a) (Leaf b) = a == b
    (==) (Procedure a) (Procedure b) = a == b
    (==) (List [Leaf Nil]) (List [Leaf Nil]) = True
    (==) _ _ = False

instance Show Atom where
    show (Symbol x) = show x
    show (Int x) = show x
    show Nil = "Nil"
    show ATrue = "#t"
    show AFalse = "#f"

instance Show Expr where
    show (Leaf x) = show x
    show (Procedure x) = '[' : printList x ", " ++ "]"
    -- If only 2 elem
    show (List x) = HALData.showList (List x)

showList :: Expr -> String 
showList (List a) = case length a of
    2 -> case last a of
        (Leaf Nil) -> '(' : (_showListContent $ init a) ++ ")"
        _ -> _showListAsTuple a
    _ -> '(' : (_showListContent a) ++ ")"
showList _ = show AFalse

_showListAsTuple :: [Expr] -> String
_showListAsTuple a =  '(' : printList a " . " ++ ")"

_showListContent :: [Expr] -> String
_showListContent a = '(' : printList a " " ++ ")"

printList :: [Expr] -> String -> String
printList list inter =  intercalate inter (map show list)