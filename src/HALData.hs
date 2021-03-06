--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HALData where
import Data.List

data Atom = Symbol String | Int Integer | Index Int | Nil | ATrue | AFalse | ANothing

data Expr = Procedure [Expr] | Lambda ([Expr], Int) | Leaf Atom | List [Expr]

instance Eq Atom where
    (==) (Symbol a) (Symbol b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) (Index a) (Index b) = a == b
    (==) Nil Nil = True
    (==) ATrue ATrue = True
    (==) AFalse AFalse = True
    (==) ANothing ANothing = True
    (==) _  _ = False

instance Eq Expr where
    (==) (Leaf a) (Leaf b) = a == b
    (==) (Procedure a) (Procedure b) = a == b
    (==) (List a) (List b) = a == b
    (==) (Lambda a) (Lambda b) = a == b
    (==) _ _ = False

instance Show Atom where
    show (Symbol x) = x
    show (Int x) = show x
    show (Index x) = "Argument n° " ++ show x
    show Nil = "()"
    show ATrue = "#t"
    show AFalse = "#f"
    show ANothing = ""

instance Show Expr where
    show (Leaf x) = show x
    show (Procedure x) = '(' : printList x " " ++ ")"
    show (Lambda _) = "#<procedure>"
    show (List x) = HALData.showList (List x)

showList :: Expr -> String 
showList (List a) = case length a of
    2 -> case last a of
        (Leaf Nil) -> "(" ++ show (head a) ++ ")"
        _ -> showListAsTuple a
    _ -> case last a of
        (Leaf Nil) -> showListContent $ init a
        _ -> showListContent a
showList _ = show AFalse

showListAsTuple :: [Expr] -> String
showListAsTuple a =  '(' : printList a " . " ++ ")"

showListContent :: [Expr] -> String
showListContent a = '(' : printList a " " ++ ")"

printList :: [Expr] -> String -> String
printList list inter =  intercalate inter (map show list)