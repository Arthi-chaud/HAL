--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HAL where
import Data.List

data Atom = Symbol String | Int Integer | Nil

data Expr = Node [Expr] | Leaf Atom

instance Eq Atom where
    (==) (Symbol a) (Symbol b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) Nil Nil = True
    (==) _  _ = False

instance Eq Expr where
    (==) (Leaf a) (Leaf b) = a == b
    (==) (Node a) (Node b) = a == b
    (==) _ _ = False

instance Show Atom where
    show (Symbol x) = show x
    show (Int x) = show x
    show Nil = "Nil"

instance Show Expr where
    show (Leaf x) = show x
    show (Node x) = '[' : intercalate ", " (map (show) x) ++ "]"
