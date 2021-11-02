--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HAL
--

module HAL where

data Atom = Symbol String | Int Integer | Nil

data Sexpr = Node [Sexpr] | Leaf Atom