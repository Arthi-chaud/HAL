--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HALError
--

module HALError where

import HALData
import Data.Either

type ErrorMessage = String

type MaybeExpr = Either ErrorMessage Expr
