--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- HALError
--

module HALError where

import HALData

data ErrorWapper a = Error String | Valid Expr

instance Show (ErrorWapper a) where
    show (Error s) = "*** ERROR : " ++ show s
    show (Valid s) = show s