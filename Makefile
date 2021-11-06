##
## EPITECH PROJECT, 2019
## makefile
## File description:
## Makefile
##

NAME		=	hal
SHELL := /bin/bash

BUILD_FOLDER := $(shell stack --no-terminal path --local-install-root)/bin

all:		$(NAME)

$(NAME):
			stack build --allow-different-user
ifeq ($(BUILD_FOLDER), /bin)	
			find . -wholename "*bin/HAL"
			cp $(shell find . -wholename "*bin/HAL") ./$(NAME)
else
			cp $(BUILD_FOLDER)/HAL ./$(NAME)
endif


clean:
			rm -f $(NAME)

clean_tests:

fclean:		clean

re:			fclean all

tests_run:
			stack test
tests_run_with_coverage:
			stack test --coverage

.PHONY:	tests_run tests_run_with_coverage clean fclean all