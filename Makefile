##
## EPITECH PROJECT, 2019
## makefile
## File description:
## Makefile
##

NAME		=	hal


BUILD_FOLDER := $(shell stack path --local-install-root)/bin

all:		$(NAME)

$(NAME):
			stack build
			cp $(BUILD_FOLDER)/$(NAME) ./$(NAME)


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