##
## EPITECH PROJECT, 2019
## makefile
## File description:
## Makefile
##

NAME		=	hal

all:		$(NAME)

$(NAME):
			stack build --allow-different-user
			cp $(shell stack path --local-install-root)/bin/HAL ./$(NAME)


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