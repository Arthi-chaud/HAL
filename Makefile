##
## EPITECH PROJECT, 2019
## makefile
## File description:
## Makefile
##

NAME		=	hal
BUILD_FOLDER := $$(stack --no-terminal path --local-install-root)/bin

all:		$(NAME)

$(NAME):
			stack build --allow-different-user
			cp $$(find . -wholename "*bin/HAL") ./$(NAME)


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