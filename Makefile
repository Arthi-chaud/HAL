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
			cp $(BUILD_FOLDER)/HAL ./$(NAME)


clean:
			rm -f $(NAME)

clean_tests:

fclean:		clean

re:			fclean all

tests_run:
			stack test --allow-different-user
tests_run_with_coverage:
			stack test --coverage --allow-different-user

.PHONY:	tests_run tests_run_with_coverage clean fclean all