##
## EPITECH PROJECT, 2019
## makefile
## File description:
## Makefile
##

NAME		=	hal


BUILD_FOLDER := $(shell stack --no-terminal path --local-install-root)/bin

all:		$(NAME)

$(NAME):
			find . 
			stack build --allow-different-user
ifeq ($(BUILD_FOLDER), /bin)	
			mv $(find / -name "HAL") ./$(NAME)
else
			mv $(BUILD_FOLDER)/HAL ./$(NAME)
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