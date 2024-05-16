##
## EPITECH PROJECT, 2022
## wolfram
## File description:
## Makefile
##

all:
	stack build
	cp $(shell stack path --local-install-root)/bin/wolfram .

clean:

fclean:

re:	fclean	all
