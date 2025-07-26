##
## EPITECH PROJECT, 2025
## B-FUN-400-NAN-4-1-mypandoc-albane.merian
## File description:
## Makefile
##


NAME = mypandoc

COVLOC = .stack-work/install/*/*/*/hpc/$(PNAME)/$(UNIT)/*.tix

UNIT = mypandoc-test

PNAME = mypandoc

all: $(NAME)

$(NAME):
	stack build --allow-different-user
	cp `stack path --local-install-root`/bin/$(NAME) .
	cp mypandoc bonus
	cp -f mypandoc test/tester/

re: fclean all

clean:
	stack clean --allow-different-user

fclean: clean
	rm -f $(NAME)
	rm -rf *.cabal
	rm -rf *.lock
	rm -rf .stack-work
	rm -rf test/tester/mypandoc

norm:
	rm -f *.hi *.o
	rm -f $(NAME)
	coding-style . .
	cat coding-style-reports.log

tests_run: fclean
	stack test --coverage --allow-different-user
	mkdir -p test/coverage
	mv $(COVLOC) test/coverage

doc:
	stack haddock
