EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner --no-win

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile espuds.el

clean-elc:
	rm -f espuds.elc

.PHONY:	all test unit
