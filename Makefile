.PHONY: default
default:
	sbcl --load build.lisp

.PHONY: clean_rounds
clean_rounds:
	./clean-rounds.sh
