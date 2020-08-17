.PHONY: default
default:
	sbcl --non-interactive --load build.lisp

.PHONY: clean_rounds
clean_rounds:
	./clean-rounds.sh

.PHONE: package
package:
	rm -rf target
	mkdir -p target
	cp *.lisp target/
	cp *.asd target/
	cp bot.json target/
	cp -f score-config target/
	cp -f speed-score-config target/
	cp Makefile target/
	cp Dockerfile target/
	cd target/ && zip bot.zip *
	mv target/bot.zip ./
