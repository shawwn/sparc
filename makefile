.PHONY: all bcrypt setup compile scm clean dist-clean test bench

all:
	@make bcrypt setup compile

setup:
	@bash bin/setup.sh


bcrypt: ./src/bcrypt/build

./src/bcrypt/build:
	@mkdir -p src/bcrypt/build
	@cd src/bcrypt/build && cmake .. && make


compile: setup ./compiled

./compiled:
	@bin/racket/bin/raco make *.scm


scm:
	@rm -rf compiled
	@make compile

clean:
	@rm -rf compiled

dist-clean: clean
	@rm -rf src/bcrypt/build
	@rm -rf bin/racket*

test: bcrypt setup compile
	@bin/arc test.arc

bench:
	@make scm
	@time make test
	@time make test
	@time make test
	@time make test
	@time make test
