.PHONY: all bcrypt setup compile scm clean dist-clean test bench

all:
	@make bcrypt setup compile

setup:
	@bash bin/setup.sh


bcrypt: ./src/bcrypt/build/libbcrypt.$(shell uname -s | grep -qi darwin && echo dylib || echo so)

./src/bcrypt/build/libbcrypt.dylib:
	@mkdir -p src/bcrypt/build
	c++ -std=c++14 -shared -fPIC -o src/bcrypt/build/libbcrypt.dylib src/bcrypt/bcrypt.cc src/bcrypt/blowfish.cc

./src/bcrypt/build/libbcrypt.so:
	@mkdir -p src/bcrypt/build
	c++ -std=c++14 -shared -fPIC -o src/bcrypt/build/libbcrypt.so src/bcrypt/bcrypt.cc src/bcrypt/blowfish.cc


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
