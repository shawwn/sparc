.PHONY: all clean dist-clean test

all:
	@mkdir -p src/bcrypt/build
	@cd src/bcrypt/build && cmake .. && make

clean:
	@rm -rf src/bcrypt/build

dist-clean: clean
	@rm -rf bin/racket*

test: all
