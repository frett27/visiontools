.PHONY: clean
.PHONY: all
.PHONY: tests

ubuntu: all

all: clean
	gnatmake -d -p -P vision.gpr
	gnatmake -d -p -P testvision.gpr

clean:
	gnat clean -P testvision.gpr
	gnat clean -P vision.gpr

