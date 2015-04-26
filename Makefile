.PHONY: clean
.PHONY: all
.PHONY: tests

ubuntu: all

all: clean
	gnatmake -d -p -P depends/simple_jpeg_lib_v2/c_simplejpeglib.gpr
	gnatmake -d -p -P depends/simple_jpeg_lib_v2/simplejpeglib.gpr
	gnatmake -d -p -P vision.gpr
	gnatmake -d -p -P testvision.gpr

clean:
	gnat clean -P testvision.gpr
	gnat clean -P vision.gpr

