#!/bin/sh
cc -c ../src/c_simple_jpeg_lib.c
gnatmake -I../src test_jpeg_reader -largs c_simple_jpeg_lib.o -ljpeg
gnatmake -I../src test_jpeg_reader_memory -largs c_simple_jpeg_lib.o -ljpeg
gnatmake -I../src makethumbnails -largs c_simple_jpeg_lib.o -ljpeg

