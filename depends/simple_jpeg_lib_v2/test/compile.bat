rem
rem Simple script for compiling Ada Jpeg Bindings
rem 

gcc -c  ../src/c_simple_jpeg_lib.c -I../Dependencies/jpeg-6b

gnatmake -g -gnatf -gnatov -gnaty -I../src test_jpeg_reader -largs c_simple_jpeg_lib.o -ljpeg -bargs -E

gnatmake -g -gnatf -gnatov -gnaty -I../src test_jpeg_reader_memory -largs c_simple_jpeg_lib.o -ljpeg -bargs -E

gnatmake -g -I../src makethumbnails -largs c_simple_jpeg_lib.o -ljpeg -bargs -E

pause
