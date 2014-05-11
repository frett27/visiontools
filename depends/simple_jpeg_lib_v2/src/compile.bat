rem
rem Simple script for compiling Ada Jpeg Bindings
rem 

gcc -c -g c_simple_jpeg_lib.c -I../Dependencies/jpeg-6b

rem gnatmake -c -g -gnatf -gnatov -gnaty simple_jpeg_lib.adb > report_log.txt
gnatmake -c -g -gnatf simple_jpeg_lib.adb > report_log.txt


pause
