Open_Image Readme.txt file

Version 0.1

Open Image is intended to be a library for reading and writing of various image
format types. It is intended that all of the code be 100% Ada. At the moment (and
perhaps forever) this library just reads and write Portable Pixmap (PPM) files.

The nice thing is that at least under Unix there are many many tools to convert almost
every image format from and to PPM files. In addition there are even tools to take
a sequence of PPM files and make an MPEG file.


The next image format I tackle will probably be some subset of TIFF.


All of the source file headers contain the standard GNAT based Run Time Version
of the GNU General Public License with the exception that allows these routines
to be included in a program without making the program GPL. I'd like to have the
entire library have this license so for example I will not be pulling the existing
PNG_IO Ada code (available at http://www.elec.rdg.ac.uk/png io.html) in since it is
pure GPL. Note that if pure GPL code is not a problem for you that is a very nice library.


I have not done extensive testing on this library other than the test_ppm.adb program
that is in the distribution (along with a little checking of files in GIMP).

I am considering in future versions making the Row and Colomn Coordinate Types
independant to reduce chances of programming errors.

Let me know what you think.

jeff@thecreems.com

