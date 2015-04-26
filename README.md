Simple Vision Tools for Ada Projects
============

![](https://travis-ci.org/frett27/visiontools.svg)

__Patrice Freydiere - 2014.5.11__

This is a library of Simple Vision Tools in Ada, for handling images in grey, binary and color (using RVB), 
a couple of simple functions are provided for :

- Mathematical Morphology
- Rescaling, Resampling
- Reading and writing from / to JPEG and PPM ascii format

This library is used for several personal projects i've been working on, but could be helpful for others.

The design is intentionnaly focused on simplicity, performance and reliability. Lot's of improvements could be done, especially in documentation, test units , and adding more functionnality.


Tested Environments :
=====================

- Raspberry PI (Model A) - ARM
- Windows 2000 / Windows 7, x86 , both 32-64 bits
- Linux x86


Usage :
=======

you must have gnat installed (use the Ada standard 2005). The Gcc Ada frontend has been tested succesfully on Raspberry PI Model B.



for compiling the project, launch the following commands from a shell :

`gprbuild vision.gpr` to compile the library

`gprbuild testvision.gpr` to compile tests



External Libraries :
====================

This library embedded libjpeg, and OpenImage 0.1, in a static way, so there are no external dependencies. Embedding thoses libraries prevent also issues on compilation options.

Compilation should run smoothly.



