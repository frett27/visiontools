/*
-----------------------------------------------------------------------
--         Simple JPEG Library - a libjpeg binding for ADA           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                           Freydiere P.                            --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------
*/

#include <stdio.h>
//#include <sys/types.h>
#include <jpeglib.h>

#include <setjmp.h> // for errors recovery


// handling errors in jpeg lib
typedef struct
{
  struct jpeg_error_mgr jerr; // std Error Handler
  jmp_buf jmpbuff; // buffer for recovering ... 
} myerrorhandler;


typedef myerrorhandler * myerrorhandler_ptr;


typedef struct  {
  j_decompress_ptr cinfo_ptr;
  void **abuffer;  //Row Array, The buffer is managed by the C part
  FILE *infile;
  int buffer_size; // Buffer size
  // struct jpeg_error_mgr jerr; // Error Handler

  myerrorhandler myerr; // my error handler
  int rowsleft; // Rows Left in the Row Array
  int curbuffer;
} jpeg_decompress_handle;

typedef struct
{
  j_compress_ptr cinfo_ptr;
  FILE *outfile;

  struct jpeg_error_mgr jerr;
  myerrorhandler myerr; // my error handler

} jpeg_compress_handle;

typedef struct
{
  int buffersize;
  JOCTET *buffer;
  JOCTET *nextfree;
} JPEGMemoryBuffer;

typedef struct
{
  j_compress_ptr cinfo_ptr;
  struct jpeg_error_mgr jerr;
  myerrorhandler myerr; // my error handler
  JPEGMemoryBuffer *m;

} jpeg_compress_memory_handle;


typedef JMETHOD(void,errorexitptr,(j_common_ptr cinfo)); // ptr to the exit error

// JPEG Reading

extern jpeg_decompress_handle *open_jpeg(char *szFileName,errorexitptr e, int scaledenom);
extern void *get_row(jpeg_decompress_handle *j);
extern int close_decompress_jpeg(jpeg_decompress_handle *j);

// information on the image
extern int get_image_width(jpeg_decompress_handle *j);
extern int get_image_height(jpeg_decompress_handle *j);
extern int get_image_num_components(jpeg_decompress_handle *j);

// JPEG Writing

extern jpeg_compress_handle *create_jpeg(char *szFileName,errorexitptr e,int Width,int Height,int nbcomp);
extern int write_row(jpeg_compress_handle *j,void *rowpointer);
extern int close_compress_jpeg(jpeg_compress_handle *j);

// Memory Created JPEG

extern jpeg_compress_memory_handle *create_jpeg_memory(int BufferSize ,errorexitptr e,int Width,int Height,int nbcomp);
extern int write_memory_row(jpeg_compress_memory_handle *j, void *rowpointer);
extern JPEGMemoryBuffer *close_compress_memory_jpeg(jpeg_compress_memory_handle *j);

extern int get_jpeg_memory_buffer_size(JPEGMemoryBuffer *j);
extern void *get_jpeg_memory_buffer(JPEGMemoryBuffer *j);

extern void free_jpeg_memory_buffer(JPEGMemoryBuffer *j);
