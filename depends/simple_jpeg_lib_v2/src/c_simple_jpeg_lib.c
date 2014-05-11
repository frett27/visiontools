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
#include "c_simple_jpeg_lib.h"
#include <setjmp.h>


// Structure

/* Data destination object for compression


struct jpeg_destination_mgr {
  JOCTET * next_output_byte;
  size_t free_in_buffer;

  JMETHOD(void, init_destination, (j_compress_ptr cinfo));
  JMETHOD(boolean, empty_output_buffer, (j_compress_ptr cinfo));
  JMETHOD(void, term_destination, (j_compress_ptr cinfo));
};




struct jpeg_source_mgr {
  const JOCTET * next_input_byte;
  size_t bytes_in_buffer;

  JMETHOD(void, init_source, (j_decompress_ptr cinfo));
  JMETHOD(boolean, fill_input_buffer, (j_decompress_ptr cinfo));
  JMETHOD(void, skip_input_data, (j_decompress_ptr cinfo, long num_bytes));
  JMETHOD(boolean, resync_to_restart, (j_decompress_ptr cinfo, int desired));
  JMETHOD(void, term_source, (j_decompress_ptr cinfo));
};


*/

// Internal error handling routine

METHODDEF(void)
simple_jpeg_lib_error_handler(j_common_ptr cinfo)
{
  myerrorhandler_ptr j = (myerrorhandler_ptr) cinfo->err;

  longjmp(j->jmpbuff , 1); //recover from the error

}



// JPEG File Reading


//=====================================================================
//
//  fonction open_jpeg
//        Parameter : szFileName , nom du fichier JPEG
//
//       function return NULL if a failure occur, or
//        a pointer to the jpeg_decompress_handle if success
//
//=====================================================================


extern jpeg_decompress_handle *open_jpeg(char *szFileName, errorexitptr e,int scaledenom)
{
  FILE *infile;
  int i;
  jpeg_decompress_handle *j; // Ret Value


#ifdef DEBUG
  printf(" %s \n ",szFileName);

#endif

  // ouverture du fichier
  if ((infile = fopen(szFileName ,"rb"))==NULL)
    {
      return NULL; // l'image ne peut pas être ouverte
    };
  // allocation du handle

#ifdef DEBUG
  printf("allocation du handle de jpeg \n");
#endif


  // allocation de la structure de handle
  j = (jpeg_decompress_handle *)malloc(sizeof(jpeg_decompress_handle) );
  if (j == NULL)
    {
      // Close the opened file
      fclose(infile);
      return NULL; // NOK
    }


  j->cinfo_ptr = (struct jpeg_decompress_struct *) malloc(sizeof(struct jpeg_decompress_struct));
  if (j->cinfo_ptr == NULL )
    {
      printf("Impossible d'allouer la mémoire \n");
      fclose(infile);
      free(j);
      return NULL;
    }
  j->infile = infile; // Remember the file Handle

#ifdef DEBUG
  printf(" affectation du canal d'erreur \n");
#endif

  // les messages d'erreurs sont stockés dans une table
  // pointée par err->jpeg_message_table le dernier message
  // est le message err->last_jpeg_message
  j->cinfo_ptr->err  = jpeg_std_error(&(j->myerr.jerr));
  j->myerr.jerr.error_exit = &simple_jpeg_lib_error_handler;


#ifdef DEBUG
  printf(" initialisation de la librairie JPEG \n");
#endif

  if (setjmp(j->myerr.jmpbuff))
  {
    // error while jpeg_create_decompress ...
    free(j->cinfo_ptr);
    free(j);
    fclose(infile);
    return NULL;
  }

  jpeg_create_decompress(j->cinfo_ptr);

  /* invariant, create_decompress OK */

  if (setjmp(j->myerr.jmpbuff))
  {
    // error while jpeg_create_decompress ...
    jpeg_destroy_decompress(j->cinfo_ptr);
    free(j->cinfo_ptr);
    free(j);
    fclose(infile);
    return NULL;
  }

#ifdef DEBUG
  printf(" setting the in stream on the file ... \n");
#endif

  jpeg_stdio_src(j->cinfo_ptr, j->infile);

#ifdef DEBUG
  printf(" Reading JPEG header \n");
#endif

  jpeg_read_header(j->cinfo_ptr, TRUE);

  // Image Scaling
  j->cinfo_ptr->scale_num = 1;
  j->cinfo_ptr->scale_denom = scaledenom;


  jpeg_start_decompress(j->cinfo_ptr);

  // internal buffer allocation

#ifdef DEBUG
  printf("Buffer malloc .. \n");
#endif

  if ( ( j->abuffer = (void **)calloc(10,sizeof(void *) ) ) == NULL  ) // 10 lignes de buffer
    {
      // Buffer Allocation Failure
      // Freeing Allocated Structures

      // Call jpeg lib to free internal structures
      jpeg_abort_decompress(j->cinfo_ptr);
      jpeg_destroy_decompress(j->cinfo_ptr);
      free(j->cinfo_ptr);
      // Close the opened file
      fclose(j->infile);
      free(j);
      return NULL;
    }


  // Memorize Line Buffer Size
  j->buffer_size = j->cinfo_ptr->num_components *  j->cinfo_ptr->output_width ;

  for (i=0;i<10;i++)
    {
      if ( (j->abuffer[i] = (void *)malloc( j->cinfo_ptr->num_components *  j->cinfo_ptr->output_width ) )  ==NULL )
	{
	  // Mem Allocation Failure
	  if (i>1)
	    {
	      int k;
	      for (k = 0 ; k < i-1; k++ )
		free (j->abuffer[i]); // Free Line Buffer
	    }
	  free(j->abuffer);
	  // Call jpeg lib to free internal structures
	  jpeg_abort_decompress(j->cinfo_ptr);
	  jpeg_destroy_decompress(j->cinfo_ptr);

	  free(j->cinfo_ptr);
	  // Close the opened file
	  fclose(j->infile);
	  free(j);
	  return NULL;
	}
    }

  /* invariant, all the lines has been allocated .. */

  if (setjmp(j->myerr.jmpbuff))
  {
    // error while reading the first lines ...
    int k;
    for (k = 0 ; k < 10 ; k++ )
      free (j->abuffer[i]); // Free Line Buffer

    free(j->abuffer);
    // Call jpeg lib to free internal structures
    jpeg_abort_decompress(j->cinfo_ptr);
    jpeg_destroy_decompress(j->cinfo_ptr);

    free(j->cinfo_ptr);
    // Close the opened file
    fclose(j->infile);
    free(j);
    return NULL;
  }


  // push lines in the line Buffer
  j->rowsleft = jpeg_read_scanlines(j->cinfo_ptr,(JSAMPROW*)(j->abuffer),j->buffer_size);
  j->curbuffer = j->rowsleft ;

#ifdef DEBUG
  printf("Retour\n");
#endif


  return j; // ok, bien passé
}

//=====================================================================
//
//  procedure GetRow
//        paramètres : Jpeg file handle
//                     p : Ptr vers la ligne, contenant les composantes
//
//=====================================================================

extern void *get_row(jpeg_decompress_handle *j)
{
  void *p=NULL;
  if (j->rowsleft==0)
    {
      // If the line buffer is empty, then fill it with new datas
      if (j->cinfo_ptr->output_scanline < j->cinfo_ptr->output_height )
	{
	  if (setjmp(j->myerr.jmpbuff))
	    {
	      /* error happening in reading the lines .. */
	      return NULL;
	    }

	  j->rowsleft = jpeg_read_scanlines(j->cinfo_ptr,(JSAMPROW*)j->abuffer,j->buffer_size );
	  j->curbuffer = j->rowsleft ;
	}
      else
	{
	  // if the fill fail then, there is no more lines in the file
	  return NULL; // No lines any more
	}
    }
  // Return a buffer to the next line retrived
  p = j->abuffer[j->curbuffer - j->rowsleft--];
  return p; //ok
}


extern int get_image_width(jpeg_decompress_handle *j)
{
  return j->cinfo_ptr->output_width;
}

extern int get_image_height(jpeg_decompress_handle *j)
{
  return j->cinfo_ptr->output_height;
}

extern int get_image_num_components(jpeg_decompress_handle *j)
{
  return j->cinfo_ptr->num_components;
}


// close Decompress Handle

extern int close_decompress_jpeg(jpeg_decompress_handle *j)
{
  int i;
  // Close JPEG Library

  if (setjmp(j->myerr.jmpbuff))
    {
      /* error happening while ending the reading .. */
      /* free the ressources .. */
      for(i=0;i<10;i++)
	{
	  free(j->abuffer[i]);
	}

      // Free all the buffers
      free(j->abuffer);
      free(j->cinfo_ptr); //on libère le buffer de rawline

      fclose(j->infile); // Close the file
      free(j);

      return 0;
    }

  if ( j->cinfo_ptr->output_scanline < j->cinfo_ptr->output_width - 1 )
    {
      jpeg_abort_decompress(j->cinfo_ptr);
    } else
      {
	jpeg_finish_decompress(j->cinfo_ptr) ;
      };
  jpeg_destroy_decompress(j->cinfo_ptr );

  // Free all buffer lines
  for(i=0;i<10;i++)
    {
      free(j->abuffer[i]);
    }

  // Free all the buffers
  free(j->abuffer);
  free(j->cinfo_ptr); //on libère le buffer de rawline

  fclose(j->infile); // Close the file
  free(j);
  return 0; // ok
}


// =====================================================================
//
// Jpeg Writing functions
//
//    TODO : Simplify the functions
//
// =====================================================================

extern jpeg_compress_handle *create_jpeg(char *szFileName,
					 errorexitptr e,
					 int Width,
					 int Height,
					 int nbcomp)
{
  FILE *outfile;
  int i;
  jpeg_compress_handle *j;

#ifdef DEBUG
  printf(" %s \n ",szFileName);
  printf(" %d %d ",j,*j);
#endif
  // ouverture du fichier
  if ((outfile = fopen(szFileName ,"wb"))==NULL)
    {
      return NULL; // l'image ne peut pas être ouverte
    };
  // allocation du handle
#ifdef DEBUG
  printf("allocation du handle de jpeg \n");
#endif
  // allocation de la structure de handle
  j = (jpeg_compress_handle *)malloc(sizeof(jpeg_compress_handle) );
  j->cinfo_ptr = (struct jpeg_compress_struct *) malloc(sizeof(struct jpeg_compress_struct));
  if (j->cinfo_ptr == NULL )
    {
      printf("Impossible d'allouer la mémoire \n");
      fclose(outfile);
      return NULL;
    }

  j->outfile = outfile;



#ifdef DEBUG
  printf(" affectation du canal d'erreur \n");
#endif

  j->cinfo_ptr->err  = jpeg_std_error(&(j->jerr));
  if (e)  // Set Custom Handler if defined
    {
      j->jerr.error_exit = e;
    }

#ifdef DEBUG
  printf(" initialisation de la librairie JPEG \n");
#endif

  jpeg_create_compress(j->cinfo_ptr);
#ifdef DEBUG
  printf(" Affectation du flux d'entrée sur le fichier \n");
#endif
  jpeg_stdio_dest(j->cinfo_ptr, j->outfile);
#ifdef DEBUG
  printf(" Lecture du header JPEG \n");
#endif

  // Spécification des paramètres de compression

  j->cinfo_ptr->image_width = Width;
  j->cinfo_ptr->image_height = Height;
  j->cinfo_ptr->input_components = nbcomp;

  // à corriger en fonction de l'espace de couleurs
  j->cinfo_ptr->in_color_space = JCS_RGB;

#ifdef DEBUG
  printf(" set defaults JPEG \n");
#endif

  jpeg_set_defaults(j->cinfo_ptr);


#ifdef DEBUG
  printf(" Start Decompress JPEG \n");
#endif

  jpeg_start_compress(j->cinfo_ptr,TRUE);


#ifdef DEBUG
  printf(" Retour OK \n");
#endif

  return j; // ok, bien passé
}


extern int write_row(jpeg_compress_handle *j,void *rowpointer)
{
  JSAMPROW row_pointer[1];	/* pointer to a single row */
  int row_stride;			/* physical row width in buffer */

  row_stride = j->cinfo_ptr->image_width * j->cinfo_ptr->num_components;

  row_pointer[0] = (JSAMPROW)rowpointer;
  if (jpeg_write_scanlines(j->cinfo_ptr, row_pointer, 1) == 1)
    return 0;
     else return 1;
}


extern int close_compress_jpeg(jpeg_compress_handle *j)
{
  jpeg_finish_compress(j->cinfo_ptr);
  jpeg_destroy_compress(j->cinfo_ptr);
  // Free allocated structures
  free(j->cinfo_ptr);
  fclose(j->outfile);
  free(j);
}


// =====================================================================
//
// Jpeg Writing functions for in memory streams
//
// =====================================================================


void jpeglib_init_destination(j_compress_ptr cinfo)
{
#ifdef DEBUG
  printf("init_destination\n");
#endif
}

boolean jpeglib_empty_output_buffer(j_compress_ptr cinfo)
{
#ifdef DEBUG
  printf("empty_output_buffer\n");
#endif
  return TRUE;
}

void jpeglib_term_destination(j_compress_ptr cinfo)
{
#ifdef DEBUG
  printf("term_destination\n");
  printf(" nb octets restants %d ", cinfo->dest->free_in_buffer);
#endif
}


extern jpeg_compress_memory_handle *create_jpeg_memory(int BufferSize,
						       errorexitptr e,
						       int Width,
						       int Height,
						       int nbcomp)
{

  int i;
  jpeg_compress_memory_handle *j;
  struct jpeg_destination_mgr *s;

  // allocation du handle
#ifdef DEBUG
  printf("allocation du handle de jpeg \n");
#endif

  // allocation de la structure de handle
  j = (jpeg_compress_memory_handle *)malloc(sizeof(jpeg_compress_memory_handle) );
  j->cinfo_ptr = (struct jpeg_compress_struct *) malloc(sizeof(struct jpeg_compress_struct));
  if (j->cinfo_ptr == NULL )
    {
      printf("Fail to allocate Memory\n");
      return NULL;
    }


#ifdef DEBUG
  printf(" Set Error Call Back Procedure \n");
#endif

  j->cinfo_ptr->err  = jpeg_std_error(&(j->jerr));
  if (e)  // Set Custom Handler if defined
    {
      j->jerr.error_exit = e;
    }

#ifdef DEBUG
  printf("Init Compress Structure JPEG \n");
#endif

  jpeg_create_compress(j->cinfo_ptr);

#ifdef DEBUG
  printf("Set Input File Stream \n");
#endif

  s = (struct jpeg_destination_mgr *)malloc(sizeof(struct jpeg_destination_mgr));
  if (s == NULL)
    return ;


  s->next_output_byte = (JOCTET *)calloc(BufferSize,sizeof(JOCTET));
  s->free_in_buffer = BufferSize;

  // Set internal handle variables.

  j->m = (JPEGMemoryBuffer *)malloc(sizeof(JPEGMemoryBuffer));
  j->m->buffersize = BufferSize;
  j->m->buffer = s->next_output_byte;

  s->init_destination = &jpeglib_init_destination;
  s->empty_output_buffer = &jpeglib_empty_output_buffer;
  s->term_destination = &jpeglib_term_destination;

  j->cinfo_ptr->dest = s;

#ifdef DEBUG
  printf(" Lecture du header JPEG \n");
#endif

  // Spécification des paramètres de compression

  j->cinfo_ptr->image_width = Width;
  j->cinfo_ptr->image_height = Height;
  j->cinfo_ptr->input_components = nbcomp;

  // à corriger en fonction de l'espace de couleurs
  j->cinfo_ptr->in_color_space = JCS_RGB;

#ifdef DEBUG
  printf(" set defaults JPEG \n");
#endif

  jpeg_set_defaults(j->cinfo_ptr);


#ifdef DEBUG
  printf(" Start Decompress JPEG \n");
#endif

  jpeg_start_compress(j->cinfo_ptr,TRUE);


#ifdef DEBUG
  printf(" Retour OK \n");
#endif

  return j; // ok, bien passé
}


extern int write_memory_row(jpeg_compress_memory_handle *j, void *rowpointer)
{
  JSAMPROW row_pointer[1];	/* pointer to a single row */
  int row_stride;			/* physical row width in buffer */

  row_stride = j->cinfo_ptr->image_width * j->cinfo_ptr->num_components;

  row_pointer[0] = (JSAMPROW)rowpointer;
  if (jpeg_write_scanlines(j->cinfo_ptr, row_pointer, 1) == 1)
    return 0;
     else return 1;

}


extern JPEGMemoryBuffer *close_compress_memory_jpeg(jpeg_compress_memory_handle *j)
{
  JPEGMemoryBuffer *m;

  m = j->m;
  jpeg_finish_compress(j->cinfo_ptr);
  jpeg_destroy_compress(j->cinfo_ptr);

  // get the jpeg buffer size information
  m->nextfree = j->cinfo_ptr->dest->next_output_byte;

  // Free allocated structures
  free(j->cinfo_ptr);

  // Get Output JPEG Buffer

  free(j);

  return m;
}

// JPEGMemory management functions

extern void free_jpeg_memory_buffer(JPEGMemoryBuffer *j)
{
  // free the buffer
  free(j->buffer);
  // free the buffer structure
  free(j);
}

extern int get_jpeg_memory_buffer_size(JPEGMemoryBuffer *j)
{
  return j->nextfree - j->buffer;
}

extern void *get_jpeg_memory_buffer(JPEGMemoryBuffer *j)
{
  return j->buffer;
}
