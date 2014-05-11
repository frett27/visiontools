-----------------------------------------------------------------------
--         Simple JPEG Library - a libjpeg binding for ADA V2        --
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

with System;
with Ada.Streams;

package Simple_Jpeg_Lib is

   -- Jpeg handles
   type Jpeg_Handle_Decompress is private;
   type Jpeg_Handle_Compress is private;
   type Jpeg_Handle_Memory_Compress is private;
   type Jpeg_Memory_Buffer is private;


   -- Byte representation ...
   type Byte is range 0..255;
   for Byte'size use 8;

   -- Byte Array used for browsing buffers ...
   type Byte_Array is array (Natural range <>) of Byte;

   -- Byte Array containing line components(RVB or Grey Value,
   -- depending on the component number)
   type Component_Array is array (Natural range <>) of aliased Byte ;

   type Jpeg_Type is (RVB, Grey);
   subtype Dimension is Natural;

   -- Exception Definition

   Jpeg_Exception : exception;
   Unknown_Jpeg_Type:exception;


   -- Open a Jpeg File for Reading
   procedure Open_Jpeg (Nom         : in     String;
                        J           :    out Jpeg_Handle_Decompress;
                        Scale_Denom : in     Integer);

   -- Get Image information
   procedure Get_Image_Info (J              : in     Jpeg_Handle_Decompress;
                             Width , Height :    out Dimension;
                             JpegType       :    out Jpeg_Type);

   -- read the next line of the file
   function Read_Next_Line (J : in Jpeg_Handle_Decompress)
                          return Component_Array;

   procedure Close_Jpeg (J : in out Jpeg_Handle_decompress);


   -- Create a Jpeg File
   procedure Create_Jpeg (Nom            : in     String;
                          J              :    out Jpeg_Handle_Compress;
                          Width , Height :        Dimension;
                          jpegtype       :        Jpeg_Type);

   procedure Write_Line (J          : in Jpeg_Handle_Compress;
                         Components : in Component_Array);

   procedure Close_Jpeg(J: in out Jpeg_Handle_Compress);

   -- create memory Jpeg
   procedure Create_Jpeg_Memory (BufferSize     : in     Natural;
                                 J              :    out Jpeg_Handle_Memory_Compress;
                                 Width , Height :        Dimension;
                                 Jpegtype       :        Jpeg_type);

   procedure Write_Line (J          : in Jpeg_Handle_Memory_Compress;
                         Components : in Component_Array);

   function Close_Jpeg (J : in Jpeg_Handle_Memory_Compress)
                      return Jpeg_Memory_Buffer;

   -- Get the Buffer size (the Memory Jpeg File Size)
   function Get_Size (J : in Jpeg_Memory_Buffer) return Natural;
   function Get_Buffer (J : in Jpeg_Memory_Buffer) return Byte_Array;

   -- Free the allocated buffer
   procedure Free_Buffer (J : in out Jpeg_Memory_Buffer);


private

   -- Implementation part

   type Jpeg_Handle_Decompress is new System.address;
   type Jpeg_Handle_Compress is new System.address;
   type Jpeg_Handle_Memory_Compress is new System.Address;
   type Jpeg_Memory_Buffer is new System.Address;

   type Jpeg_Buffer is access System.Address; -- not used for instance


   -- Exit Error Call Back
   type Jpeg_Exit_Error_Handler_Access is
     access procedure (Info : System.Address);
   pragma Convention (C, JPEG_EXIT_ERROR_HANDLER_ACCESS);


end simple_jpeg_lib;
