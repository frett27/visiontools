---------------------------------------------------------------------
--          Open_Image - Ada95 Raster Image Data Library
--
--                     Copyright (C) 2000
--                     Jeffrey Creem
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public
-- License along with this library; if not, write to the
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,
-- Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this  unit  does not  by itself cause  the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file  might be covered by the  GNU Public License.
---------------------------------------------------------------------

--  <description>
--
--  This is the top level package of Open_Image library. Open Image
--  (Open as in Open Source) is a hierarchy of packages providing
--  general purpose image Input/Output and manipulation.
--
--  </description>
--
with Ada.Io_Exceptions;

package Open_Image is

   type Image_8_Bit_Type is mod 2**8;
   type Image_16_Bit_Type is mod 2**16;
   type Image_32_Bit_Type is mod 2**32;

   type Pixel_Value_Type is new Image_8_Bit_Type;

   type Color_Channels_Type is
      record
         Red   : Pixel_Value_Type;
         Green : Pixel_Value_Type;
         Blue  : Pixel_Value_Type;
      end record;


   --
   -- Unless some file format has some specific governing rules that
   -- override our default, all images have image coordinates 1,1
   -- in the upper left hand corner of the picture.
   --
   type Axis_Base_Type is range 0 .. 2**32;

   type Column_Index_Type is new Axis_Base_Type range 1 .. Axis_Base_Type'Last;
   type Row_Index_Type is new Axis_Base_Type range 1 .. Axis_Base_Type'Last;


   --
   -- Most packages in the hiearchy have no preferred storage structure
   -- for the pixel data however there are times when it is nice to
   -- have a single common predefined type. Greyscale_Pixel_Data_Type
   -- is this greyscale common type and Color_Pixel_Data_Type will
   -- be used for color data.
   --
   type Greyscale_Pixel_Data_Type is array (Column_Index_Type range <>, Row_Index_Type range <>) of Pixel_Value_Type;

   --
   -- Some image file formats have color palettes associated with them
   -- where the value stored in a pixel is actually a lookup into a full
   -- color palette table. Palette_Based_Pixel_Data_Type may be used
   -- for data of this type.
   --
   subtype Palette_Based_Pixel_Data_Type is Greyscale_Pixel_Data_Type;

   type Color_Pixel_Data_Type is array (Column_Index_Type range <>, Row_Index_Type range <>) of Color_Channels_Type;

   --
   -- Most of the I/O routines that are a part of Open_Image defined
   -- their own exceptions for errors specific to their formats however
   -- each of them may raise the standard IO_Exceptions for the normal
   -- "LRM" reasons.
   --
   Status_Error : exception renames Ada.Io_Exceptions.Status_Error;
   Mode_Error   : exception renames Ada.Io_Exceptions.Mode_Error;
   Name_Error   : exception renames Ada.Io_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.Io_Exceptions.Use_Error;
   Device_Error : exception renames Ada.Io_Exceptions.Device_Error;
   End_Error    : exception renames Ada.Io_Exceptions.End_Error;

   File_Format_Protocol_Violation_Error : exception;


   type Byte_Ordering_Type is
         (Little_Endian,
          Big_Endian);


end Open_Image;
