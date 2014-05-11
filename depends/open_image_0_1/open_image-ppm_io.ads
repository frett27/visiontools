---------------------------------------------------------------------
--          Open_Image - Portable Pixmap (PPM) IO             
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
--  This package defines routines to allow for reading and writing
--  of portable pixmap (PPM) files. PPM is a simple standard for
--  storing color pixmap. It is capable of storing the pixel data either
--  as ASCII numeric values or in raw binary form.
--
--  </description>
--

with Open_Image;

package Open_Image.PPM_IO is

  --
  -- PPM files can store data in both ASCII numeric and binary format. The
  -- binary format will almost always result in a smaller file and faster
  -- file IO but the files are limited to 8 bits per color channel per pixel.
  --
  type Pixel_Value_Storage_Type is
    (ASCII_Data,
    Raw_Bits);

  Max_Value_For_Raw_Bits_File : constant := 255;  

  Not_A_PPM_File_Error                 : exception;  
  File_Format_Protocol_Violation_Error : exception renames Open_Image.File_Format_Protocol_Violation_Error;
     








  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Is_A_PPM_File
  --
  -- PURPOSE    : Checks the contents of the file to determine if it is PPM 
  --              file.
  --
  -- NOTES      : At the moment this just checks the "magic number" information
  --              in the file header. 
  --
  -----------------------------------------------------------------------------
  function Is_A_PPM_File (
        Name : in     String ) 
    return Boolean; 


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE : Read_PPM
  --
  -- PURPOSE   : Read a portable pixmap file from a filename specified in
  --             Name into the user structure Image.
  --
  -- NOTES     : In addition to the standard I/O exceptions, this procedure
  --             will raise Not_A_PPM_File_Error if it is determined that
  --             the file is not a PPM file.
  --
  -----------------------------------------------------------------------------

  generic

    type Image_Type is limited private;

    --
    -- The procedure Set_Size will be called before any calls to Set_Pixel.
    -- The Dimension passed in will be based on the information from the
    -- header of the file.
    --
    with procedure Set_Size (
          Image     : in out Image_Type;       
          Max_Color : in     Pixel_Value_Type; 
          Columns   : in     Column_Index_Type;   
          Rows      : in     Row_Index_Type    ); 

    --
    -- Set_Pixel will be called for every pixel of the data in the file.
    -- It is usually a good idea to structure your code so that a pragma
    -- inline on the procedure that is the generic actual parameter for
    -- this procedure is possible.
    --
    with procedure Set_Pixel (
          Image  : in out Image_Type;         
          Column : in     Column_Index_Type;    
          Row    : in     Row_Index_Type;    
          Value  : in     Color_Channels_Type ); 

    procedure Read_PPM (
          Name  : in     String;    
          Image : in out Image_Type ); 



  -----------------------------------------------------------------------------
  --
  -- PROCEDURE : Write_PPM
  --
  -- PURPOSE   : Creates a new PPM file in the file specified by Name based
  --             on the given Image data.
  --
  --             Max_Pixel_Value must contain the largest value in any channel
  --             of the data. No explicit checking is done during the write 
  --             of the data to verify that the data does not exceed this value
  --             but you will either get an exception (if certain run-time
  --             checks are enabled) or more likely result in an invalid
  --             PPM file.
  --  
  --             Store_Pixel_Value_As defines the format for the pixel
  --             data. If Max_Pixel_Value > Max_Value_For_Raw_Bits_File,
  --             then Store_Pixel_Value_As must be ASCII_Data.
  --
  -- NOTES     : In addition to the standard IO type exceptions, this procedure
  --             will raise File_Format_Protocol_Violation_Error if 
  --             Max_Pixel_Value > Max_Value_For_Raw_Bits_File and 
  --             Store_Pixel_Value_As is not ASCII_Data.
  --
  -----------------------------------------------------------------------------

  generic

    type Image_Type is limited private;

    --
    -- Get_Pixel will be called for every pixel of the data in the file.
    -- It is usually a good idea to structure your code so that a pragma
    -- inline on the procedure that is the generic actual parameter for
    -- this procedure is possible.
    --

    with function Get_Pixel (
          Image  : in     Image_Type;      
          Column : in     Column_Index_Type; 
          Row    : in     Row_Index_Type  ) 
      return Color_Channels_Type; 

    procedure Write_PPM (
          Name                  : in     String;                   
          Max_Pixel_Value       : in     Pixel_Value_Type;         
          Columns               : in     Column_Index_Type;           
          Rows                  : in     Row_Index_Type;           
          Store_Pixel_Values_As : in     Pixel_Value_Storage_Type; 
          Image                 : in     Image_Type                ); 

end Open_Image.PPM_IO;
