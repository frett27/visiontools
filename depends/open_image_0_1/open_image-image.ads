---------------------------------------------------------------------
--                      Open_Image - Image             
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
--  This package implements a generic unbounded image type.
--
--  </description>
--

with Ada.Finalization;
with Unchecked_Deallocation;
with Open_Image;

generic

  type Pixel_Element is private;
  type Image_Type is array (Column_Index_Type range <>,
    Row_Index_Type range <>) of Pixel_Element;

package Open_Image.Image is

  type Unbounded_Image is new Ada.Finalization.Controlled with private;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Set_Size
  --
  -- PURPOSE    : Sets the size of the given image to the given value.
  --
  -- NOTES      : The contents of the Image data after this call are undefined.
  --
  -----------------------------------------------------------------------------

  procedure Set_Size (
        Image   : in out Unbounded_Image;   
        Columns : in     Column_Index_Type; 
        Rows    : in     Row_Index_Type     ); 


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Rows
  --
  -- PURPOSE    : Returns the number of rows in the given image.
  --
  -- NOTES      : None 
  --
  -----------------------------------------------------------------------------

  function Rows (
        Image : in     Unbounded_Image ) 
    return Row_Index_Type'Base; 


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Columns
  --
  -- PURPOSE    : Returns the number of columns in the given image.
  --
  -- NOTES      : None 
  --
  -----------------------------------------------------------------------------

  function Columns (
        Image : in     Unbounded_Image ) 
    return Column_Index_Type'Base; 


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Get_Pixel
  --
  -- PURPOSE    : Returns the pixel value at the given row and column.
  --
  -- NOTES      : None 
  --
  -----------------------------------------------------------------------------

  function Get_Pixel (
        Image : in     Unbounded_Image;   
        Col   : in     Column_Index_Type; 
        Row   : in     Row_Index_Type     ) 
    return Pixel_Element; 


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Set_Pixel
  --
  -- PURPOSE    : Sets the pixel value at the given location.
  --
  -- NOTES      : None 
  --
  -----------------------------------------------------------------------------

  procedure Set_Pixel (
        Image : in out Unbounded_Image;   
        Col   : in     Column_Index_Type; 
        Row   : in     Row_Index_Type;    
        Value : in     Pixel_Element); 


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Convert_To_Unbounded_Image
  --
  -- PURPOSE    : Converts the given pixel data to an Unbounded_Image
  --
  -- NOTES      : None 
  --
  -----------------------------------------------------------------------------

  procedure Convert_To_Unbounded_Greyscale (
        Pixel_Image_Data : in     Image_Type; 
        Image  : in out Unbounded_Image            ); 




private


  package AF renames Ada.Finalization;


  pragma Inline(Get_Pixel, Set_Pixel);



  type Access_Pixel_Data_Type is access all Image_Type;

  procedure Deallocate is 
  new Unchecked_Deallocation
    (Image_Type, Access_Pixel_Data_Type);

  Non_Aliased_Null_Data : Image_Type (2 .. 1, 2 .. 1);

  Null_Pixel_Data :
    aliased Image_Type := Non_Aliased_Null_Data;

  type Unbounded_Image is new AF.Controlled with
    record
    Reference : Access_Pixel_Data_Type := Null_Pixel_Data' access;
  end record;


  procedure Initialize (
        Object : in out Unbounded_Image ); 

  procedure Adjust (
        Object : in out Unbounded_Image ); 

  procedure Finalize (
        Object : in out Unbounded_Image ); 



end Open_Image.Image;