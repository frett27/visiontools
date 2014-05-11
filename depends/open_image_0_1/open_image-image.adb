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

with Unchecked_Deallocation;

package body Open_Image.Image is



  procedure Set_Size (
        Image   : in out Unbounded_Image;
        Columns : in     Column_Index_Type;
        Rows    : in     Row_Index_Type             ) is

  begin

    if Image.Reference'Length(1) /=Columns or Image.Reference'Length(2) /=
      Rows then
       if Image.Reference /= Null_Pixel_Data'Access then
          Deallocate(Image.Reference);
       end if;

       Image.Reference := new Image_Type(1 .. Columns, 1 ..
                                         Rows);
    end if;

  end Set_Size;



  function Rows (
        Image : in     Unbounded_Image )
    return Row_Index_Type'Base is

  begin
    return Image.Reference'Length(2);
  end Rows;



  function Columns (
        Image : in     Unbounded_Image )
    return Column_Index_Type'Base is

  begin
    return Image.Reference'Length(1);
  end Columns;



  function Get_Pixel (
        Image : in     Unbounded_Image;
        Col   : in     Column_Index_Type;
        Row   : in     Row_Index_Type             )
    return Pixel_Element is

  begin
    return Image.Reference(Col, Row);
  end Get_Pixel;



  procedure Set_Pixel (
        Image : in out Unbounded_Image;
        Col   : in     Column_Index_Type;
        Row   : in     Row_Index_Type;
        Value : in     Pixel_Element           ) is

  begin
    Image.Reference(Col, Row) := Value;
  end Set_Pixel;





  procedure Initialize (
        Object : in out Unbounded_Image ) is

  begin
    Object.Reference := Null_Pixel_Data'access;
  end Initialize;


  procedure Adjust (
        Object : in out Unbounded_Image ) is

  begin

    if Object.Reference /= Null_Pixel_Data'access then
      Object.Reference := new Image_Type'(Object.Reference.all);
    end if;

  end Adjust;



  procedure Finalize (
        Object : in out Unbounded_Image ) is

  begin
    if Object.Reference /= Null_Pixel_Data'access then

      Deallocate(Object.Reference);
    end if;

  end Finalize;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Convert_To_Unbounded_Greyscale
  --
  -- PURPOSE    : Converts the given pixel data to an Unbounded_Greyscale
  --              image.s
  --
  -- NOTES      : None
  --
  -----------------------------------------------------------------------------

  procedure Convert_To_Unbounded_Greyscale (
        Pixel_Image_Data : in     Image_Type;
        Image  : in out Unbounded_Image            ) is

  begin

    if Image.Reference'Length(1) /= Pixel_Image_Data'
        Length(1) or
        Image.Reference'Length(2) /= Pixel_Image_Data'
        Length(2) then

      Deallocate(Image.Reference);

      Image.Reference := new Image_Type(1 ..
        Pixel_Image_Data'
        Length(1), 1 ..
        Pixel_Image_Data'
        Length(2));

    end if;

    Image.Reference.all := Pixel_Image_Data;

  end  Convert_To_Unbounded_Greyscale;


end Open_Image.Image;
