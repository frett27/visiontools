------------------------------------------------------------------------------
--                          Vision Library Tools                            --
--                                                                          --
--                         Copyright (C) 2004-2005                          --
--                                                                          --
--  Authors: Patrice Freydiere                                              --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
------------------------------------------------------------------------------


with Ada.Finalization;
with Unchecked_Deallocation;

generic

   type Pixel_Element is private;

package Images.Image is

   package AF renames Ada.Finalization;

   -- define the pixel type
   subtype Pixel_Value_Type is Pixel_Element;

   -- index de ligne d'une image
   subtype Row_Index_Type is Natural;

   -- index de colonne d'une image
   subtype Column_Index_Type is Natural;

   -- type d'une image
   type Unbounded_Image is new AF.Controlled with private;

   ----------
   -- Init --
   ----------

   --
   -- initialisation d'une image
   --
   procedure Init (Img           : in out Unbounded_Image;
                   Width, Height : in     Positive);

   ---------------
   -- Get_Pixel --
   ---------------

   --
   -- récupère la valeur d'un pixel
   --
   function Get_Pixel (M   : in Unbounded_Image;
                       Col :    Column_Index_Type;
                       Row :    Row_Index_Type)
                      return Pixel_Value_Type;

   ---------------
   -- Set_Pixel --
   ---------------

   --
   -- définit la valeur d'un pixel de l'image
   --
   procedure Set_Pixel (M     : in out Unbounded_Image;
                        Col   :        Column_Index_Type;
                        Row   :        Row_Index_Type;
                        Value :        Pixel_Value_Type);


   -------------------------
   -- Set_Pixel_Unbounded --
   -------------------------

   ---  Cette fonction permet de définir la valeur d'un pixel,
   ---  sans se soucier des bords ..
   ---  X, Y -> la position du pixel à définir
   ---  Value -> la valeur du pixel à positionner

   procedure Set_Pixel_Unbounded (M     : in out Unbounded_image;
                                  X,Y   : in     Integer;
                                  Value : in     pixel_value_type);

   -------------------------
   -- Get_Pixel_unbounded --
   -------------------------

   --- Cette fonction permet de lire la valeur d'un pixel d'une image.
   --- Si la lecture est réalisée
   --- en dehors de l'image, la valeur Default est retournée
   ---
   ---  X, Y -> la position du pixel à définir
   ---  Default -> la valeur du pixel à positionner

   function Get_Pixel_unbounded (Img     : in Unbounded_Image;
                                 X,Y     : in Integer;
                                 Default : in Pixel_Value_Type)
                               return Pixel_Value_Type ;

   ----------
   -- Rows --
   ----------

   -- Image rows
   function Rows (M : in Unbounded_Image) return Natural;

   -------------
   -- Columns --
   -------------

   -- getting Image columns number
   function Columns (M : in Unbounded_Image) return Natural;


   --------------
   -- Set_Size --
   --------------

   -- redéfinit la taille d'une image
   procedure Set_Size (Img          : in out Unbounded_Image;
                       Width,Height :        Positive);


   ------------
   -- Resize --
   ------------

   -- change l'image de taille
   function Resize (Img                   : in Unbounded_Image;
                    New_Width, New_Height :    Positive)
                  return Unbounded_Image;



private

   pragma Inline(Get_Pixel);
   pragma Inline(Set_Pixel);

   type Pixel_Array is array (Natural range <>,
                              Natural range <>) of Pixel_Value_Type;
   pragma Pack(Pixel_Array);

   type Pixel_Array_Access is access all Pixel_Array;

   Non_Aliased_Null_Data : Pixel_Array (2 .. 1, 2 .. 1);
   Null_Pixel_Data :
     aliased Pixel_Array := Non_Aliased_Null_Data;

   -- Contenu d'une image pouvant être partagé entre
   -- plusieures images
   type Content is record
      Counter : Natural := 1;
      Reference : Pixel_Array_Access := Null_Pixel_Data'Access;
   end record;

   type Content_Access is access all Content;

   -- type image
   type Unbounded_Image is new AF.Controlled with
      record
         Shallow_Copy : Boolean := False;
         Content : Content_Access  := null;
      end record;

   -- désallocation de l'image
   procedure Deallocate is
      new Unchecked_Deallocation
     (Pixel_Array, Pixel_Array_Access);

   -- désallocation du contenu de l'image
   procedure Deallocate is
      new Unchecked_Deallocation
     (Content, Content_Access);

   procedure Initialize (Object : in out Unbounded_Image);

   procedure Adjust (Object : in out Unbounded_Image);

   procedure Finalize (Object : in out Unbounded_Image);

   Image_Non_Vide : exception;

end Images.Image;
