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

package body Images.Image is

   ------------------------------------------------------------------------
   -- Gestion de l'objet image

   ----------
   -- Init --
   ----------
   -- Initialisation d'une image
   procedure Init (Img           : in out Unbounded_Image;
                   Width, Height : in     Positive) is
   begin

      -- Allocation de la mémoire en fonction de la taille de l'image demandée
      if Img.Content /= null then
         raise Image_Non_Vide;
      end if;

      -- allocation d'un nouveau contenu
      Img.Content := new Content'(Counter => 1,
                                  Reference =>
                                  new Pixel_Array (1 .. Width, 1 .. Height));
      Img.Shallow_Copy := False;
   end Init;

   function Fork(C : Content_Access) return Content_Access
   is
      Tmp : Content_Access := new Content;
   begin
      Tmp.Counter := 1;
      Tmp.Reference := new Pixel_Array(1..C.Reference'Length(1),
                                       1..C.Reference'Length(2));
      Tmp.Reference.all := C.Reference.all;
      C.Counter := C.Counter - 1;
      return Tmp;
   end Fork;

   -- release the content if the image is not shared
   procedure Release_Content(Object : in out Unbounded_Image) is
   begin

      if Object.Content /= null then

         Object.Content.Counter := Object.Content.Counter - 1;

         -- desallocation du contenu si personne ne
         -- l'utilise
         if Object.Content.Counter = 0 then
            if Object.Content.Reference /= Null_Pixel_Data'Access then
               Deallocate(Object.Content.Reference);
            end if;
            Deallocate(Object.Content);
         end if;

         Object.Content := null;
      end if;

      Object.Shallow_Copy := False;

   end release_content;

   --
   -- redefine the image size
   ---
   procedure Set_Size (Img           : in out Unbounded_Image;
                       Width, Height : in     Positive) is
   begin

      if Img.Content /= null then
         Release_Content(Img);
      end if;

      -- allocate a new content
      Img.Content := new Content'(Counter => 1,
                                  Reference =>
                                  new Pixel_Array (1 .. Width, 1 .. Height));
      Img.Shallow_Copy := False;

   end Set_Size;


   procedure Initialize (Object : in out Unbounded_Image) is
   begin
      Object.Content := null;
   end Initialize;

   procedure Adjust (Object : in out Unbounded_Image) is
   begin
      -- par défaut, si l'image est initialisée,
      -- on partage le contenu

      if Object.Content /= null then
         Object.Shallow_Copy := True;
         Object.Content.Counter := Object.Content.Counter + 1;
      end if;

   end Adjust;

   procedure Finalize (Object : in out Unbounded_Image ) is
   begin
      Release_content(Object);
   end Finalize;


   -----------------------------------------------------------------------
   -- Fonctions de manipulation de l'image
   --

   function Rows (M : in Unbounded_Image)
                 return Natural is
   begin
      return M.Content.Reference'Length(2);
   end Rows;

   function Columns (M : in Unbounded_Image)
                    return Natural is
   begin
      return M.Content.Reference'Length(1);
   end Columns;

   function Get_Pixel (M   : in Unbounded_Image;
                       Col : in Column_Index_Type;
                       Row : in Row_Index_Type)
                      return Pixel_Value_Type is
   begin
      return M.Content.Reference(Col, Row);
   end Get_Pixel;

   procedure Set_Pixel (M     : in out Unbounded_Image;
                        Col   : in     Column_Index_Type;
                        Row   : in     Row_Index_Type;
                        Value : in     Pixel_Value_Type) is
   begin
      -- si l'image est partagée, on la duplique
      if M.Shallow_Copy = True then
         M.Content := Fork(M.Content);
         M.Shallow_Copy := False;
      end if;
      M.Content.Reference (Col, Row) := Value;
   end Set_Pixel;


  procedure Set_Pixel_Unbounded(M : in out Unbounded_image;
                                X,Y:in Integer;
                                Value : in pixel_value_type) is
   begin
      if X>=1 and X<=Integer(Columns(M)) then
         if Y>=1 and Y<=Integer(Rows(M)) then
            Set_Pixel(M,Column_Index_Type(X),Row_Index_Type(Y),value);

         end if;
      end if;
   end;

  function Get_Pixel_unbounded(Img:in Unbounded_Image;
                               X,Y: in Integer ;
                               Default:in Pixel_Value_Type) return Pixel_Value_Type is
    RetVal : pixel_value_type := default;
   begin
      if X>=1 and X<=Integer(Columns(Img)) then
         if Y>=1 and Y<=Integer(rows(Img)) then
            Retval := Get_Pixel(Img,Column_Index_Type(X),Row_Index_Type(Y));
         end if;
      end if;
      return Retval;
   end;

  ------------
  -- Resize --
  ------------
   function Resize(Img : in Unbounded_Image;
                   New_Width, New_Height : Positive)
                  return Unbounded_Image
   is
      Result : Unbounded_Image;
      Px : Pixel_Value_Type;
   begin
      Init(Result, New_Width, New_Height);

      for I in 1..New_Width loop
         for J in 1..New_Height loop
            Px := Get_Pixel(Img,
                            Natural(Float(I - 1)/Float(New_Width - 1) * Float(Columns(Img) - 1)) + 1,
                            Natural(Float(J - 1)/Float(New_Height - 1) * Float(Rows(Img) - 1)) + 1);
            Set_Pixel(Result, I,J, px);
         end loop;
      end loop;

      return Result;
   end Resize;

end Images.Image;
