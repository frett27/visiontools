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

-- Procedure de test des fonctions d'image
with Images.Greyscale; use Images.greyscale;
with Images.Color;
use Images;
-- For timing issues
with Calendar;use Calendar;
with Text_Io;use Text_Io;
with Image_Io;
with Simple_Jpeg_Lib;
use Simple_Jpeg_Lib;

with Ada.Streams;
use Ada.Streams;

with Utils;

procedure Test_Image is

   T1: Time;
   Fake:Character;
begin
   declare
      Img : Unbounded_Image;
      Img2 : Unbounded_Image;
   begin
      Put_Line("Initialisation de l'image");
      Init(Img, 10000, 1000);

      Put_Line("Remplissage de l'image ...");
      for I in 1..Columns(Img) loop
         for J in 1..Rows(Img) loop
            Set_Pixel(Img, I, J, 0);
         end loop;
      end loop;

      Put_Line("Récupération de l'image dans une autre variable ...");
      Img2 := Img;

      Set_Pixel(Img2, 1, 1, 100);

      Put_Line ("pixel 1 de l'image 1 : "
                & Natural'Image(Get_Pixel(Img, 1, 1)));
      Put_Line ("pixel 1 de l'image 2 : "
                & Natural'Image(Get_Pixel(Img2, 1, 1)));

      Put_Line("redéfinition de la taille de l'image");
      Set_Size(Img, 20000, 2000);

      Put_Line("Redéfinition du premier pixel");
      Set_Pixel(Img, 1, 1, 100);

      Put_Line("appuyez sur une touche pour libérer les structures de données");
      Get(Fake);

      Put_Line("redimensionnement de l'image");

      declare
         Imgcarton : Color.Unbounded_Image;
	 ImgTest : Color.Unbounded_Image;
         Newimage : Color.Unbounded_Image;
      begin
         Image_Io.Read_Jpeg_Image("divers_1289.jpg", Imgcarton);
         Image_Io.Read_Jpeg_Image("test.jpg", ImgTest);



         Newimage := Color.Resize(Imgcarton, 640, 480);
         Image_Io.Write_Jpeg_Image("divers_1289_resize.jpg", Newimage);

--         Newimage := Utils.Resize_Bicubic(Imgcarton, 640, 480);
--         Image_Io.Write_Jpeg_Image("divers_1289_resize_bicubic.jpg", Newimage);

         Newimage := Utils.resize_bilinear(imgtest, 640, 480);
         Image_Io.Write_Jpeg_Image("test_resize_bilinear.jpg", Newimage);


         Newimage := Utils.resize_bicubic(Imgcarton, 640, 480);
         Image_Io.Write_Jpeg_Image("test_resize_bicubic.jpg", Newimage);


         Put_Line("done");
      end;


   end;

   begin
      Put_Line("test mémoire des images ... regardez la mémoire et appuyez sur une touche");
      Get(Fake);

      for I in 1..2 loop
         declare
            Img : Color.Unbounded_Image;
            Img2 : Color.Unbounded_Image;
         begin
            Image_Io.Read_Jpeg_Image("CART3CROP_2.jpg",Img);
            Color.Set_Size(Img2, 1000, 1000);
            Img2 := Img;
         end;
      end loop;
      Put_Line("fin du test, regardez la mémoire");
      Get(Fake);

   end;

   Put_Line("test de la liberation de mémoire avec la librairie simple_jpeg_lib");
   for I in 1..100 loop
   declare
      U : Color.Unbounded_Image;
      Jpeg : Jpeg_Handle_Memory_Compress;
   begin
      Color.Set_Size(u,1000,1000);

      Create_Jpeg_Memory(Images.Color.Columns(U) * Images.Color.Rows(U) * 4,
                         Jpeg,
                         Images.Color.Columns(U),
                         Images.Color.Rows(U),
                         Rvb);
      for I in 1..Images.Color.Rows(U) loop
         declare
            CA : Component_Array(1..3 * Images.Color.Columns(U));
         begin
            for J in 1..Images.Color.Columns(U) loop
               declare
                  Px : Images.Color_Pixel_type :=
                    Images.Color.Get_Pixel(U, J, I);
               begin
                  Ca( (J-1)*3 + 1) := Byte(Px.R);
                  Ca( (J-1)*3 + 2) := Byte(Px.V);
                  Ca( (J-1)*3 + 3) := Byte(Px.B);
               end;
            end loop;
            Write_Line(Jpeg, Ca);
         end;
      end loop;

      declare
         Mb : Jpeg_Memory_Buffer :=
           Close_jpeg(Jpeg);
         Buffer : Byte_Array(1..Get_Size(mb)) :=
           Get_Buffer(mb);
         Ea : Stream_Element_Array(1..Stream_Element_Offset(Get_Size(Mb)));
      begin
         for I in Buffer'Range loop
            Ea(Stream_Element_Offset(I)) := Stream_Element(Buffer(I));
         end loop;
         Free_Buffer(Mb);
      end;

   end;
   end loop;



   Put_Line("appuyez sur une touche pour terminer le programme");
   Get(Fake);




end Test_Image;
