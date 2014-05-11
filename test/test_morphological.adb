------------------------------------------------------------------------------
--                             Vision Library                               --
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


--
--- Fonctions de test du package Morphologiques et Image
--

with Images.Greyscale; use Images.Greyscale;
with Image_Io; -- I/O for images ...
with morphological;
with Utils; use Utils;

with Text_Io; -- for logging operations ...

with Calendar;use Calendar;

with Contours;

procedure Test_morphological is

   -- affichage d'une date ...
   function Image(T : Time) return String is
   begin
      return Day_Duration'Image(Seconds(T));
   end;

   -- logguer les informations
   procedure Log(S : String) is
   begin
      Text_Io.Put_Line(S);

   end;


  Test_Image : Unbounded_Image;
  Dilated : Unbounded_Image;

  First, Second : Unbounded_Image;


  T1,T2 : Time;

  P : Pixel_Value_Type;
  N : Natural := 0;

begin

   Log("Reading file");
   -- read a file for Comparisons
   T1 := Clock;
   Log ("  -> Start at :" & Image(T1));

   Image_Io.Read_Jpeg_Image("divers_1289.jpg", Test_Image);

   T2 := Clock;
   Log ("  -> End at :" & Image(T2));
   Log ("  -> Delay :" & Duration'Image(T2-T1));

   -- Perform a sobel ...

   Log("perform a sobel");
   T1 := Clock;
   Log ("  -> Start at :" & Image(T1));

   First := Contours.Horizontal_Sobel(Test_Image);

   T2 := Clock;
   Log ("  -> End at :" & Image(T2));
   Log ("  -> Delay :" & Duration'Image(T2-T1));

   Log ("ecriture du resultat");
   Image_Io.Write_Jpeg_Image("sobel.jpg",First);

   Log("Filtering image");
   T1 := Clock;
   Log ("  -> Start at :" & Image(T1));
   T2 := Clock;
   Log ("  -> End at :" & Image(T2));
   Log ("  -> Delay :" & Duration'Image(T2-T1));

   First := Morphological.FastDilate(Morphological.FastErode(First, 4), 4);
   First := Morphological.FastDilate(Morphological.FastErode(First, 4), 4);
   First := Morphological.FastDilate(Morphological.FastErode(First, 4), 4);
   First := Morphological.FastDilate(Morphological.FastErode(First, 4), 4);

   Image_Io.Write_Jpeg_Image("sobel_multiple_erode_dilate.jpg",First);



   -- read all pixels
   Log("Read all pixels");

   T1 := Clock;
   Log ("  -> Start at :" & Image(T1));

   for I in 1..Columns(Test_Image) loop
      for J in 1..Rows(Test_Image) loop
         P := Get_Pixel(Test_Image, I , J);
         N:=N+Natural(P);
      end loop;
   end loop;


   T2 := Clock;
   Log ("  -> End at :" & Image(T2));
   Log ("  -> Delay :" & Duration'Image(T2-T1));


   -- perform a dilate operation ..
       Log("Perform a dilate operation with a 10 square pattern");
       T1 := Clock;
       Log ("  -> Start at :" & Image(T1));
       Dilated := Morphological.Dilate(Test_Image,10);

       T2 := Clock;
       Log ("  -> End at :" & Image(T2));
       Log ("  -> Delay :" & Duration'Image(T2-T1));
       First := Dilated;

   -- perform a dilate operation ..
       Log("Perform a fast dilate operation with a 10 square pattern");
       T1 := Clock;
       Log ("  -> Start at :" & Image(T1));
       Dilated := Morphological.FastDilate(Test_Image,10);
       T2 := Clock;
       Log ("  -> End at :" & Image(T2));
       Log ("  -> Delay :" & Duration'Image(T2-T1));

       Log("saving work");
       Image_Io.Write_Image("save.ppm", Dilated);

       -- perform difference between the two methods
       Image_Io.Write_Image("Difference.ppm", Morphological."-"(Dilated,First));

end test_morphological;
