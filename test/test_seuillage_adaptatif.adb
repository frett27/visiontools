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



-- For timing issues
with Calendar;use Calendar;

-- the tested library
with Statistics; use Statistics;

-- pour les image I/O
with Images;use Images;
with Images.Binary;
with Images.Greyscale; use Images.Greyscale;


with Image_Io; use Image_Io;
with Utils;use Utils;

-- utilisation de la vectorisation
with Vectorisation;use Vectorisation;


-- output
with Text_Io;use Text_Io;

with GNAT.Traceback.Symbolic;
with Ada.Exceptions;

procedure Test_Seuillage_Adaptatif is

   Image_Test_Name : constant String := "cart3crop_2.jpg";
   Image_Test_Result : constant String := "test_blanc_seuil.jpg";

   --
   -- affiche l'histogramme de couleurs dans la console
   --
   procedure Affiche_Histogram (Color_Number : Greyscale_Color_Number) is
      Cumul : Natural := 0; -- cumul
      Last_Cumul : Natural := 0;
   begin

      for I in Color_Number'Range loop
         Last_Cumul := Cumul;
         Cumul := Cumul + Color_Number (I);
         -- dessin de l'histogramme ...
         Text_Io.Put_Line("colonne " & Pixel_Value_Type'Image(I)
                          & " - valeur " & Natural'Image(Color_Number(I))
                          & " - cumul " & Natural'Image(cumul)
                          & " - delta " & Natural'Image(Cumul - Last_Cumul));
      end loop;

   end Affiche_Histogram;


   -- recherche le max dans la liste de points
   function FindMax(P : Point_Array_Access)
                   return Natural is
      M : Natural := P'First;
   begin
      for I in P'Range loop
         if P(I).Y > P(M).y then
            M := I;
         end if;
      end loop;
      return M;
   end;

   procedure Log(S:String) is
   begin
      Text_Io.Put_Line(S);
   end;

   procedure Test_Seuil is
      Img : Unbounded_Image;
      Color_Number : Greyscale_Color_Number;
      P1, P2 : Pixel_Value_Type;
      Seuil : Natural;
      T1 : Time;

   begin
      Log("Start ..");
      T1 := Clock;
      Read_Jpeg_Image (Image_Test_name, Img);
      Log("Delay for reading => " & Duration'Image(Clock-T1));

      -- calcul des nombre d'occurences de couleurs
      -- dans l'image ...
      Color_Number := Calc_Color_Number (Img);

      -- color_number contient un tableau contenant la liste des couleurs
      -- et le nombre d'occurence de ces couleures
      Affiche_Histogram (Color_Number);

      -- cette fonction recherche les deux couleures dominantes
      T1 := Clock;
      Calc_First_Colors(Img, P1, P2);
      Log("Delay => " & Duration'Image(Clock-T1));

      Put_Line("Couleur 1 :" & Pixel_Value_Type'Image(P1));
      Put_Line("Couleur 2 :" & Pixel_Value_Type'Image(P2));


      Seuil := Pixel_Value_Type( ( Natural(P1) + Natural(P2))/2);
      Put_Line("Seuil utilise : " & Pixel_Value_Type'image(seuil));

      declare
         Result : Binary.Unbounded_Image := Threshold(Img, Seuil);
      begin
         Put_Line("Ecriture du résultat");
         -- Ecriture du resultat
         Write_Jpeg_Image(image_test_result, To_Greyscale(Result));
      end;

   end Test_Seuil;

begin
   Test_Seuil;

exception
   when E : others =>
      Text_Io.Put_Line (Ada.Exceptions.Exception_Name(E));
      -- on affiche le stack trace
      Text_Io.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end;
