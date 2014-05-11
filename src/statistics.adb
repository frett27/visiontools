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

with Vectorisation;
use Vectorisation;

package body Statistics is

   -----------------------
   -- Calc_Color_Number --
   -----------------------

   function Calc_Color_Number
     (Img : in Unbounded_Image)
      return Greyscale_Color_Number
   is
      A : Greyscale_Color_Number := (others => 0);
      P : Pixel_Value_Type;
   begin
      for I in 1..Columns(Img) loop
         for J in 1..Rows(Img) loop
            P := Get_Pixel(Img, I, J);
            A(P) := Natural'Succ(A(P));
         end loop;
      end loop;
      return A;
   end Calc_Color_Number;



   procedure Calc_First_Colors (Img        : in     Unbounded_Image;
                                Col1, Col2 :    out Pixel_Value_Type;
                                Tolerance  : in Natural := 30 )
   is

      Color_Number : Greyscale_Color_Number := Calc_Color_Number(Img);

      function Find_Max(P : Point_Array_Access)
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

   begin -- Creation du tableau de points
      declare
         P : Point_Array_Access :=
           new Point_Array(Color_Number'Range);
         Res : Point_Array_Access;
         Index : Natural;
      begin
         for I in P.all'Range loop
            P(I).X := I;
            P(I).Y := Color_Number(I);
         end loop;

         -- vectorisation de l'histogramme, pour récupérer les dominantes
         Res := Generalise(P, Float(Tolerance));

         -- Affichage du resultat
         Index := Find_Max(Res);
         Col1 := Pixel_Value_Type(Res(Index).X);
         Res(Index).Y := 0;

         Index := Find_Max(Res);
         Col2 := Pixel_Value_Type(Res(Index).X);

         -- libération du tableau de points

         Free(P);
         Free(Res);
      end;
   end Calc_First_Colors;

end Statistics;

