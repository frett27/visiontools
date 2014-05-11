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

with Text_Io;

package body Contours is

   -----------
   -- Sobel --
   -----------

   --- Cette fonction retourne une image contenant les contours
   --- par utilisation d'un Sobel Horizontal
   function Horizontal_Sobel (Img : Unbounded_Image)
                             return Unbounded_Image is
      Result : Unbounded_Image;
      N : Integer ;
      A1, A2, A3, A4, A5, A6, A7, A8, A9 : Pixel_Value_Type;

   begin
      Set_Size(Result, Columns(Img), Rows(Img));
      for I in 1..Columns(Img) loop
         for J in 1..Rows(Img) loop
            n := 0;

            --
            --  Matrice de transformation
            --   de la fonction sobel
            --
            --        -1 -2 -1
            --         0  0  0
            --         1  2  1
            --
            --

            --  on lit les valeurs autours du pixel de lecture
            --       a1 a2 a3
            --       a4 a5 a6
            --       a7 a8 a9
            --

            A5 := Get_Pixel_Unbounded(Img, I, J , 0 );

            A4 := Get_Pixel_Unbounded(Img, I-1, J, A5);
            A6 := Get_Pixel_Unbounded(Img, I+1, J, A5);
            A2 := Get_Pixel_Unbounded(Img, I, J-1, A5);
            A8 := Get_Pixel_Unbounded(Img, I, J+1, A5);

            A1 := Get_Pixel_Unbounded(Img, I-1, J-1, A4);
            A3 := Get_Pixel_Unbounded(Img, I+1, J-1, A6);

            A7 := Get_Pixel_Unbounded(Img, I-1, J+1, A4);
            A9 := Get_Pixel_Unbounded(Img, I+1, J+1, A6);

            n := n - 1 * A1 ;
            n := n - 1 * A3;
            n := n - 2 * A2;

            n := n + 1 * A7;
            n := n + 1 * A9;
            n := n + 2 * A8;

            N := N + (Pixel_Value_Type'Last - Pixel_Value_Type'First) /2;

            -- on écrete les valeurs dans le cas où la dérivée sort
            -- de la plage de valeurs pixel_value_type'range

            if N > Pixel_Value_Type'Last then
               N := Pixel_Value_Type'Last;
            end if;
            if N < Pixel_Value_Type'first then
               N := Pixel_Value_Type'first;
            end if;

            Set_Pixel(Result, I, J, Pixel_Value_Type(N));
         end loop;
      end loop;

      return Result;
   end Horizontal_Sobel;


   function Vertical_Sobel(Img: Unbounded_Image) return Unbounded_Image is
      Result : Unbounded_Image;
      N : Integer ;
      A1, A2, A3, A4, A5, A6, A7, A8, A9 : Pixel_Value_Type;

   begin
      Set_Size(Result, Columns(Img), Rows(Img));
      for I in 1..Columns(Img) loop
         for J in 1..Rows(Img) loop
            n := 0;

            --
            --  Matrice de transformation
            --   de la fonction sobel
            --
            --        -1  0  1
            --        -2  0  2
            --        -1  0  1
            --
            --

            --  on lit les valeurs autours du pixel de lecture
            --       a1 a2 a3
            --       a4 a5 a6
            --       a7 a8 a9
            --

            A5 := Get_Pixel_Unbounded(Img, I, J , 0 );

            A4 := Get_Pixel_Unbounded(Img, I-1, J, A5);
            A6 := Get_Pixel_Unbounded(Img, I+1, J, A5);
            A2 := Get_Pixel_Unbounded(Img, I, J-1, A5);
            A8 := Get_Pixel_Unbounded(Img, I, J+1, A5);

            A1 := Get_Pixel_Unbounded(Img, I-1, J-1, A4);
            A3 := Get_Pixel_Unbounded(Img, I+1, J-1, A6);

            A7 := Get_Pixel_Unbounded(Img, I-1, J+1, A4);
            A9 := Get_Pixel_Unbounded(Img, I+1, J+1, A6);

            n := n - 1 * A1;
            n := n - 1 * A7;
            n := n - 2 * A4;

            n := n + 1 * A3;
            n := n + 1 * A9;
            n := n + 2 * A6;

            -- on centre le résultat autour du milieu

            N := N + (Pixel_Value_Type'Last - Pixel_Value_Type'First) /2;

            -- on écrete les valeurs dans le cas où la dérivée sort
            -- de la plage de valeurs pixel_value_type'range

            if N > Pixel_Value_Type'Last then
               N := Pixel_Value_Type'Last;
            end if;
            if N < Pixel_Value_Type'first then
               N := Pixel_Value_Type'first;
            end if;

            Set_Pixel(Result, I, J, Pixel_Value_Type(N));
         end loop;
      end loop;

      return Result;
   end Vertical_Sobel;



end Contours;

