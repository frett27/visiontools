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

-- ce packetage effectue des statistiques sur la répartition
-- des niveaux de gris dans l'image ..

with Images.Greyscale;
use Images.Greyscale;

package Statistics is

   --
   --  tableau contenant le nombre d'occurence de la couleur
   --  dans une image
   --
   type Greyscale_Color_Number is array (Pixel_Value_Type) of Natural;

   --
   --  Cette fonction calcule le nombre d'occurence des niveaux de
   --  gris dans l'image
   --
   --   Complexité en o(n*m)
   --
   function Calc_Color_Number (Img : in Unbounded_Image)
                             return Greyscale_Color_Number;


   --
   -- Cette fonction retourne les deux couleurs dominantes
   -- d'une image
   --

   procedure Calc_First_Colors (Img        : in     Unbounded_Image;
                                Col1, Col2 :    out Pixel_Value_Type;
                                Tolerance  : in Natural := 30);


end Statistics;
