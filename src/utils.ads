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



with Images.Greyscale;
use Images.Greyscale;

with Images.Color;

with Images.Binary;

use Images;


Package Utils is


   -----------------
   -- MaxContrast --
   -----------------
   --- Cette fonction rééchelonne l'intensité
   --- de l'image sur toute la plage de valeur
   ---
   --- Calcule les min / max sur l'image en niveau de gris
   --- puis réattribue la valeur du pixel en fonction des min/max
   ---

   procedure MaxContrast(Img: in out Unbounded_Image) ;


   -----------------
   -- MaxContrast --
   -----------------
   --- Cette fonction applique un facteur
   --- multiplicatif à l'intensité de l'image
   ---
   ---   Lorsque la valeur dépasse la valeur du pixel,
   ---   la valeur max du pixel est prise

   function "*"(Left: in Unbounded_Image;
                Right : in Natural) return Unbounded_Image;



   ---------------
   -- Threshold --
   ---------------

   --- Seuillage d'une image en fonction d'un niveau donné
   --- tout ce qui est en dessous du seuil, est converti en noir
   --- tout ce qui est en dessus du seuil est converti en blanc

   function Threshold(Img : Unbounded_Image ;
                      Seuil : Pixel_Value_Type) return Binary.Unbounded_Image;


   ------------------
   -- To_GreyScale --
   ------------------

   --- Converti une image binaire en image en niveau
   --- de gris, (valeur extremes de l'image en niveau de gris)
   function To_GreyScale(Img : Binary.Unbounded_Image)
                        return Greyscale.Unbounded_Image;


   ---------------
   -- Rotate_90 --
   ---------------
   type Direction is (Left, Right);

   function Rotate_90(Img : Unbounded_Image;
                      Dir : Direction)
     return Unbounded_Image;


   ---------------------
   -- Resize_Bilinear --
   ---------------------
   -- Rééchantillonnage en bilineaire

   function Resize_Bilinear( U : Color.Unbounded_Image ;
                            New_Width : Column_Index_Type;
                            New_Height : Row_Index_Type)
                          return Color.Unbounded_image;


   ---------------------
   -- Resize_Bicubic  --
   ---------------------
   -- Rééchantillonnage en bicubique

   function Resize_Bicubic( U : Color.Unbounded_Image ;
                            New_Width : Column_Index_Type;
                            New_Height : Row_Index_Type)
                          return Color.Unbounded_image;



end utils;
