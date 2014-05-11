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

-- Package d'opérators morphologiques

with Images.Greyscale;use Images.Greyscale;

Package Morphological is

   -- fonctions génériques d'opérateurs morphologiques

   function Dilate(Imgin : in Unbounded_image;
                   Pattern : in Unbounded_image )
                  return Unbounded_image;

   function Erode(Imgin : in Unbounded_image;
                  Pattern : in Unbounded_image)
                 return Unbounded_image;

   function TopHat(Imgin: in Unbounded_image;
                   Pattern : in Unbounded_image )
                  return Unbounded_image;

   function Closing(Imgin: in Unbounded_image;
                    Pattern : in Unbounded_image )
                   return Unbounded_image;

   function Opening(Imgin: in Unbounded_Image;
                    Pattern : in Unbounded_image )
                   return Unbounded_image;

   -- opérateurs d'ajouts ou de combinaison des composantes de l'image

   function "-"(Left,Right:in Unbounded_Image)
               return Unbounded_Image;
   function "+"(Left,Right:in Unbounded_Image)
               return Unbounded_Image;

   -- inverse l'image
   function Inverse(Imgin : in Unbounded_Image)
                   return Unbounded_Image;


   -- implantation spécifique des Operateurs morphologiques
   -- pour des patterns carres
   -- beaucoup plus rapide qu'un pattern non carré

   function Dilate(Imgin : in Unbounded_Image ; Size : in Integer )
                        return Unbounded_Image;

   function Erode(Imgin : in Unbounded_image ;Size : in Integer )
                       return Unbounded_Image;
   function TopHat(Imgin: in Unbounded_image ; Size : in Integer )
                        return Unbounded_Image;

   function Closing(Imgin : in Unbounded_Image; Size : in Integer)
                   return Unbounded_Image;

   function Opening(Imgin : in Unbounded_Image; Size : in Integer)
                   return Unbounded_Image;

   -- implantation rapide des opérateurs morphologiques pour des patterns carré
   -- (utilise la programmation dynamique pour la recherche de maximaux)

   function FastDilate(Imgin : in Unbounded_Image; Size : in Integer )
                        return Unbounded_Image;

   function FastErode(Imgin : in Unbounded_image; Size : in Integer )
                     return Unbounded_Image;

   function FastTopHat(Imgin: in Unbounded_image; Size : in Integer )
                        return Unbounded_Image;

   function FastClosing(Imgin : in Unbounded_Image; Size : in Integer)
                   return Unbounded_Image;

   function FastOpening(Imgin : in Unbounded_Image; Size : in Integer)
                   return Unbounded_Image;


   -- diverses fonctions intéressantes ..
   -- utilisées dans les différents programmes

   procedure PutPattern(Imgin : in out unbounded_image;
                        Pattern : in unbounded_image;
                        X,Y:Integer );

   function GetMedian(M : unbounded_image;
                      Pattern : unbounded_image;
                      X, Y : integer )
                     return pixel_value_type;


   Invalid_Size: exception ;

end Morphological;
