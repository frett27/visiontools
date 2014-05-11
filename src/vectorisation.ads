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

-- Package de généralisation de points
-- Author : Freydiere Patrice

with Ada.Unchecked_Deallocation;

--
--
-- routine de vectorisation d'un tableau de point
--
--

package Vectorisation is

   ---
   --- Définition des types utilisés pour la généralisation
   ---

   subtype Coord is Integer;

   type Point is record
      X : Coord;
      Y : Coord;
   end record;

   type Point_Array is array (Natural range <>) of Point;

   type Point_Array_Access is access Point_Array;

   subtype Distance is Float;

   --
   -- cette fonction calcule la distance entre deux points
   --
   function Calc_Distance (P1, P2 : Point) return Distance;

   --
   -- cette fonction calcule la distance d'un point à une droite
   --
   function Calc_Distance
     (Point_Origine, Point_Destination, P : Point)
      return                                Distance;

   --
   -- recherche le premier index qui satisfait encore
   -- le principe du tube
   --
   function Recherche_Index_Suivant
     (P         : Point_Array_Access;
      Index     : Natural;
      Tolerance : Distance)
      return      Natural;

   -- Fonction de généralisation du tableau de points
   -- par la méthode du corridor, en indiquant la distance
   -- max des points au corridor

   function Generalise
     (P    : Point_Array_Access;
      D    : Distance)
      return Point_Array_Access;

   -- libération d'un contour vectoriel
   procedure Free is new Ada.Unchecked_Deallocation (
      Point_Array,
      Point_Array_Access);

private

   Too_Much_Points_In_Result : exception;

end Vectorisation;
