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

with Ada.Numerics.Generic_Elementary_Functions;

package body Vectorisation is

   package Distance_Numerics_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (
      Float_Type => Distance);
   use Distance_Numerics_Functions;

   Max_Result_Segment_Size : constant Natural := 3000;

   type Ligne is record
      O : Point; -- point origine
      D : Point; -- point destination
   end record;

   function GetX (P : Point) return Coord is
      pragma Inline (GetX);
   begin
      return P.X;
   end GetX;

   function GetY (P : Point) return Coord is
      pragma Inline (GetY);
   begin
      return P.Y;
   end GetY;

   -- distance entre deux points
   function Calc_Distance (P1, P2 : Point) return Distance is
      pragma Inline (Calc_Distance);
      Dx : Coord := GetX (P1) - GetX (P2);
      Dy : Coord := GetY (P1) - GetY (P2);
   begin
      return Sqrt (Distance (Dx * Dx) + Distance (Dy * Dy));
   end Calc_Distance;

   --
   -- Cette fonction calcule la distance d'un point à une ligne (et non un
   --segment)
   -- cad  sans tenir compte de la distance avec les extrémités
   --
   function Calc_Distance
     (Point_Origine, Point_Destination, P : Point)
      return                                Distance
   is
      Dx    : Coord    := GetX (Point_Destination) - GetX (Point_Origine);
      Dy    : Coord    := GetY (Point_Destination) - GetY (Point_Origine);
      Ratio : Distance :=
         ((Distance (GetX (P)) - Distance (GetX (Point_Origine))) *
          Distance (Dx) +
          (Distance (GetY (P)) - Distance (GetY (Point_Origine))) *
          Distance (Dy)) /
         (Distance (Dx) * Distance (Dx) + Distance (Dy) * Distance (Dy));
      Px    : Distance :=
         (1.0 - Ratio) * Distance (GetX (Point_Origine)) +
         Ratio * Distance (GetX (Point_Destination));
      Py    : Distance :=
         (1.0 - Ratio) * Distance (GetY (Point_Origine)) +
         Ratio * Distance (GetY (Point_Destination));

      Dpx : Distance := Px - Distance (GetX (P));
      Dpy : Distance := Py - Distance (GetY (P));

   begin
      return Sqrt (Dpx * Dpx + Dpy * Dpy);
   end Calc_Distance;

   -- fonction de recherche de l'index suivant,
   -- respectant la tolerance de recherche
   function Recherche_Index_Suivant
     (P         : Point_Array_Access;
      Index     : Natural;
      Tolerance : Distance)
      return      Natural
   is
      Point_Origine   : Point   := P (Index);
      Index_Recherche : Natural := Index + 1;
   begin
      if not (Index_Recherche in P'Range) then
         return Index; -- pas de point suivant
      end if;

      Index_Recherche := Index_Recherche + 1;

      while Index_Recherche in P'Range loop
         for I in  Index + 1 .. Index_Recherche - 1 loop
            declare
               D : Distance :=
                  Calc_Distance (Point_Origine, P (Index_Recherche), P (I));
            begin
               if D > Tolerance then
                  return Index_Recherche - 1;
               end if;
            end;
         end loop;

         Index_Recherche := Index_Recherche + 1;
      end loop;

      return Index_Recherche - 1;
   end Recherche_Index_Suivant;

   ----------------
   -- Generalise --
   ----------------

   function Generalise
     (P    : Point_Array_Access;
      D    : Distance)
      return Point_Array_Access
   is
      Tableau_Points : Point_Array (1 .. Max_Result_Segment_Size);
      Compteur       : Natural := 0;

      Index : Natural;

      Result : Point_Array_Access;
   begin
      if P = null then
         return null;
      end if;

      if P'Length < 1 then
         return new Point_Array (0 .. 0);
      end if;

      Index := P'First;

      -- on a au moins un point, on l'ajoute au resultat
      Compteur                  := Compteur + 1;
      Tableau_Points (Compteur) := P (Index);

      loop
         Index := Recherche_Index_Suivant (P, Index, D);
         -- on ajoute le point dans le resultat
         Compteur                  := Compteur + 1;
         Tableau_Points (Compteur) := P (Index);
         exit when Index = P'Last;
      end loop;

      -- allocation du résultat
      Result     := new Point_Array (1 .. Compteur);
      Result.all := Tableau_Points (1 .. Compteur);

      return Result;
   end Generalise;

end Vectorisation;
