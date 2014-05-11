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
with Ada.Real_Time; use Ada.Real_Time;

-- the tested library
with Vectorisation; use Vectorisation;

-- output
with Text_Io;use Text_Io;

procedure Testvectorisation is


   procedure Test_Distance is
      P1 : Point := Point'(X => 0, Y => 0);
      P2 : Point := Point'(X => 2, Y => 2);
      P3 : Point := Point'(X => 4, Y => 4);
      P4 : Point := Point'(X => 2, Y => 0);

   begin
      Put("Distance entre deux points ");
      Put_Line( Distance'Image( Calc_Distance(P1, P2)));

      Put("Distance d'un point confondu a une droite ");
      Put_Line( Distance'Image( Calc_Distance(P1, P2, P3)));

      Put("Distance d'un point a une droite ");
      Put_Line( Distance'Image( Calc_Distance(P1, P2, P4)));

   end;

   --
   -- Affiche les caractéristiques du point
   --
   procedure Affiche_Point ( P : Point ) is
   begin
      Put_Line(" X : " & Coord'Image(P.X)
               & ", Y : " & Coord'Image(P.Y));

   end;


   procedure Affiche_Result( P : Point_Array_Access ) is
   begin
     -- affichage du résultat
      for I in p'Range loop
         Put_Line(" Point No " & Integer'Image(I));
         Affiche_Point(P(I));
      end loop;
   end;


   procedure Test_Recherche_Index_Et_Generalise is

      P1 : Point := Point'(X=>0, Y=>0);
      P2 : Point := Point'(X=>1, Y=>5);
      P3 : Point := Point'(X=>2, Y=>0);
      P4 : Point := Point'(X=>3, Y=>0);
      P5 : Point := Point'(X=>4, Y=>0);

      PA : Point_Array_Access :=
        new Point_Array'( 1=> P1,
                          2 => P2 ,
                          3 => P3,
                          4 => P4,
                          5 => P5);

      Result : Point_Array_Access;

      T : Time;
      D : Duration;

   begin
      Put("Recherche dans le tableau avec une tolerance de 2 ");
      Put_Line( Natural'Image( Recherche_Index_Suivant( PA, 1, 2.0)));
      Put("Recherche dans le tableau avec une tolerance de 0.5 ");
      Put_Line( Natural'Image( Recherche_Index_Suivant( PA, 1, 0.5)));
      Put("Recherche dans le tableau avec une tolerance de 0.5 à partir de 2");
      Put_Line( Natural'Image( Recherche_Index_Suivant( PA, 2, 0.5)));
      Put("Recherche dans le tableau avec une tolerance de 0.5 à partir de 5");
      Put_Line( Natural'Image( Recherche_Index_Suivant( PA, 5, 0.5)));

      Put_Line("-------------------- test généralise avec tolerance de 0.5  -------------------");
      Result := Generalise (Pa, 0.5);
      Affiche_Result(Result);
      Put_Line("-------------------- test généralise avec tolerance de 2 -------------------");
      Result := Generalise (Pa, 2.0);
      Affiche_Result(Result);
      Put_Line("-------------------- test généralise avec tolerance de 5 -------------------");
      Result := Generalise (Pa, 5.0);
      Affiche_Result(Result);

      Put_Line(" test avec beaucoup de points alignés (cas défavorable)");

      -- initialisation du tableau
      Pa := new Point_Array(1 .. 1600);
      for I in Pa'Range loop
         Pa(I) := Point'(X => I, Y=>0);
      end loop;

      -- début du timing

      T := Clock;
      Result := Generalise (Pa, 1.0);
      D := To_Duration (Clock - T);
      -- fin
      Affiche_Result(Result);
      Put_Line("  temps de l'operation : " & Duration'Image(D));

   end;


begin
      Test_Distance;
      Test_Recherche_Index_Et_Generalise;
end;
