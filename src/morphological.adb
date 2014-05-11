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

-- Filename        : morphological.adb
-- Description     : Package d'opérateurs morphologiques

-- Package d'opérators morphologiques

package body Morphological is

   -- Définition d'un pixel, en autorisant l'attribution d'un pixel en dehors
   --de
   -- l'image, le pixel n'est alors pas définit
   procedure Set_Pixel_Unbounded
     (M     : in out Unbounded_Image;
      X, Y  : in Integer;
      Value : in Pixel_Value_Type)
   is
   begin
      if X >= 1 and X <= Integer (Columns (M)) then
         if Y >= 1 and Y <= Integer (Rows (M)) then
            Set_Pixel (M, Column_Index_Type (X), Row_Index_Type (Y), Value);
         end if;
      end if;
   end Set_Pixel_Unbounded;

   -- Récupération d'un pixel en attribuant une valeur lorsque le pixel
   -- est en dehors de l'image
   function Get_Pixel_unbounded
     (Img     : in Unbounded_Image;
      X, Y    : in Integer;
      Default : in Pixel_Value_Type)
      return    Pixel_Value_Type
   is
      RetVal : Pixel_Value_Type := Default;
   begin
      if X >= 1 and X <= Integer (Columns (Img)) then
         if Y >= 1 and Y <= Integer (Rows (Img)) then
            RetVal :=
               Get_Pixel (Img, Column_Index_Type (X), Row_Index_Type (Y));
         end if;
      end if;
      return RetVal;
   end Get_Pixel_unbounded;

   function Max (X, Y : in Pixel_Value_Type) return Pixel_Value_Type is
   begin
      if X > Y then
         return X;
      else
         return Y;
      end if;
   end Max;

   function Min (X, Y : in Pixel_Value_Type) return Pixel_Value_Type is
   begin
      if X < Y then
         return X;
      else
         return Y;
      end if;
   end Min;

   function GetMax
     (M       : in Unbounded_Image;
      Pattern : in Unbounded_Image;
      X       : in Column_Index_Type;
      Y       : in Row_Index_Type)
      return    Pixel_Value_Type
   is
      PatternMaxpx : Pixel_Value_Type := Pixel_Value_Type'Last;
      Retval       : Pixel_Value_Type := 0;
   begin
      -- Initialisation De La Premiere Variable
      for I in  1 .. Columns (Pattern) loop
         for J in  1 .. Rows (Pattern) loop
            -- teste si on est dans la matrice
            if Get_Pixel (Pattern, I, J) = PatternMaxpx then
               Retval :=
                  Max
                    (Retval,
                     Get_Pixel_unbounded
                        (M,
                         Integer (X + I - 1),
                         Integer (Y + J - 1),
                         0));
            end if;
         end loop;
      end loop;
      return Retval;
   end GetMax;

   function Dilate
     (Imgin   : in Unbounded_Image;
      Pattern : in Unbounded_Image)
      return    Unbounded_Image
   is
      RetImg        : Unbounded_Image;
      Pixvalue, alt : Pixel_Value_Type;

   begin
      RetImg := Imgin; -- Par copie
      for j in  1 .. Rows (Imgin) loop
         for i in  1 .. Columns (Imgin) loop

            -- Récupération de l'altitude maximale
            alt := GetMax (Imgin, Pattern, i, j);

            for K in  1 .. Rows (Pattern) loop
               for L in  1 .. Columns (Pattern) loop
                  Pixvalue :=
                     Get_Pixel_unbounded
                       (RetImg,
                        Integer (i + L - 1),
                        Integer (j + K - 1),
                        0);
                  if Get_Pixel (Pattern, L, K) = Pixel_Value_Type'Last then
                     Set_Pixel_Unbounded
                       (RetImg,
                        Integer (i + L - 1),
                        Integer (j + K - 1),
                        Max (Pixvalue, alt));
                  end if;
               end loop;
            end loop;

         end loop;
      end loop;
      return RetImg;
   end Dilate;

   function Inverse (Imgin : in Unbounded_Image) return Unbounded_Image is
      Retval : Unbounded_Image := Imgin;
   begin
      for I in  1 .. Columns (Imgin) loop
         for J in  1 .. Rows (Imgin) loop
            Set_Pixel
              (Retval,
               I,
               J,
               Pixel_Value_Type'Last - Get_Pixel (Imgin, I, J));
         end loop;
      end loop;
      return Retval;
   end Inverse;

   function "-" (Left, Right : in Unbounded_Image) return Unbounded_Image is
      Retval : Unbounded_Image := Left;
   begin
      if Columns (Left) /= Columns (Right) or
         Rows (Left) /= Rows (Right)
      then
         raise Invalid_Size;
      end if;
      for I in  1 .. Columns (Left) loop
         for J in  1 .. Rows (Left) loop
            if Integer (Get_Pixel (Left, I, J)) -
               Integer (Get_Pixel (Right, I, J)) >=
               0
            then
               Set_Pixel
                 (Retval,
                  I,
                  J,
                  Max (0, Get_Pixel (Left, I, J) - Get_Pixel (Right, I, J)));
            else
               Set_Pixel (Retval, I, J, 0);
            end if;

         end loop;
      end loop;
      return Retval;
   end "-";

   function "+" (Left, Right : in Unbounded_Image) return Unbounded_Image is
      Retval : Unbounded_Image;
   begin
      if Columns (Left) /= Columns (Right) or
         Rows (Left) /= Rows (Right)
      then
         raise Invalid_Size;
      end if;
      Set_Size (Retval, Columns (Left), Rows (Left));
      for I in  1 .. Columns (Left) loop
         for J in  1 .. Rows (Left) loop
            if (Integer (Get_Pixel (Left, I, J)) +
                Integer (Get_Pixel (Right, I, J))) >
               Pixel_Value_Type'Last
            then
               Set_Pixel (Retval, I, J, Pixel_Value_Type'Last);
            else
               Set_Pixel
                 (Retval,
                  I,
                  J,
                  Max (0, Get_Pixel (Left, I, J) + Get_Pixel (Right, I, J)));
            end if;
         end loop;
      end loop;
      return Retval;
   end "+";

   -- L'erosion est une dilatation en utilisant une inversion
   function Erode
     (Imgin   : in Unbounded_Image;
      Pattern : in Unbounded_Image)
      return    Unbounded_Image
   is
      Retval : Unbounded_Image;
   begin
      Retval := Inverse (Dilate (Inverse (Imgin), Pattern));
      return Retval;
   end Erode;

   function TopHat
     (Imgin   : in Unbounded_Image;
      Pattern : in Unbounded_Image)
      return    Unbounded_Image
   is
      Retval : Unbounded_Image;
   begin
      Retval := Imgin - Dilate (Erode (Imgin, Pattern), Pattern);
      return Retval;
   end TopHat;

   function Closing
     (Imgin   : in Unbounded_Image;
      Pattern : in Unbounded_Image)
      return    Unbounded_Image
   is
      Result : Unbounded_Image;
   begin
      return Erode (Dilate (Imgin, Pattern), Pattern);
   end Closing;

   function Opening
     (Imgin   : in Unbounded_Image;
      Pattern : in Unbounded_Image)
      return    Unbounded_Image
   is
      Result : Unbounded_Image;
   begin
      return Dilate (Erode (Imgin, Pattern), Pattern);
   end Opening;

   --
   --
   --   Optimized Version for square patterns ..
   --
   --

   -- Fonction utilisée pour les opérateur morphologiques accélérés
   -- Les Fonctions accélérées fonctionnent avec des patterns rectangulaires
   function GetMax
     (M            : in Unbounded_Image;
      PatternSizeX : in Column_Index_Type;
      PatternSizeY : in Row_Index_Type;
      X            : Column_Index_Type;
      Y            : Row_Index_Type)
      return         Pixel_Value_Type
   is
      Retval : Pixel_Value_Type := 0;

   begin
      for I in  1 .. PatternSizeX loop
         for J in  1 .. PatternSizeY loop
            Retval :=
               Max
                 (Retval,
                  Get_Pixel_unbounded
                     (M,
                      Integer (X + I - 1),
                      Integer (Y + J - 1),
                      0));
         end loop;
      end loop;
      return Retval;
   end GetMax;

   -- faster execution
   function Dilate
     (Imgin        : in Unbounded_Image;
      SizeX, SizeY : in Integer)
      return         Unbounded_Image
   is
      RetImg        : Unbounded_Image;
      Pixvalue, alt : Pixel_Value_Type;
   begin
      RetImg := Imgin; -- Par copie
      for j in  1 .. Rows (Imgin) loop
         for i in  1 .. Columns (Imgin) loop

            -- Récupération de l'altitude maximale
            alt :=
               GetMax
                 (Imgin,
                  Column_Index_Type (SizeX),
                  Row_Index_Type (SizeY),
                  i,
                  j);
            for K in  1 .. Row_Index_Type (SizeY) loop
               for L in  1 .. Column_Index_Type (SizeX) loop
                  Pixvalue :=
                     Get_Pixel_unbounded
                       (RetImg,
                        Integer (i + L - 1),
                        Integer (j + K - 1),
                        0);
                  Set_Pixel_Unbounded
                    (RetImg,
                     Integer (i + L - 1),
                     Integer (j + K - 1),
                     Max (Pixvalue, alt));
               end loop;
            end loop;

         end loop;
      end loop;
      return RetImg;
   end Dilate;

   function Dilate
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      RetImg : Unbounded_Image;
   begin
      RetImg := Dilate (Imgin, Size, 1);
      RetImg := Dilate (RetImg, 1, Size);
      return RetImg;
   end Dilate;

   -- L'erosion est une dilatation en utilisant une inversion
   function Erode
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Retval : Unbounded_Image;
   begin
      Retval := Inverse (Dilate (Inverse (Imgin), Size));
      return Retval;
   end Erode;

   function TopHat
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Retval : Unbounded_Image;
   begin
      Retval := Imgin - Dilate (Erode (Imgin, Size), Size);
      return Retval;
   end TopHat;

   function Closing
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Result : Unbounded_Image;
   begin
      return Erode (Dilate (Imgin, Size), Size);
   end Closing;

   function Opening
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Result : Unbounded_Image;
   begin
      return Dilate (Erode (Imgin, Size), Size);
   end Opening;

   --
   --
   -- Very Fast implementation ...
   --
   --

   function FastDilateH
     (Imgin : in Unbounded_Image;
      Size  : in Positive)
      return  Unbounded_Image
   is
      Result : Unbounded_Image := Imgin;

      CurrentFilePos : Natural range 0 .. 2 * Size - 2;  -- Derniere position
                                                         --..
      CurrentMax     : Pixel_Value_Type                              :=
         Pixel_Value_Type'First;
      CurrentWindow  : array (0 .. 2 * Size - 2) of Pixel_Value_Type :=
        (others => Pixel_Value_Type'First);

   --- cet algorithme est basé sur le principe qu'à la position I,
   --- la valeur du pixel est la max de la fenetre I-Size+1, i+size-1
   --- on maintient donc une fenetre glissante contenant la valeur de max de
   --la fenetre

   begin
      for J in  1 .. Rows (Imgin) loop
         CurrentMax     := Pixel_Value_Type'First;
         CurrentFilePos := 0;
         CurrentWindow  := (others => Pixel_Value_Type'First);
         for I in  2 - Size .. Columns (Imgin) loop
            -- invariant, I est la position du pixel considéré
            declare
               P : Pixel_Value_Type :=
                  Get_Pixel_unbounded
                    (Imgin,
                     I + Size - 1,
                     J,
                     Pixel_Value_Type'First);
            begin
               -- on pointe sur le suivant
               CurrentFilePos := (CurrentFilePos + 1) mod (2 * Size - 1);
               if P >= CurrentMax then
                  -- si le nouveau pixel est plus grand ... on le prends pour
                  --max
                  CurrentWindow (CurrentFilePos) := P;
                  CurrentMax                     := P;
               else
                  -- le pixel lu n'est pas le max ...
                  if CurrentWindow (CurrentFilePos) = CurrentMax then
                     -- Le pixel sortant est le max .. il faut recalculer le
                     --max local
                     CurrentWindow (CurrentFilePos) := P;
                     CurrentMax                     := Pixel_Value_Type'First;
                     for K in  CurrentWindow'Range loop
                        CurrentMax := Max (CurrentWindow (K), CurrentMax);
                     end loop;
                  else
                     -- Le pixel sortant n'est pas le max .. le max reste le
                     --max ..
                     CurrentWindow (CurrentFilePos) := P;
                  end if;
               end if;
               Set_Pixel_Unbounded (Result, I, J, CurrentMax);
            end;
         end loop;
      end loop;
      return Result;
   end FastDilateH;

   -- Implementation, with a fast method
   function FastDilateV
     (Imgin : in Unbounded_Image;
      Size  : in Positive)
      return  Unbounded_Image
   is
      Result : Unbounded_Image := Imgin;

      CurrentFilePos : Natural range 0 .. 2 * Size - 2;  -- Derniere position
                                                         --..
      CurrentMax     : Pixel_Value_Type                              :=
         Pixel_Value_Type'First;
      CurrentWindow  : array (0 .. 2 * Size - 2) of Pixel_Value_Type :=
        (others => Pixel_Value_Type'First);

   --- cet algorithme est basé sur le principe qu'à la position I,
   --- la valeur du pixel est la max de la fenetre I-Size+1, i+size-1
   --- on maintient donc une fenetre glissante contenant la valeur de max de
   --la fenetre

   begin
      for J in  1 .. Columns (Imgin) loop
         CurrentMax     := Pixel_Value_Type'First;
         CurrentFilePos := 0;
         CurrentWindow  := (others => Pixel_Value_Type'First);
         for I in  2 - Size .. Rows (Imgin) loop
            -- invariant, I est la position du pixel considéré dans la ligne
            declare
               P : Pixel_Value_Type :=
                  Get_Pixel_unbounded
                    (Imgin,
                     J,
                     I + Size - 1,
                     Pixel_Value_Type'First);
            begin
               -- on pointe sur le suivant
               CurrentFilePos := (CurrentFilePos + 1) mod (2 * Size - 1);
               if P >= CurrentMax then
                  -- si le nouveau pixel est plus grand ... on le prends pour
                  --max
                  CurrentWindow (CurrentFilePos) := P;
                  CurrentMax                     := P;
               else
                  -- le pixel P n'est pas le max ...
                  if CurrentWindow (CurrentFilePos) = CurrentMax then
                     -- Le pixel sortant est le max .. il faut recalculer le
                     --max local
                     CurrentWindow (CurrentFilePos) := P;
                     CurrentMax                     := Pixel_Value_Type'First;
                     for K in  CurrentWindow'Range loop
                        CurrentMax := Max (CurrentWindow (K), CurrentMax);
                     end loop;
                  else
                     -- Le pixel sortant n'est pas le max .. le max reste le
                     --max ..
                     CurrentWindow (CurrentFilePos) := P;
                  end if;
               end if;

               Set_Pixel_Unbounded (Result, J, I, CurrentMax);
            end;
         end loop;
      end loop;
      return Result;
   end FastDilateV;

   function FastDilate
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      RetImg : Unbounded_Image;
   begin
      RetImg := FastDilateH (Imgin, Size);
      RetImg := FastDilateV (RetImg, Size);
      return RetImg;
   end FastDilate;

   -- L'erosion est une dilatation en utilisant une inversion
   function FastErode
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Retval : Unbounded_Image;
   begin
      Retval := Inverse (FastDilate (Inverse (Imgin), Size));
      return Retval;
   end FastErode;

   function FastTopHat
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Retval : Unbounded_Image;
   begin
      Retval := Imgin - FastDilate (FastErode (Imgin, Size), Size);
      return Retval;
   end FastTopHat;

   function FastClosing
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Result : Unbounded_Image;
   begin
      return FastErode (FastDilate (Imgin, Size), Size);
   end FastClosing;

   function FastOpening
     (Imgin : in Unbounded_Image;
      Size  : in Integer)
      return  Unbounded_Image
   is
      Result : Unbounded_Image;
   begin
      return FastDilate (FastErode (Imgin, Size), Size);
   end FastOpening;

   -- Récupération de la valeure moyenne des pixels sous un pattern
   function GetMedian
     (M       : Unbounded_Image;
      Pattern : Unbounded_Image;
      X, Y    : Integer)
      return    Pixel_Value_Type
   is
      PatternMaxpx : Pixel_Value_Type := Pixel_Value_Type'Last;
      Retval       : Natural          := 0;
      NbPts        : Natural          := 0;

   begin
      -- Initialisation De La Premiere Variable
      for I in  1 .. Columns (Pattern) loop
         for J in  1 .. Rows (Pattern) loop
            -- teste si on est dans la matrice
            if Get_Pixel (Pattern, I, J) = PatternMaxpx then
               NbPts  := NbPts + 1;
               Retval := Retval +
                         Natural (Get_Pixel_unbounded
                                     (M,
                                      X + Integer (I) - 1,
                                      Y + Integer (J) - 1,
                                      0));
            end if;
         end loop;
      end loop;

      return Pixel_Value_Type (Retval / NbPts);
   end GetMedian;

   -- procedure affichage le pattern dans une image
   procedure PutPattern
     (Imgin   : in out Unbounded_Image;
      Pattern : in Unbounded_Image;
      X, Y    : Integer)
   is
   begin
      for K in  1 .. Columns (Pattern) loop
         for L in  1 .. Rows (Pattern) loop
            if Get_Pixel (Pattern, K, L) = Pixel_Value_Type'Last then
               Set_Pixel_Unbounded
                 (Imgin,
                  X + Integer (K) - 1,
                  Y + Integer (L) - 1,
                  0);
            end if;
         end loop;
      end loop;
   end PutPattern;

end Morphological;
