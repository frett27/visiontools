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
with Ada.Text_Io; use Ada.Text_Io;

package body Classification is

   --------------------------------------------------------------------------
   --
   --  Fonctions permettant d'utiliser un grid comme un tableau de partition
   --
   --------------------------------------------------------------------------

   --  Get_Element , récupère l'élément P du tableau
   function Get_Element
     (Img  : Grid.Unbounded_Image;
      P    : Positive)
      return Natural
   is
      use Grid;
      I, J : Positive;
   begin
      J := ((P - 1) / Columns (Img)) + 1;
      I := ((P - 1) mod Columns (Img)) + 1;
      return Get_Pixel (Img, I, J);
   end Get_Element;

   procedure Set_Element
     (Img     : in out Grid.Unbounded_Image;
      P       : in Positive;
      Element : in Natural)
   is
      use Grid;
      I, J : Positive;
   begin
      J := ((P - 1) / Columns (Img)) + 1;
      I := ((P - 1) mod Columns (Img)) + 1;
      Set_Pixel (Img, I, J, Element);
   end Set_Element;

   -------------------
   -- Renumber_Grid --
   -------------------

   --
   --   Cette fonction renumerote les partitions de l'image ..
   --   de 1 à nb de partition de l'image
   --   Procedure permettant de renumeroter les partitions à l'interieur
   --   de l'image chaque zone est renumerotée à partir de 1,
   --
   --   Paramètres en entrée : G -> la définition des zones
   --             en sortie : Result -> la définition des zones renumérotée
   --             Partition_Number : Nombre de partitions trouvées dans l'image
   --
   procedure ReNumber_Grid
     (G                : in Grid.Unbounded_Image;
      Result           :  out Grid.Unbounded_Image;
      Partition_Number :  out Natural)
   is

      use Grid;

      --   grid contenant les conversions d'images ...
      New_Partition : Grid.Unbounded_Image;

      P_Number : Natural := 0;

      --   Cette fonction récupère la couleur de la partition demandée.
      --   Si la partition n'a pas encore de couleur attribuée,
      --   une nouvelle couleur est attribuée.
      function Get_New_Partition_Number (P : Positive) return Natural is
         New_Partition_Number : Natural;
      begin
         New_Partition_Number := Get_Element (New_Partition, P);
         if New_Partition_Number = 0 then
            --   il n'y a pas encore de nouveau numero
            --   associé à cette partition ..
            P_Number := P_Number + 1;

            --   on l'associe à la partition -> Partition_Number + 1
            Set_Element (New_Partition, P, P_Number);
            return P_Number;
         else
            --   on retourne le nouveau numero associe à la partition ...
            return New_Partition_Number;
         end if;
      end Get_New_Partition_Number;

   begin
      P_Number := 0;
      Set_Size (Result, Columns (G), Rows (G));
      Set_Size (New_Partition, Columns (G), Rows (G));

      for J in  1 .. Rows (G) loop
         for I in  1 .. Columns (G) loop



            Set_Pixel
              (Result,
               I,
               J,
               Get_New_Partition_Number (Get_Pixel (G, I, J)));
         end loop;
      end loop;

      Partition_Number := P_Number;
   end ReNumber_Grid;

   --------------
   -- Classify --
   --------------

   --  Algorithm de classification d'ensembles contigues dans une image.

   --  Logique générale,
   --
   --
   --   1 ere etape, classification des ensembles.
   --
   --  on regarde si la couleur du pixel à gauche est la même que le pixel
   --courant
   --  si oui, on dit que le pixel appartient à la partition du pixel de gauche
   --
   --  on regarde si le pixel du dessus est le même que le pixel courant
   --  si c'est le cas,
   --     -> si le pixel n'appartient pas à la partition de gauche,
   --        le pixel appartient à la patition du haut.
   --     -> si le pixel appartient à la partition de gauche ..
   --        on fusionne les deux partitions
   --
   --  2 eme etape, coloriage des ensembles
   --
   --
   --   Complexite en O(n2)
   --

   procedure Classify
     (Img               : in GreyScale.Unbounded_Image;
      Partitioned_Image : out Grid.Unbounded_Image;
      Partition_Number  : out Natural)
   is
      use Grid;
      use GreyScale;
      Result         : Grid.Unbounded_Image;
      Pere_Partition : Grid.Unbounded_Image; --   on utilise une image pour
      --   stocker le lien partition pere
      Next_Classify_Number : Natural := 1;
      Calculated_Partition : Natural;

      --  P : Numéros de la partition , les numéros des partitions commences à
      --1
      --  Father : nom du pere de la partition
      procedure Set_Partition_Father (P : Natural; Father : Natural) is
      begin
         Set_Element (Pere_Partition, P, Father);
      end Set_Partition_Father;

      --  Cette fonction récupère la partition principale d'une partition
      function Get_Partition (P : Natural) return Natural is
         Partition_Pere   : Natural;
         Racine_Partition : Natural;
      begin
         Partition_Pere := Get_Element (Pere_Partition, P);
         if Partition_Pere = 0 then
            return P;
         else
            Racine_Partition := Get_Partition (Partition_Pere);
            --  réattribution de la partition
            Set_Partition_Father (P, Racine_Partition);
            return Racine_Partition;
         end if;
      end Get_Partition;

      function Get_Partition (I, J : Natural) return Natural is
      begin
         return Get_Partition (Get_Pixel (Result, I, J));
      end Get_Partition;

   begin
      Set_Size (Result, Columns (Img), Rows (Img));
      Set_Size (Pere_Partition, Columns (Img), Rows (Img));

      for J in  1 .. Rows (Img) loop
         for I in  1 .. Columns (Img) loop
            Calculated_Partition := Next_Classify_Number;

            --  Vérification de l'appartenance à la partition de gauche ...
            if (I > 1) then
               if Get_Pixel (Img, I, J) = Get_Pixel (Img, I - 1, J) then
                  Calculated_Partition := Get_Partition (I - 1, J);
               end if;
            end if;

            --   Vérification de l'appartenance à la partition du haut ...
            --   ici, Calculated_partition /= next_classify_number si il y a
            --   une partition à gauche
            --   ie : il y a un pixel à gauche ...

            if (J > 1) then
               if Get_Pixel (Img, I, J) = Get_Pixel (Img, I, J - 1) then

                  if Calculated_Partition = Next_Classify_Number then
                     --   le pixel n'appartient pas à la partition de gauche
                     --...
                     Calculated_Partition := Get_Partition (I, J - 1);
                  else
                     --   le pixel appartient à la partition de gauche ...
                     --   Calculated_partition contient le numero de la
                     --partition à gauche

                     declare
                        Partition_Haut : Natural := Get_Partition (I, J - 1);
                     begin
                        --   on teste si la partition est la meme, si c'est la
                        --meme, on
                        --   ne fusionne pas les partitions.
                        if Partition_Haut /= Calculated_Partition then
                           --   on fusionne les deux partitions
                           --   la partition du haut a pour partition pere la
                           --partition de gauche
                           Set_Partition_Father
                             (Partition_Haut,
                              Calculated_Partition);
                        end if;

                     end;
                  end if;
               end if;
            end if;

            --   détermination de la partition du pixel ..
            Set_Pixel (Result, I, J, Calculated_Partition);

            if Calculated_Partition = Next_Classify_Number then
               Set_Partition_Father (Next_Classify_Number, 0);
               --   la nouvelle partition n'a pas de père
               Next_Classify_Number := Next_Classify_Number + 1;
            end if;

         end loop;
      end loop;

      --   On refait un passage sur l'image, et on la colorie en utilisant
      --   une identification par partition
      for J in  1 .. Rows (Result) loop
         for I in  1 .. Columns (Result) loop
            --  renumerotation des partitions ...
            Set_Pixel (Result, I, J, Get_Partition (I, J));
         end loop;
      end loop;

      --  On réordonne les partitions
      ReNumber_Grid (Result, Partitioned_Image, Partition_Number);

   end Classify;

   --   Cette fonction convertie un Grid Naturel en Image ...
   --   Il est nécessaire qu'il n'y aie pas plus de partition que ce que peut
   --contenir
   --   l'image ...
   function GridToImg
     (G    : Grid.Unbounded_Image)
      return GreyScale.Unbounded_Image
   is
      use Grid;
      use GreyScale;
      Reordered_Grid   : Grid.Unbounded_Image;
      Partition_Number : Natural;
      Result           : GreyScale.Unbounded_Image;
   begin

      ReNumber_Grid (G, Reordered_Grid, Partition_Number);

      --  S'il y a plus de partition que de couleurs associée à l'image ...
      --  on envoie une exception ...
      if Partition_Number > Natural'Last then
         raise Too_Many_Partitions;
      end if;

      Set_Size (Result, Columns (G), Rows (G));
      for J in  1 .. Rows (Result) loop
         for I in  1 .. Columns (Result) loop
            Set_Pixel
              (Result,
               I,
               J,
               GreyScale_Pixel_Type (Get_Pixel (Reordered_Grid, I, J)));
         end loop;
      end loop;
      return Result;
   end GridToImg;

   --   Construit les informations associées aux différentes partitions
   --
   --   G : L'image contenant les différentes partitions
   --   Partition_Number : L'indice de la derniere partition de l'image
   --
   function Construct_Grid_Infos
     (G                : in Grid.Unbounded_Image;
      Partition_Number : Natural)
      return             Partition_Info_Array
   is
      use Grid;
      Current_Partition : Natural;
      Current_Info      : Partition_Info;
      S                 : Partition_Info_Array (1 .. Partition_Number) :=
        (others => Null_Partition_Info);
   begin
      --Put_Line("Construct_Grid_Infos : " & Natural'Image(Partition_Number));

      if partition_number = 0 then
      	return S;
      end if;

      for J in  1 .. Rows (G) loop
         for I in  1 .. Columns (G) loop
            --  Récupération de la partition
            Current_Partition := Get_Pixel (G, I, J);

            --Put_Line("current partition : " & Natural'Image(Current_Partition) & ", Partition_number "& Natural'Image(Partition_Number));
            Current_Info          := S (Current_Partition);
            Current_Info.Minx     := Natural'Min (Current_Info.Minx, I);
            Current_Info.Miny     := Natural'Min (Current_Info.Miny, J);
            Current_Info.Maxx     := Natural'Max (Current_Info.Maxx, I);
            Current_Info.Maxy     := Natural'Max (Current_Info.Maxy, J);
            Current_Info.NbPixels := Current_Info.NbPixels + 1;

            S (Current_Partition) := Current_Info;

         end loop;
      end loop;
      return S;
   end Construct_Grid_Infos;

   --  Comparateur par ordre décroissant
   function CompareHLRatio (P1, P2 : Partition_Info) return Boolean is
   begin
      if (Float (P1.Maxx - P1.Minx + 1) / Float (P1.Maxy - P1.Miny + 1)) <
         (Float (P2.Maxx - P2.Minx + 1) / Float (P2.Maxy - P2.Miny + 1))
      then
         return True;
      else
         return False;
      end if;
   end CompareHLRatio;

   function ComparePXNumber (P1, P2 : Partition_Info) return Boolean is
   begin
      if P1.NbPixels < P2.NbPixels then
         return True;
      else
         return False;
      end if;
   end ComparePXNumber;

   --   Cet fonction construit une liste ordonnées des partitions qui
   --   ont la propriété suivante H/L maximum

   --   L'implantation utilise un tri par insertion,
   function Order_Partitions
     (S    : Partition_Info_Array;
      P    : Partition_Comparator)
      return Partition_Array
   is
      Result  : Partition_Array (S'Range) := (others => 0);
      Current : Natural                   := Result'First;
      J       : Natural;
   begin
      for I in  S'Range loop
         --  on insere l'element I dans la liste s'il correspond à un element
         --valide
         if (S (I) /= Null_Partition_Info) then
            J := Result'First;
            while J < Current and then P (S (Result (J)), S (I)) loop
               J := J + 1;
            end loop;
            -- on ajoute l'element
            Result (J + 1 .. Current) := Result (J .. Current - 1);  -- on
                                                                     --decale
                                                                     --le
                                                                     --tableau
            Current                   := Current + 1;
            Result (J)                := I;
         end if;
      end loop;
      return Result (Result'First .. Current - 1);
   end Order_Partitions;

   --
   -- Cette fonction élimine les partitions qui ne sont pas dans le tableau P
   --
   function Keep_Only_Partitions
     (G                : Grid.Unbounded_Image;
      Partition_Number : Natural;
      P                : Partition_Array;
      Nodata_Partition : Natural)
      return             Grid.Unbounded_Image
   is
      Result               : Grid.Unbounded_Image := G;
      Partition_Number_Max : Natural              := 0;
      use Grid;
   begin
      --   on construit un tableau indexé par les numéros de partition,
      --   contenant un boolean permettant de
      --   savoir si on conserve la partition, ou si celle si est éliminée,
      --   on remplace la partition
      --   par la partition "Root_Partition_Number"

      --  on recherche d'abord, le numéros max du tableau de partition,
      --  pour connaitre la taille du tableau à creer

      declare
         T : array (0 .. Partition_Number) of Boolean := (others => False);
      begin
         for I in  P'Range loop
            T (P (I))  := True; -- on conserve la partition
         end loop;

         -- T contient un tableau de boolean permettant
         -- de savoir si on conserve la partition
         for I in  1 .. Columns (Result) loop
            for J in  1 .. Rows (Result) loop
               if not T (Get_Pixel (Result, I, J)) then
                  Set_Pixel (Result, I, J, Nodata_Partition);
               end if;
            end loop;
         end loop;
      end;
      return Result;
   end Keep_Only_Partitions;

   -- fonction de fusion physique de partition dans l'image
   function Merge_Partition
     (G             : Grid.Unbounded_Image;
      Partition     : Natural;
      New_Partition : Natural;
      S             : Partition_Info)
      return          Grid.Unbounded_Image
   is
      Result : Grid.Unbounded_Image := G; -- copie de l'image
      use Grid;
   begin

      for I in  S.Minx .. S.Maxx loop
         for J in  S.Miny .. S.Maxy loop
            if Get_Pixel (Result, I, J) = Partition then
               Set_Pixel (Result, I, J, New_Partition);
            end if;
         end loop;
      end loop;
      return Result;

   end Merge_Partition;

   -- fonction de fusion physique de partition dans l'image
   function Merge_Partition
     (G             : Grid.Unbounded_Image;
      Partition     : Natural;
      New_Partition : Natural)
      return          Grid.Unbounded_Image
   is
      P : Partition_Info;
      use Grid;
   begin

      -- on parcours toute l'image, car à priori on ne connait pas l'étendue
      --...

      P.Minx := 1;
      P.Maxx := Columns (G);

      P.Miny := 1;
      P.Maxy := Rows (G);

      return Merge_Partition (G, Partition, New_Partition, P);

   end Merge_Partition;

   --
   --   Cette fonction calcule pour les différentes partitions,
   --   la distance minimale pour sortir de la partition
   --
   --   La complexité de la fonction est en 4 x o(n2)
   --
   function Construct_Exterior_Distance
     (G    : in Grid.Unbounded_Image)
      return Grid.Unbounded_Image
   is
      use Grid;
      Result : Unbounded_Image;

      Current_Partition     : Natural;
      Calculating_Partition : Natural;
      Distance              : Natural;
      Current_Partition_Set : Boolean := False;

   begin
      --   définition de la taille du grid resultat
      Set_Size (Result, Columns (G), Rows (G));

      --   on initialise l'image résultat avec l'élément neutre de l'opération
      --min -> natural'last

      for I in  1 .. Columns (G) loop
         for J in  1 .. Rows (G) loop
            Set_Pixel (Result, I, J, Natural'Last);
         end loop;
      end loop;

      -- Parcours vertical de haut en bas
      for I in  1 .. Columns (G) loop
         Current_Partition_Set := False;
         Distance              := 0;
         for J in  1 .. Rows (G) loop

            if Current_Partition_Set = False then
               Current_Partition_Set := True;
               Calculating_Partition := Get_Pixel (G, I, J);
            end if;

            --  Calculating_partition contient le numero de la partition que
            --l'on a vu precedemment
            Current_Partition := Get_Pixel (G, I, J);

            --  Current_Partition contient le numero de la partition que l'on
            --est en cours d'analyse
            if Current_Partition /= Calculating_Partition then
               --  Changement de partition, on était sur une autre partition
               --precedemment
               Distance              := 0;
               Calculating_Partition := Current_Partition;
            end if;

            Distance := Distance + 1;
            Set_Pixel
              (Result,
               I,
               J,
               Natural'Min (Distance, Get_Pixel (Result, I, J)));

         end loop;
      end loop;

      --  parcours vertical de bas en haut
      for I in  1 .. Columns (G) loop
         Current_Partition_Set := False;
         Distance              := 0;
         for J in reverse  1 .. Rows (G) loop
            if Current_Partition_Set = False then
               Current_Partition_Set := True;
               Calculating_Partition := Get_Pixel (G, I, J);
            end if;
            Current_Partition := Get_Pixel (G, I, J);
            if Current_Partition /= Calculating_Partition then
               Distance              := 0;
               Calculating_Partition := Current_Partition;
            end if;
            Distance := Distance + 1;
            Set_Pixel
              (Result,
               I,
               J,
               Natural'Min (Distance, Get_Pixel (Result, I, J)));
         end loop;
      end loop;

      --  parcours horizontal de gauche à droite
      for J in  1 .. Rows (G) loop
         Current_Partition_Set := False;
         Distance              := 0;
         for I in  1 .. Columns (G) loop
            if Current_Partition_Set = False then
               Current_Partition_Set := True;
               Calculating_Partition := Get_Pixel (G, I, J);
            end if;
            Current_Partition := Get_Pixel (G, I, J);
            if Current_Partition /= Calculating_Partition then
               Distance              := 0;
               Calculating_Partition := Current_Partition;
            end if;
            Distance := Distance + 1;
            Set_Pixel
              (Result,
               I,
               J,
               Natural'Min (Distance, Get_Pixel (Result, I, J)));
         end loop;
      end loop;

      --  parcours horizontal de droite à gauche
      for J in  1 .. Rows (G) loop
         Current_Partition_Set := False;
         Distance              := 0;
         for I in reverse  1 .. Columns (G) loop
            if Current_Partition_Set = False then
               Current_Partition_Set := True;
               Calculating_Partition := Get_Pixel (G, I, J);
            end if;
            Current_Partition := Get_Pixel (G, I, J);
            if Current_Partition /= Calculating_Partition then
               Distance              := 0;
               Calculating_Partition := Current_Partition;
            end if;
            Distance := Distance + 1;
            Set_Pixel
              (Result,
               I,
               J,
               Natural'Min (Distance, Get_Pixel (Result, I, J)));
         end loop;
      end loop;

      return Result;
   end Construct_Exterior_Distance;

   --
   --  Cette fonction liste toutes les partitions utilisée
   --
   --    En entrée, on prends le tableau d'information sur les partitions
   --
   --    En sortie, on liste les partitions
   --
   function List_Partitions
     (S    : Partition_Info_Array)
      return Partition_Array
   is
      P : Partition_Array (S'Range); -- le tableau que l'on construit
      C : Natural := S'First; -- premiere place libre
   begin
      for I in  S'Range loop
         if S (I) = Null_Partition_Info then
            P (C) := I; -- l'index représente le numéros de la partition
            C     := C + 1;
         end if;
      end loop;
      return P (S'First .. C - 1);
   end List_Partitions;

   --
   --   Cette fonction retourne la partition ayant
   --   le plus de pixels
   --
   function Calculate_Max_Pixel_Partition
     (G    : Grid.Unbounded_Image;
      N    : Natural)
      return Natural
   is
      S_Back : Partition_Info_Array := Construct_Grid_Infos (G, N);
      P_Back : Partition_Array      :=
         Order_Partitions (S_Back, ComparePXNumber'Access);
   begin
      return P_Back (P_Back'Last);
   end Calculate_Max_Pixel_Partition;

end Classification;
