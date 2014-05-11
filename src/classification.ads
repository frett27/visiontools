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

--  Package de classification d'une image..
--  Author : Freydiere Patrice

with Images; use Images;
with Images.Greyscale;
with Images.Grid;


package Classification is


   -- Information sur une partition
   -- Enveloppe englobante + Nombre de pixels
   type Partition_Info is record
      -- Envelope
      Minx, Miny : Natural := Natural'Last;
      Maxx, Maxy : Natural := Natural'First;
      NbPixels : Natural := 0;
   end record;

   type Partition_Array is array (Natural range <>) of Natural;

   Null_Partition_Info : Partition_Info;

   type Partition_Info_Array is
     array (Natural range <>) of aliased Partition_Info;

   type Partition_Info_Array_Access is access Partition_Info_Array;


   --
   --   Classification de zones dans une image
   --   Chaque zone contigue de pixel est num�rot�e avec un nombre unique
   --   dans l'image
   --
   --   Param�tres : Img , image contenant des zones contigues
   --
   --    La fonction retourne
   --      - la classification des zones dans un Grid  ( Partitioned_image )
   --      - Le nombre de partitions trouv�es dans l'image ( Partition_Number )
   --
   procedure Classify (Img : in Greyscale.Unbounded_Image;
                       Partitioned_Image : out Grid.Unbounded_Image;
                       Partition_Number : out Natural);

   --
   --  Cette fonction convertie un Grid Naturel en Image ... ,
   --  celle ci r�duit le nombre de partition � 255.
   --     (cas d'image en valeur de gris de 255 niveaux)
   --
   --  Param�tre en entr�e :
   --         Grid : L'image � convertir
   --
   --     Retour : L'image correspondant aux partitions
   --     exceptions : Too_Many_Partition -> lorsque le nombre de
   --                                        partition d�passe la
   --                                        capacit� de l'image
   function GridToImg (G : Grid.unbounded_image)
                      return Greyscale.Unbounded_Image;

   Too_Many_Partitions : exception;

   --
   --  Cette fonction r�cup�re les informations sur les partitions
   --    de l'image ...
   --    En entr�e, on demande l'image classifi�e, et le nombre de partitions
   --
   --    En Sortie, la fonction retourne un tableau d'information
   --               pour chaque partition
   --
   --
   function Construct_Grid_Infos (G : in Grid.Unbounded_Image;
                                  Partition_Number : Natural)
                                 return Partition_Info_Array;

   --
   --  Cette fonction liste toutes les partitions utilis�es
   --
   --    En entr�e, on prends le tableau d'information sur les partitions
   --
   --    En sortie, on liste les partitions
   --
   function List_Partitions (S : Partition_Info_Array)
                            return Partition_Array;

   --
   --  Cette fonction retourne la partition ayant le plus de pixels
   --
   function Calculate_Max_Pixel_Partition (G : Grid.Unbounded_Image;
                                           N : Natural)
                                          return Natural;


   type Partition_Comparator is
     access function (P1, P2 : Partition_Info) return Boolean;


   --
   --  Fonction de comparaisons pouvant �tre
   --  utilis�es dans la classification des partitions
   --
   --
   function CompareHLRatio (P1, P2 : Partition_Info) return Boolean;
   function ComparePXNumber (P1, P2 : Partition_Info) return Boolean;


   --
   --  Cette fonction classe les partitions du tableau en
   --  fonction du rapport Largeur sur Hauteur
   --
   --  Les partitions tri�es sont retourn�es
   --   dans un tableau, par ordre croissant en fonction
   --   de la fonction de comparaison pass�e en param�tres
   --
   function Order_Partitions (S : Partition_Info_Array;
                              P : Partition_Comparator)
                             return Partition_Array;

   --
   --   Cette fonction ne garde dans le grid ,
   --   que les partitions indiqu�es dans le tableau Partition_Array
   function Keep_Only_Partitions (G                : Grid.Unbounded_Image;
                                  Partition_Number : Natural;
                                  P                : Partition_Array;
                                  Nodata_Partition : Natural)
                                return Grid.Unbounded_Image;


   --
   --  Fusion de deux partitions dans un grid.
   --    La partition "Partition" est fusionn�e avec New_Partition.
   --
   --      G : Nom de l'image dans laquelle on souhaite fusionner des partitions
   --      Partition :  Num�ros de la partition que l'on souhaite fusionner
   --      New_Partition : Num�ros  de la partition � laquelle on souhaite
   --                      fusionner "Partition"
   function Merge_Partition (G             : Grid.Unbounded_Image;
                             Partition     : Natural;
                             New_Partition : Natural)
                            return Grid.Unbounded_Image;

   --
   --  Version optimis�e dans laquelle, on ne parcours que l'�tendue de la partition
   --
   --   Le param�tre S : permet de connaitre l'�tendue de la partition et
   --                    son positionnement dans l'image
   --                    Cela permet d'acc�l�rer les calculs
   --
   function Merge_Partition (G             : Grid.Unbounded_Image;
                             Partition     : Natural;
                             New_Partition : Natural;
                             S             : Partition_Info)
                            return Grid.Unbounded_Image;


   --  Cette fonction renumerote les partitions de l'image ..
   --  de 1 � nb de partition de l'image
   --  Procedure permettant de renumeroter les partitions � l'interieur de l'image
   --  Chaque zone est renumerot�e � partir de 1,
   --
   --   Param�tres en entr�e : G -> la d�finition des zones
   --              en sortie : Result -> la d�finition des zones renum�rot�e
   --              Partition_Number : Nombre de partitions trouv�es dans l'image
   --
   procedure ReNumber_Grid (G                : in     Grid.Unbounded_Image;
                            Result           :    out Grid.Unbounded_Image;
                            Partition_Number :    out Natural);



   --
   --   Cette fonction calcule la distance � l'exterieur des diff�rentes partitions
   --
   --    Param�tres en entr�e : G -> la d�finition des partitions
   --
   --    La fonction retourne un grid , indiquant, pour un pixel de la partition, la
   --    distance minimale pour "sortir" de la partition
   --
   function Construct_Exterior_Distance (G : in Grid.Unbounded_Image)
                                        return Grid.Unbounded_Image;



end Classification;
