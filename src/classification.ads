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
   --   Chaque zone contigue de pixel est numérotée avec un nombre unique
   --   dans l'image
   --
   --   Paramètres : Img , image contenant des zones contigues
   --
   --    La fonction retourne
   --      - la classification des zones dans un Grid  ( Partitioned_image )
   --      - Le nombre de partitions trouvées dans l'image ( Partition_Number )
   --
   procedure Classify (Img : in Greyscale.Unbounded_Image;
                       Partitioned_Image : out Grid.Unbounded_Image;
                       Partition_Number : out Natural);

   --
   --  Cette fonction convertie un Grid Naturel en Image ... ,
   --  celle ci réduit le nombre de partition à 255.
   --     (cas d'image en valeur de gris de 255 niveaux)
   --
   --  Paramètre en entrée :
   --         Grid : L'image à convertir
   --
   --     Retour : L'image correspondant aux partitions
   --     exceptions : Too_Many_Partition -> lorsque le nombre de
   --                                        partition dépasse la
   --                                        capacité de l'image
   function GridToImg (G : Grid.unbounded_image)
                      return Greyscale.Unbounded_Image;

   Too_Many_Partitions : exception;

   --
   --  Cette fonction récupère les informations sur les partitions
   --    de l'image ...
   --    En entrée, on demande l'image classifiée, et le nombre de partitions
   --
   --    En Sortie, la fonction retourne un tableau d'information
   --               pour chaque partition
   --
   --
   function Construct_Grid_Infos (G : in Grid.Unbounded_Image;
                                  Partition_Number : Natural)
                                 return Partition_Info_Array;

   --
   --  Cette fonction liste toutes les partitions utilisées
   --
   --    En entrée, on prends le tableau d'information sur les partitions
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
   --  Fonction de comparaisons pouvant être
   --  utilisées dans la classification des partitions
   --
   --
   function CompareHLRatio (P1, P2 : Partition_Info) return Boolean;
   function ComparePXNumber (P1, P2 : Partition_Info) return Boolean;


   --
   --  Cette fonction classe les partitions du tableau en
   --  fonction du rapport Largeur sur Hauteur
   --
   --  Les partitions triées sont retournées
   --   dans un tableau, par ordre croissant en fonction
   --   de la fonction de comparaison passée en paramètres
   --
   function Order_Partitions (S : Partition_Info_Array;
                              P : Partition_Comparator)
                             return Partition_Array;

   --
   --   Cette fonction ne garde dans le grid ,
   --   que les partitions indiquées dans le tableau Partition_Array
   function Keep_Only_Partitions (G                : Grid.Unbounded_Image;
                                  Partition_Number : Natural;
                                  P                : Partition_Array;
                                  Nodata_Partition : Natural)
                                return Grid.Unbounded_Image;


   --
   --  Fusion de deux partitions dans un grid.
   --    La partition "Partition" est fusionnée avec New_Partition.
   --
   --      G : Nom de l'image dans laquelle on souhaite fusionner des partitions
   --      Partition :  Numéros de la partition que l'on souhaite fusionner
   --      New_Partition : Numéros  de la partition à laquelle on souhaite
   --                      fusionner "Partition"
   function Merge_Partition (G             : Grid.Unbounded_Image;
                             Partition     : Natural;
                             New_Partition : Natural)
                            return Grid.Unbounded_Image;

   --
   --  Version optimisée dans laquelle, on ne parcours que l'étendue de la partition
   --
   --   Le paramètre S : permet de connaitre l'étendue de la partition et
   --                    son positionnement dans l'image
   --                    Cela permet d'accélérer les calculs
   --
   function Merge_Partition (G             : Grid.Unbounded_Image;
                             Partition     : Natural;
                             New_Partition : Natural;
                             S             : Partition_Info)
                            return Grid.Unbounded_Image;


   --  Cette fonction renumerote les partitions de l'image ..
   --  de 1 à nb de partition de l'image
   --  Procedure permettant de renumeroter les partitions à l'interieur de l'image
   --  Chaque zone est renumerotée à partir de 1,
   --
   --   Paramètres en entrée : G -> la définition des zones
   --              en sortie : Result -> la définition des zones renumérotée
   --              Partition_Number : Nombre de partitions trouvées dans l'image
   --
   procedure ReNumber_Grid (G                : in     Grid.Unbounded_Image;
                            Result           :    out Grid.Unbounded_Image;
                            Partition_Number :    out Natural);



   --
   --   Cette fonction calcule la distance à l'exterieur des différentes partitions
   --
   --    Paramètres en entrée : G -> la définition des partitions
   --
   --    La fonction retourne un grid , indiquant, pour un pixel de la partition, la
   --    distance minimale pour "sortir" de la partition
   --
   function Construct_Exterior_Distance (G : in Grid.Unbounded_Image)
                                        return Grid.Unbounded_Image;



end Classification;
