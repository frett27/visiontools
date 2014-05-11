
-----------------------------------------------------------------------
--         Simple JPEG Library - a libjpeg binding for ADA           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                           Freydiere P.                            --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  Program test for jpeglib binding ..
--  this program create thumbnails for a directory tree in a separate directory
--  the destination folder will have the same folder tree as the source one.

with Simple_Jpeg_Lib;use Simple_Jpeg_Lib ;
with Text_Io;

with Gnat.Directory_Operations;
with Gnat.Os_Lib;

with ADA.Characters.Handling;

with Gnat.Traceback.Symbolic;

with Ada.Command_Line;

-- this programme read a sample image and
-- write a thumbnail which size is 1/8 of the original

procedure Makethumbnails is

   --
   -- Test Directory existance ...
   --
   function IsDirectoryExist(Path : in String) return Boolean is
      use Gnat.Directory_Operations;
      D : Dir_Type;
   begin
      Open(D , Dir_Name_Str(Path));
      Close(D);
      return True;
   exception
      when Directory_Error =>
         return false;
   end;


   --
   -- Create the thumbnails in the related directories
   --
   function MakeThumbnail(NomJpeg : in String;
                          AbsoluteInDirectory : in String ;
                          AbsoluteOutDirectory : in String ) return String is

      J : Jpeg_Handle_Decompress;
      JC : Jpeg_Handle_Compress;
      W , H : Dimension;
      Jpegtype : Jpeg_Type;

      use Text_Io;

   begin

      Put_Line("Opening JpegFile " & Absoluteindirectory & "\" & NomJpeg );
      -- ouverture de l'image, and decompress it with 1/8 of the original size

      Open_Jpeg(Absoluteindirectory & "\" & NomJpeg , J , 8 );

      begin

         -- Get current Jpeg Informations
         Get_Image_Info( J , W , H , JpegType );

         Put_Line(" Image Characteristics "
                  & Jpeg_Type'Image(Jpegtype) & " "
                  & Dimension'Image(W) & "x" & Dimension'Image(h));

         -- Create a JpegFile
         Create_Jpeg(AbsoluteOutDirectory & "\" & NomJpeg, Jc, W, H, Jpegtype);

         for I in 1..H loop
            declare
               c : Component_Array := Read_Next_Line(J);
            begin
               Write_Line(JC, C);
            end;
         end loop;

         Close_Jpeg(Jc);

      exception
         when others =>
            Put_Line("Fermeture du fichier");
            Close_Jpeg(J);
      end;
      return  NomJpeg;
   end;


   -- This procedure read the content of the RelativeDir path
   -- and make the thumbnails for the jpegs found in the directory

   -- source and destination path doesn't have trailing slash
   procedure RecurseIntoDir(SourceDir : in String;
                            DestinationDir : in String) is
      use Gnat.Directory_Operations;
      use Ada.Characters.Handling; -- for upper ...
      use Gnat.Os_Lib;

      DirName : Gnat.Directory_Operations.Dir_Name_Str :="";
      CurrentDir :  Gnat.Directory_Operations.Dir_Type ;
      NextFichier : String(1..50) ;
      N : Natural := 0;

   begin
      Text_io.Put_Line("Entering path :" & Sourcedir );
      Open( CurrentDir, Sourcedir );
      Read(Currentdir, NextFichier, N );

      while ( N /= 0 ) loop

         declare
            FileName : String := Nextfichier(1..N);
         begin

            if Filename /= "." and Filename /=".." then
               if Is_Directory( Sourcedir & "\" & Filename ) then
                  Text_Io.Put( sourcedir & "\" & Filename );
                  Text_Io.New_Line;

                  if not Isdirectoryexist(Destinationdir & "\" & Filename) then
                     -- Create the directory for the thumbnails
                     Make_Dir(Dir_Name_Str( destinationdir & "\" & Filename ));
                  end if;
                  -- Make Thumbnails
                  RecurseIntoDir(Sourcedir & "\" & Filename ,
                                 DestinationDir & "\" &  Filename );
               else
                  -- looking for a jpeg file ...
                  if Filename'Length >= 4 then
                     if To_Upper( Filename( Filename'Last - 3 .. Filename'Last ) ) = ".JPG" then
                        Text_Io.Put("Creation of the thumbnail "
                                    & Destinationdir
                                    & "\" & Filename);

                        Text_Io.New_Line;

                        begin
                           Text_Io.Put(MakeThumbnail(  Filename  ,
                                                       Sourcedir  ,
                                                       Destinationdir )
                                       & " thumnail generated");
                           Text_Io.New_Line;

                        exception
                           when Simple_Jpeg_Lib.Jpeg_Exception =>
                              Text_Io.Put("Fail to create thumbnail ");
                        end;
                     end if;
                  end if; -- filename'length >=4
               end if; -- isdirectory
            end if;

         end;
         Read(Currentdir, NextFichier, N); -- next file
      end loop;
      Close(CurrentDir);

   end;

   -- Main Program
begin

   -- get the arguments on the command line
   if Ada.Command_Line.Argument_Count < 2 then
      -- print help on the command line
      Text_Io.Put_Line(" MakeThumbnails - Patrice Freydiere - 2004 - Command line help ...");
      Text_Io.Put_Line(" this program create jpeg thumbnail for all subfolders in the given SOURCE_FOLDER");
      Text_Io.New_Line;
      Text_Io.Put_Line("    MakeThumbnails SOURCE_FOLDER DESTINATION_FOLDER ");
      return;
   end if;


     -- List Sub Directories and recurse
     Recurseintodir(Ada.Command_Line.Argument(1),
                    Ada.Command_Line.Argument(2));


exception
   when E: others  =>
      Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end;
