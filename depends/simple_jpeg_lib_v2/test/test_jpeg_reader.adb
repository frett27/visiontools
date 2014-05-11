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

-- Test Program

with Simple_Jpeg_lib;use simple_jpeg_lib ;
with Text_Io;
with Interfaces.C;

-- Symbolic Trace Stack
with GNAT.Traceback.Symbolic;
-- this programme read a sample image and
-- write a thumbnail which size is 1/8 of the original

procedure test_jpeg_reader is

   J : Jpeg_Handle_decompress;
   JC : Jpeg_Handle_Compress;
   I:Integer := 0;

begin

       -- ouverture de l'image, and decompress it with 1/8 of the original size
      Open_Jpeg("sample.jpg",J,8);

      -- Get Informations
      declare
         W,H : Dimension ;
         Jpegtype : Jpeg_Type;
      begin
         Get_Image_Info(J,W,H,jpegtype);

         Text_Io.Put(" Width :" & dimension'Image(W)
                     & " , height " & dimension'Image(h)
                     & " , nb comp : " & Jpeg_type'Image(jpegtype) );

         Create_Jpeg("output.jpg",Jc,W,H,jpegtype);
      end;

      declare
      begin
         while True loop
            declare
               Comp : Component_Array := Read_Next_Line(J);
            begin
               Write_Line(JC,Comp);
               I:=I+1; -- count the lines
            end;
         end loop;
      exception
         when JPEG_EXCEPTION  =>
            null;
      end;

      Text_Io.New_Line;
      Text_Io.Put( Integer'Image(I) & " read Lines ");

      Close_Jpeg(J);
      Close_Jpeg(jc);

      Text_Io.Put("End  ");
      Text_Io.New_Line;

exception
        when E : others =>
           Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

end;
