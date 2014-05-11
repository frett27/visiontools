
with Open_Image;
with Open_Image.Ppm_Io;


with Simple_Jpeg_Lib;

with Text_io; use Text_Io;

package body Image_Io is


   -- Procedure de lecture et d'ecriture d'un fichier PPM

   procedure Set_Size (
                       TheImage  : in out Greyscale.unbounded_image;
                       Max_Color : in      Open_Image.Pixel_Value_Type;
                       Columns   : in      Open_Image.Column_Index_Type;
                       Rows      : in      Open_Image.Row_Index_Type  ) is
      use Greyscale;
   begin
      Set_Size(TheImage,Positive(Columns),Positive(Rows));
   end;

   procedure Set_Pixel (
                        Img  : in out Greyscale.unbounded_image;
                        Column : in     Open_Image.Column_Index_Type;
                        Row    : in     Open_Image.Row_Index_Type;
                        Value  : in     Open_Image.Color_Channels_Type ) is
      use Greyscale;
      Px : Pixel_Value_type;

   begin
      Px :=  Pixel_Value_Type ( (Natural (Value.Red)
                                 + Natural ( Value.Green )
                                 + Natural (Value.Blue)) / 3) ;

      Set_Pixel(img,
                Column_Index_Type( Column),
                Row_Index_Type(Row),
                px);
   end;


   procedure Read_PPM is
      new Open_Image.Ppm_Io.Read_Ppm (
                                     Image_Type => Greyscale.Unbounded_Image,
                                     Set_Size   => Set_Size,
                                     Set_Pixel  => Set_Pixel);


   function Get_Pixel (
                       Img  : in     Greyscale.unbounded_image;
                       Column : in     Open_Image.Column_Index_Type;
                       Row    : in     Open_Image.Row_Index_Type     )
                      return Open_Image.Color_Channels_Type is
      use Greyscale;
      R : Open_Image.Color_Channels_Type;
   begin
      -- Lecture primaire
      R.Red := Open_Image.Pixel_Value_Type(Get_Pixel(Img,
                                                     Column_Index_Type(Column),
                                                     Row_Index_Type(Row)));
      R.Green := R.Red;
      R.Blue := R.Green;
      return r;
   end Get_Pixel;



   procedure Write_Ppm is
      new Open_Image.Ppm_Io.Write_Ppm
     (Image_Type => Greyscale.unbounded_image,
      Get_Pixel  => Get_Pixel);





   procedure Write_image (FileName : String;
                          Img: in Greyscale.unbounded_image) is
      use Greyscale;
   begin
      Write_Ppm (
                 Name                  => Filename,
                 Max_Pixel_Value       => Open_Image.Pixel_Value_Type(Pixel_Value_Type'Last),
                 Columns               => Open_Image.Column_Index_Type( Columns(Img)),
                 Rows                  => Open_Image.Row_Index_Type( Rows(Img)),
                 Store_Pixel_Values_As => Open_Image.Ppm_Io.Ascii_Data,
                 Image                 => img);
   end Write_image;


   procedure Read_image (FileName : String;
                         Img: in out Greyscale.unbounded_image) is

   begin
      Read_Ppm(Filename , Img);
   end;

   -- binary images


   procedure Set_Size (
                       TheImage  : in out Binary.unbounded_image;
                       Max_Color : in      Open_Image.Pixel_Value_Type;
                       Columns   : in      Open_Image.Column_Index_Type;
                       Rows      : in      Open_Image.Row_Index_Type  ) is
      use Binary;
   begin
      Set_Size(TheImage,Positive(Columns),Positive(Rows));
   end;

   procedure Set_Pixel (
                        Img  : in out Binary.unbounded_image;
                        Column : in     Open_Image.Column_Index_Type;
                        Row    : in     Open_Image.Row_Index_Type;
                        Value  : in     Open_Image.Color_Channels_Type ) is
      use Binary;
      Px : Pixel_Value_type;

   begin
      if Open_Image."=" (Value.Red , 255) then
         Px := Pixel_Value_Type'Last;
      else
         Px := Pixel_Value_Type'First;
      end if;

      Set_Pixel(img,
                Column_Index_Type( Column),
                Row_Index_Type(Row),
                px);
   end;


   procedure Read_PPM is
      new Open_Image.Ppm_Io.Read_Ppm (
                                     Image_Type => Binary.Unbounded_Image,
                                     Set_Size   => Set_Size,
                                     Set_Pixel  => Set_Pixel);


   function Get_Pixel (
                       Img  : in     Binary.unbounded_image;
                       Column : in     Open_Image.Column_Index_Type;
                       Row    : in     Open_Image.Row_Index_Type     )
                      return Open_Image.Color_Channels_Type is
      use binary;
      R : Open_Image.Color_Channels_Type;
   begin
      if Get_Pixel(Img,
                   Column_Index_Type(Column),
                   Row_Index_Type(Row)) = Pixel_Value_Type'First then
         R := (others => 0);
      else
         R := (others => 255);
      end if;
      return r;
   end Get_Pixel;



   procedure Write_Ppm is
      new Open_Image.Ppm_Io.Write_Ppm
     (Image_Type => Binary.unbounded_image,
      Get_Pixel  => Get_Pixel);





   procedure Write_image (FileName : String;
                          Img: in binary.unbounded_image) is
      use binary;
   begin
      Write_Ppm (
                 Name                  => Filename,
                 Max_Pixel_Value       => 255,
                 Columns               => Open_Image.Column_Index_Type( Columns(Img)),
                 Rows                  => Open_Image.Row_Index_Type( Rows(Img)),
                 Store_Pixel_Values_As => Open_Image.Ppm_Io.Ascii_Data,
                 Image                 => img);
   end Write_image;


   procedure Read_image (FileName : String;
                         Img: in out binary.unbounded_image) is

   begin
      Read_Ppm(Filename , Img);
   end;






   -- Read the Jpeg File ...

   procedure Read_Jpeg_Image(FileName : String ;
                             Img : out Greyscale.Unbounded_Image;
                             Scale : in Integer := 1) is

      use Simple_Jpeg_Lib;
      use Greyscale;
      Retimg : Unbounded_Image;
      J : Simple_Jpeg_Lib.Jpeg_Handle_Decompress;
      Width, Height : Dimension;
      JT : Jpeg_Type;
      R,V,B : Byte;

   begin

      Open_Jpeg(FileName , J , Scale ); -- ouverture du fichier JPEG
      Get_Image_Info(J,Width,Height,JT);

      Set_Size( Retimg ,
                Column_Index_Type(Width),
                Row_Index_type(Height) );

      -- Put_line("Largeur de l'image d'origine :" & Dimension'Image(Width));
      --      Log("Hauteur de l'image d'origine :" & Interfaces.C.Int'Image(Height));

      for I in 1..Height loop
         -- Lecture d'un ligne du fichier Jpeg
         declare
            C : Component_Array := Read_Next_Line(J);
            Value : Pixel_Value_Type;
         begin

            for J in 1..Width loop
               case Jt is
                  when RVB =>
                     R := C( (J-1) * 3 + C'First );
                     V := C( (J-1) * 3 + C'First + 1 );
                     B := C( (J-1) * 3 + C'First + 2 );
                     Value := Pixel_Value_Type ( (Integer(R) + Integer(V) + Integer(B)) / 3 );
                  when Grey =>
                     Value := Pixel_Value_Type(C(J));
               end case;


               Set_Pixel(Retimg,
                         Column_Index_Type(J),
                         Row_Index_Type(I),
                         Value
                         );
            end loop;
         end;
      end loop;
      Close_Jpeg(J);
      Img := Retimg;

   end Read_Jpeg_Image;

   --
   --
   -- Write image as Jpeg Image ...
   --
   procedure Write_Jpeg_Image(FileName:String;
                              Img : in Greyscale.Unbounded_Image) is
      use Greyscale;
      use Simple_Jpeg_Lib;
      J : Jpeg_Handle_Compress;
      C : Component_Array(1 .. Columns (Img) * 3 );
   begin

      Create_Jpeg(Filename,
                  J ,
                  Columns(Img),
                  Rows(Img),
                  RVB);

      for I in 1..Rows(Img) loop
         for k in 1..Columns(Img) loop
            declare
               V : Pixel_Value_Type := Get_Pixel (Img, K, I);
            begin
               C((K-1) * 3 + 1) := Byte(V);
               C((K-1) * 3 + 2) := Byte(V);
               C((K-1) * 3 + 3) := Byte(V);
            end;
         end loop;
         Write_Line (J, C);
      end loop;
      Close_Jpeg(J);
   end Write_Jpeg_Image;




   procedure Read_Jpeg_Image(FileName:String;
                             Img : out Color.Unbounded_Image;
                             Scale : in Integer := 1) is

      use Color;
      use Simple_Jpeg_Lib;
      Retimg : Unbounded_Image;
      J : Simple_Jpeg_Lib.Jpeg_Handle_Decompress;
      Width, Height : Dimension;
      JT : Jpeg_Type;
      R,V,B : Byte;

   begin

      Open_Jpeg(FileName , J , Scale ); -- ouverture du fichier JPEG
      Get_Image_Info(J,Width,Height,JT);

      Set_Size( Retimg ,
                Column_Index_Type(Width),
                Row_Index_type(Height) );

      --Put_line("Largeur de l'image d'origine :" & Dimension'Image(Width));
      --      Log("Hauteur de l'image d'origine :" & Interfaces.C.Int'Image(Height));

      for I in 1..Height loop
         -- Lecture d'un ligne du fichier Jpeg
         declare
            C : Component_Array := Read_Next_Line(J);
            Value : Pixel_Value_Type;
         begin

            for J in 1..Width loop
               case Jt is
                  when RVB =>
                     R := C( (J-1) * 3 + C'First );
                     V := C( (J-1) * 3 + C'First + 1 );
                     B := C( (J-1) * 3 + C'First + 2 );
                     Value := Pixel_Value_Type' ( R=>Color_byte(R), V=>Color_byte(V), B=>Color_byte(B) );
                  when Grey =>
                     Value := Pixel_Value_Type'(R=>Color_byte(C(J-1)),
                                                V=>Color_byte(C(J-1)),
                                                B=>Color_byte(C(J-1)));
               end case;


               Set_Pixel(Retimg,
                         Column_Index_Type(J),
                         Row_Index_Type(I),
                         Value
                         );
            end loop;
         end;
      end loop;
      Close_Jpeg(J);
      Img := Retimg;

   end Read_Jpeg_Image;


   procedure Write_Jpeg_Image(FileName:String;
                              Img : in Color.Unbounded_Image)
   is
      use color;
      use Simple_Jpeg_Lib;
      J : Jpeg_Handle_Compress;
      C : Component_Array(1 .. Columns (Img) * 3 );
   begin

      Create_Jpeg(Filename,
                  J ,
                  Columns(Img),
                  Rows(Img),
                  RVB);

      for I in 1..Rows(Img) loop
         for k in 1..Columns(Img) loop
            declare
               V : Pixel_Value_Type := Get_Pixel (Img, K, I);
            begin
               C((K-1) * 3 + 1) := Byte(V.R);
               C((K-1) * 3 + 2) := Byte(V.V);
               C((K-1) * 3 + 3) := Byte(V.B);
            end;
         end loop;
         Write_Line (J, C);
      end loop;
      Close_Jpeg(J);
   end Write_Jpeg_Image;

end Image_Io;

