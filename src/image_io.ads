
---
---
---  I/O function for writing PPM files / and JPEG files...
---
---
---

with Images.Greyscale;
with Images.Binary;
with Images.Color;

use Images;


package Image_Io is

   -- greyscale images ...

   procedure Write_Image (FileName : String;
                          Img: in Greyscale.Unbounded_Image);

   procedure Read_Image(FileName : String;
                        Img : in out Greyscale.Unbounded_Image);

   procedure Read_Jpeg_Image(FileName:String;
                             Img : out Greyscale.Unbounded_Image;
                            Scale : in Integer := 1);

   procedure Write_Jpeg_Image(FileName:String;
                              Img : in Greyscale.Unbounded_Image);

   -- binary images ...

   procedure Write_Image (FileName : String;
                          Img: in Binary.Unbounded_Image);

   procedure Read_Image(FileName : String;
                        Img : in out Binary.Unbounded_Image);

   -- color images ...

   procedure Read_Jpeg_Image(FileName:String;
                             Img : out Color.Unbounded_Image;
                            Scale : in Integer := 1);

   procedure Write_Jpeg_Image(FileName:String;
                              Img : in Color.Unbounded_Image);


end Image_Io;
