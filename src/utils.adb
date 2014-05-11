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


package body utils is


   -- Modifie l'image pour avoir un contraste Max

   procedure MaxContrast(Img: in out Unbounded_Image)  is
      MaxValue,tmpValue : pixel_value_type :=0;
      Minvalue : Pixel_Value_Type := 255;
  begin
     -- Calcule le max utilise et le min utilisé pour toute l'image
     for I in 1..rows(Img) loop
        for J in 1..Columns(Img) loop
           Tmpvalue := Get_Pixel(Img,J,I);
           if Tmpvalue >= Maxvalue then
              Maxvalue := Tmpvalue ;
           end if;
           if Tmpvalue <Minvalue then
              Minvalue := Tmpvalue;
           end if;
        end loop;
     end loop;

     -- Normalisation des couleurs en fonction des bornes
     if Maxvalue = 255 and Minvalue = 0 then
        return; -- rien à faire
     end if;

     if Minvalue = Maxvalue then
        return ; -- rien  à faire, l'image est homogène
     end if;

     -- Calcule le max utilise et le min utilisé
     for I in 1..Rows(Img) loop
        for J in 1..Columns(Img) loop
           Tmpvalue := Get_Pixel(Img,J,I);
           Tmpvalue := pixel_value_type(Float(
                                              (Tmpvalue-Minvalue)
                                              )/
                                        (float(Maxvalue-Minvalue))*255.0);
           Set_Pixel(Img,J,I,Tmpvalue);
        end loop;
     end loop;
  end;


   function "*"(Left : in Unbounded_Image;
                Right : Natural) return Unbounded_Image
   is
      TempImg : Unbounded_Image;
      Pxv:Pixel_Value_Type;
  begin
      Set_Size(TempImg,Columns(Left),Rows(Left));
      for I in 1..Columns(Left) loop
         for J in 1..Rows(Left) loop
            Pxv := Get_Pixel(Left,I,J);
            if natural(Pxv)*Right> 255 then
               Set_Pixel(tempimg,I,J,255);
            else
               Set_Pixel(tempimg,I,J,Pxv*Pixel_Value_Type(Right));
            end if;
         end loop;
      end loop;
      return Tempimg;
   end;


   function Threshold(Img : Unbounded_Image ;
                      Seuil : Pixel_Value_Type) return Binary.Unbounded_Image
   is
      use Binary;
      Result : Binary.Unbounded_Image;
   begin
      Set_Size(Result, Columns(Img), Rows(Img));
      for I in 1..Columns(Img) loop
         for J in 1..Rows(Img) loop
            if Get_Pixel(Img, I , J ) < Seuil then
               Set_Pixel(Result, I, J, Binary.Pixel_Value_Type'First);
            else
               Set_Pixel(Result, I, J, Binary.Pixel_Value_Type'last);
            end if;
         end loop;
      end loop;
      return Result;
   end Threshold;

   ---------------
   -- Rotate_90 --
   ---------------

   function Rotate_90(Img : Unbounded_Image;
                      Dir : Direction)
                     return Unbounded_Image
   is
      Result : Unbounded_Image;
      Width : Column_Index_Type := Columns(Img);
      Height : Row_Index_Type := Rows(Img);

      StartX, StartY  : Natural;
      StepX , StepY : Integer;

      CurrentX, CurrentY : Natural;

   begin

      Set_Size(Result,Height, Width);

      case Dir is
         when Left =>
            Startx := 1;
            StepX := 1;
            StartY := Width;
            StepY := -1;
         when Right =>
            Startx := Height;
            StepX := -1;
            StartY := 1;
            StepY := 1;
      end case;

      Currenty := Starty;
      for I in 1..Width loop -- hauteur de la nouvelle image
         CurrentX := Startx;
         for J in 1..Height loop -- largeur

            Set_Pixel(Result,
                      CurrentX,
                      CurrentY ,
                      Get_Pixel(Img, I, J));


            Currentx := Currentx + StepX;
         end loop;
         Currenty := Currenty + StepY;
      end loop;

      return Result;
   end Rotate_90;


   function To_GreyScale(Img : Binary.Unbounded_Image)
                        return Greyscale.Unbounded_Image
   is
      use Binary;
      Result : Greyscale.Unbounded_Image;
   begin
      Greyscale.Set_Size(Result, Columns(Img), Rows(Img));
      for I in 1..Columns(Img) loop
         for J in 1..Rows(Img) loop
            if Get_Pixel(Img, I, J) = Binary.Pixel_Value_Type'First then
               Greyscale.Set_Pixel(Result, I, J, Greyscale.Pixel_Value_Type'First );
            else
               Greyscale.Set_Pixel(Result, I, J, Greyscale.Pixel_Value_Type'Last );
            end if;
         end loop;

      end loop;

      return Result;
   end To_GreyScale;


   function Resize_Bilinear( U : Color.Unbounded_Image ;
                            New_Width : Column_Index_Type;
                            New_Height : Row_Index_Type)
                          return Color.Unbounded_image
   is
      use Color;
      Result : Color.Unbounded_image;
   begin
      Set_Size(Result, New_Width, New_Height);

      for Ip in 1..New_Width loop
         for Jp in 1..New_Height loop
            declare
               Newpix : Color_Pixel_Type;
               X : Float := (Float(Ip) - 1.0)
                 / Float(New_Width) * Float(Columns(U)) + 1.0;
               Y : Float := (Float(Jp) - 1.0)
                 / Float(New_Height) * Float(Rows(U)) + 1.0;
               I : Natural := Natural(X);
               J : Natural := Natural(Y);
               Dx : Float := X - Float(I);
               Dy : Float := Y - Float(J);
               cR, cV, cB : Float := 0.0;

               P: Color_Pixel_Type :=
                 Color.Get_Pixel_Unbounded(U, I, J, (0, 0, 0));
               Px : Color_Pixel_Type :=
                 Color.Get_Pixel_Unbounded(U, I + 1, J, (0, 0, 0));
               Py : Color_Pixel_Type :=
                 Color.Get_Pixel_Unbounded(U, I, J + 1, (0, 0, 0));
               Pxy : Color_Pixel_Type :=
                 Color.Get_Pixel_Unbounded(U, I + 1, J + 1, (0, 0, 0));
            begin
               Cr := (1.0 - Dx) * (1.0 - Dy) * Float(P.R)
                 + Dx * (1.0 - Dy) * Float(Px.R)
                 + Dy * (1.0 - Dx) * Float(Py.R)
                 + Dx * Dy * Float(Pxy.R);

               Cv := (1.0 - Dx) * (1.0 - Dy) * Float(P.v)
                 + Dx * (1.0 - Dy) * Float(Px.v)
                 + Dy * (1.0 - Dx) * Float(Py.v)
                 + Dx * Dy * Float(Pxy.v);

               Cb := (1.0 - Dx) * (1.0 - Dy) * Float(P.b)
                 + Dx * (1.0 - Dy) * Float(Px.b)
                 + Dy * (1.0 - Dx) * Float(Py.b)
                 + Dx * Dy * Float(Pxy.b);


               if Cr < 0.0 then
                  Cr := 0.0;
               end if;

               if Cr > Float(Color_Byte'Last) then
                  Cr := Float(Color_Byte'Last);
               end if;

               if Cv < 0.0 then
                  Cv := 0.0;
               end if;

               if Cv > Float(Color_Byte'Last) then
                  Cv := Float(Color_Byte'Last);
               end if;

               if Cb < 0.0 then
                  Cb := 0.0;
               end if;

               if Cb > Float(Color_Byte'Last) then
                  Cb := Float(Color_Byte'Last);
               end if;

               Newpix := (R => Color_byte(Cr),
                          V=>Color_byte(Cv),
                          B => Color_byte(Cb));
               Set_Pixel(Result, Ip, Jp, Newpix);
            end;
         end loop;
      end loop;
      return Result;
   end;

--  FUNCTION Bicubic (x,y:real) : byte;
--  var j,k:integer;
--  var a,b:real;
--  var dest:real;
--  var t1,t2,t3,t4:real;
--  var color:integer;
--  begin
--    j:=round (x);
--    k:=round (y);
--    a:=x-j;
--    b:=y-k;
--    t1:=-a*(1-a)*(1-a)*RC(j-1,k-1)+
--           (1-2*a*a+a*a*a)*RC(j,k-1)+
--         a*(1+a-a*a)*RC(j+1,k-1)-
--         a*a*(1-a)*RC(j+2,k-1);
--    t2:=-a*(1-a)*(1-a)*RC(j-1,k)+
--           (1-2*a*a+a*a*a)*RC(j,k)+
--         a*(1+a-a*a)*RC(j+1,k)-
--         a*a*(1-a)*RC(j+2,k);
--    t3:=-a*(1-a)*(1-a)*RC(j-1,k+1)+
--           (1-2*a*a+a*a*a)*RC(j,k+1)+
--         a*(1+a-a*a)*RC(j+1,k+1)-
--         a*a*(1-a)*RC(j+2,k+1);
--    t4:=-a*(1-a)*(1-a)*RC(j-1,k+2)+
--           (1-2*a*a+a*a*a)*RC(j,k+2)+
--         a*(1+a-a*a)*RC(j+1,k+2)-
--         a*a*(1-a)*RC(j+2,k+2);

--    dest:= -b*(1-b)*(1-b)*t1+
--              (1-2*b*b+b*b*b)*t2+
--            b*(1+b-b*b)*t3+
--            b*b*(b-1)*t4;
--    color:=round(dest);
--    if color<0 then color:=abs(color);  // cannot have negative values !
--    if (color>gifrec.colors) then color:=gifrec.colors; //
--    Bicubic:=color;
--  end;

   type Pixel_float is record
      R,V,B : Float;
   end record;

   function "*"(P : Pixel_Float; F : Float) return Pixel_Float is
      Retvalue : Pixel_Float;
   begin
      Retvalue.R := p.R * F;
      Retvalue.v := p.v * F;
      Retvalue.b := p.b * F;
      return Retvalue;
   end;

   function "*"(F : Float; P : Pixel_Float ) return Pixel_Float is
   begin
      return "*"(P,F);
   end;

   function "-"(P : Pixel_Float) return Pixel_Float is
   begin
      return "*"(-1.0,P);
   end;

   function "+"(P1,P2 : Pixel_Float) return Pixel_Float is
   begin
      return Pixel_Float'(R=>P1.R + P2.R,
                          V=> P1.V + P2.V,
                          B => P1.B + P2.B);
   end;

   function "-"(P1,P2 : Pixel_Float) return Pixel_Float is
   begin
      return P1 + (- P2);
   end;


   function Resize_Bicubic( U : Color.Unbounded_Image ;
                            New_Width : Column_Index_Type;
                            New_Height : Row_Index_Type)
                          return Color.Unbounded_image
   is
      use Color;

      function To_Float(C : Color_Pixel_Type) return Pixel_Float is
         Ret : Pixel_Float;
      begin
         Ret.R := Float(C.R);
         Ret.v := Float(C.V);
         Ret.b := Float(C.B);
         return Ret;
      end;

      function To_Color(F : Float) return Color_Byte is
      begin
         if F < Float(Color_byte'First) then
            return Color_byte'First;
         end if;

         if F > Float(Color_byte'Last) then
            return Color_byte'Last;
         end if;

         return Color_byte(F);
      end;



      Result : Color.Unbounded_image;
   begin
      Set_Size(Result, New_Width, New_Height);
      for Ip in 1..New_Width loop
         for Jp in 1..New_Height loop

            declare
               X : Float := (Float(Ip) - 1.0)
                 / Float(New_Width) * Float(Columns(U)) + 1.0;
               Y : Float := (Float(Jp) - 1.0)
                 / Float(New_Height) * Float(Rows(U)) + 1.0;
               j : Natural := Natural(X);
               k : Natural := Natural(Y);

               a : Float := X - Float(j);
               b : Float := Y - Float(k);


               --    t1:=-a*(1-a)*(1-a)*RC(j-1,k-1)+
               --           (1-2*a*a+a*a*a)*RC(j,k-1)+
               --         a*(1+a-a*a)*RC(j+1,k-1)-
               --         a*a*(1-a)*RC(j+2,k-1);

               T1 : Pixel_Float :=
                 -A * (1.0 - A)**2 * To_Float(Color.Get_Pixel_Unbounded(U, J - 1, k - 1, (0, 0, 0)))
                 +(1.0 - 2.0*A**2 + A**3) * To_Float(Color.Get_Pixel_Unbounded(U, J, k - 1, (0, 0, 0)))
                 + A * (1.0 + A - A**2) * To_Float(Color.Get_Pixel_Unbounded(U, J + 1, k - 1, (0, 0, 0)))
                 - A **2 * (1.0 - A) * To_Float(Color.Get_Pixel_Unbounded(U, J + 2, k - 1, (0, 0, 0)));

               --    t2:=-a*(1-a)*(1-a)*RC(j-1,k)+
               --           (1-2*a*a+a*a*a)*RC(j,k)+
               --         a*(1+a-a*a)*RC(j+1,k)-
               --         a*a*(1-a)*RC(j+2,k);


               T2 : Pixel_Float :=
                 -A * (1.0 - A)**2 * To_Float(Color.Get_Pixel_Unbounded(U, J - 1, k, (0, 0, 0)))
                 +(1.0 - 2.0*A**2 + A**3) * To_Float(Color.Get_Pixel_Unbounded(U, J, k, (0, 0, 0)))
                 + A * (1.0 + A - A**2) * To_Float(Color.Get_Pixel_Unbounded(U, J + 1, k, (0, 0, 0)))
                 - A **2 * (1.0 - A) * To_Float(Color.Get_Pixel_Unbounded(U, J + 2, k, (0, 0, 0)));

               --    t3:=-a*(1-a)*(1-a)*RC(j-1,k+1)+
               --           (1-2*a*a+a*a*a)*RC(j,k+1)+
               --         a*(1+a-a*a)*RC(j+1,k+1)-
               --         a*a*(1-a)*RC(j+2,k+1);
               T3 : Pixel_Float :=
                 -A * (1.0 - A)**2 * To_Float(Color.Get_Pixel_Unbounded(U, J - 1, K+1, (0, 0, 0)))
                 +(1.0 - 2.0*A**2 + A**3) * To_Float(Color.Get_Pixel_Unbounded(U, J, K+1, (0, 0, 0)))
                 + A * (1.0 + A - A**2) * To_Float(Color.Get_Pixel_Unbounded(U, J + 1, K+1, (0, 0, 0)))
                 - A **2 * (1.0 - A) * To_Float(Color.Get_Pixel_Unbounded(U, J + 2, K+1, (0, 0, 0)));

               --    t4:=-a*(1-a)*(1-a)*RC(j-1,k+2)+
               --           (1-2*a*a+a*a*a)*RC(j,k+2)+
               --         a*(1+a-a*a)*RC(j+1,k+2)-
               --         a*a*(1-a)*RC(j+2,k+2);

               T4 : Pixel_Float :=
                 -A * (1.0 - A)**2 * To_Float(Color.Get_Pixel_Unbounded(U, J - 1, K+2, (0, 0, 0)))
                 +(1.0 - 2.0*A**2 + A**3) * To_Float(Color.Get_Pixel_Unbounded(U, J, K+2, (0, 0, 0)))
                 + A * (1.0 + A - A**2) * To_Float(Color.Get_Pixel_Unbounded(U, J + 1, K+2, (0, 0, 0)))
                 - A **2 * (1.0 - A) * To_Float(Color.Get_Pixel_Unbounded(U, J + 2, K+2, (0, 0, 0)));

               --    dest:= -b*(1-b)*(1-b)*t1+
               --              (1-2*b*b+b*b*b)*t2+
               --            b*(1+b-b*b)*t3+
               --            b*b*(b-1)*t4;

               Dest : Pixel_Float :=
                 -B * (1.0 - B) ** 2 * T1
                 +(1.0 - 2.0 * B ** 2 + B**3)*T2
                 + B * (1.0 + B - B ** 2) * T3
                 + B **2 * (b - 1.0) * T4;

               Newpix : Color_Pixel_Type := ( R=> To_Color(Dest.R),
                                           V=> To_Color(Dest.V),
                                           B=> To_Color(Dest.B));

            begin
               Set_Pixel(Result, Ip, Jp, Newpix);
            end;
         end loop;
      end loop;
      return Result;
   end;



end utils;
