---------------------------------------------------------------------
--          Open_Image - Portable Pixmap (PPM) IO
--
--                     Copyright (C) 2000
--                     Jeffrey Creem
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public
-- License along with this library; if not, write to the
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,
-- Boston, MA 02111-1307, USA.
--
-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this  unit  does not  by itself cause  the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file  might be covered by the  GNU Public License.
---------------------------------------------------------------------

--  <description>
--
--  This package defines routines to allow for reading and writing
--  of portable pixmap (PPM) files. PPM is a simple standard for
--  storing color pixmap. It is capable of storing the pixel data either
--  as ASCII numeric values or in raw binary form.
--
--  </description>
--

with Open_Image;
with Ada.Streams.Stream_IO;
with Ada.Characters.Handling;
with IO_Exceptions;

-- for debug purpose
with Ada.Exceptions;
with Gnat.Traceback.Symbolic;
with Text_Io;


package body Open_Image.PPM_IO is


  --
  -- Declare a byte type to be used when we are writing the output data
  -- as Raw_Bits.
  --
  type Unsigned_Byte is mod 2**8;
  for Unsigned_Byte'Size use 8;

  --
  -- PPM files must start with a two character string that specifies
  -- that the file contains a PPM image and defines the type of
  -- data that is stored for the pixels.
  --
  ASCII_PPM_Magic_String    : constant String := "P3";
  Raw_Bits_PPM_Magic_String : constant String := "P6";

  --
  -- Note that the PPM file standard does not require a specific
  -- End_Of_Line marker to be used so we use the combination of
  -- a line feed and a return to make it "PC" friendly (and it is
  -- still easy to view under Unix.
  --
  End_Of_Line : constant String := ASCII.LF & ASCII.CR;

  Comment_Character : constant Character := '#';


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Is_A_PPM_File
  --
  -- PURPOSE    : Checks the contents of the file to determine if it is PPM
  --              file.
  --
  -- NOTES      : At the moment this just checks the "magic number" information
  --              in the file header.
  --
  -----------------------------------------------------------------------------
  function Is_A_PPM_File (
        Name : in     String )
    return Boolean is

    File         : Ada.Streams.Stream_IO.File_Type;
    Magic_String : String (1 .. 2);


  begin

    Ada.Streams.Stream_IO.Open(
      File => File,
      Mode => Ada.Streams.Stream_IO.In_File,
      Name => Name);

    begin

      String'Read(Ada.Streams.Stream_IO.Stream(File), Magic_String);
      Ada.Streams.Stream_IO.Close(File);

    exception

      when others =>

        Ada.Streams.Stream_IO.Close(File => File);
        --
        -- If we got this far the file opened but for whatever
        -- reason, we could not process its data. Lets assume it is
        -- not a PPM file.
        --
        return False;

    end;

    return Magic_String = ASCII_PPM_Magic_String or Magic_String =
      Raw_Bits_PPM_Magic_String;

  end Is_A_PPM_File;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Is_Whitespace
  --
  -- PURPOSE    : Checks to see if the given character is a whitespace
  --              character as defined by the PPM specification.
  --
  -- NOTES      : None
  --
  -----------------------------------------------------------------------------

  function Is_Whitespace (
        Char : in     Character )
    return Boolean is

  begin
    return Char = ASCII.HT or Char = ASCII.LF or Char = ASCII.CR or
      Char = ' ';
  end Is_Whitespace;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Skip_Whitespace_And_Comments
  --
  -- PURPOSE    : Skips all comment lines and whitespace in the given
  --              file starting at the current file location. When this
  --              procedure completes the file will be positioned at the
  --              next non whitespace character on a non-comment line.
  --
  -- NOTES      : This procedure will consume input from the file until
  --              the exit criterea are met or the end of file is reached.
  --              If the end of file is reached, an end_error exception
  --              will be raised.
  --
  -----------------------------------------------------------------------------

  procedure Skip_Whitespace_And_Comments (
        File : in out Ada.Streams.Stream_IO.File_Type ) is

    Char : Character;

    type Processing_State_Type is (Looking_For_Non_Whitespace,
      Looking_For_End_Of_Line);

    Processing_State : Processing_State_Type := Looking_For_Non_Whitespace;


    Current_Index : Ada.Streams.Stream_IO.Positive_Count;

    use type Ada.Streams.Stream_IO.Count;

  begin

    --
    -- Each time through this loop, a character is read from
    -- the input. If we are not Looking_For_End_Of_Line (due to a
    -- comment marker character) then we exit on the first non-whitespace
    -- non comment character we see.
    --
    loop

      Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);

      case Processing_State is

        when Looking_For_Non_Whitespace =>

          if Char = Comment_Character then

            Processing_State := Looking_For_End_Of_Line;

          elsif not Is_Whitespace(Char) then

            exit;

          end if;


        when Looking_For_End_Of_Line    =>

          if Char = ASCII.LF or Char = ASCII.CR then

            Processing_State := Looking_For_Non_Whitespace;

          end if;

      end case;
    end loop;


    --
    -- When we exit the above loop, we have just read the
    -- first non-whitespace character so back-up one.
    --
    Current_Index := Ada.Streams.Stream_IO.Index(File);
    Current_Index := Current_Index - 1;
    Ada.Streams.Stream_IO.Set_Index(
      File => File,
      To   => Current_Index);

  end Skip_Whitespace_And_Comments;



  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Read_ASCII_Integer_From_Stream
  --
  -- PURPOSE    : Reads an integer from the given file starting at the
  --              current location. This integer is expected to be encoded
  --              as a sequence of ASCII digits.
  --
  -- NOTES      : Raises Data_Error if the first non-whitespace data found
  --              can not be part of an integer.
  --
  -----------------------------------------------------------------------------


  generic

    type Integer_Type is  (<>);

    procedure Read_ASCII_Integer_From_Stream (
          File : in out Ada.Streams.Stream_IO.File_Type;
          Item :    out Integer_Type                     );

  procedure Read_ASCII_Integer_From_Stream (
        File : in out Ada.Streams.Stream_IO.File_Type;
        Item :    out Integer_Type                     ) is


    Current_Index          : Ada.Streams.Stream_IO.Positive_Count;
    First_Index_Of_Integer : Ada.Streams.Stream_IO.Positive_Count;
    Last_Index_Of_Integer  : Ada.Streams.Stream_IO.Positive_Count;
    Char                   : Character;

    use type Ada.Streams.Stream_IO.Count;

  begin

    --
    -- Note that I am not super happy with this algorithm at the moment
    -- since we essentially read the data twice. This could make the ASCII
    -- PPM file reads slow. May look at doing this differently some day.
    --

    Current_Index := Ada.Streams.Stream_IO.Index(File);

    --
    -- First skip any whitespace that may be present. Note that
    -- we need to be forgiving with respect to end of lines
    --
    loop

      Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
      exit when not Is_Whitespace(Char);

    end loop;

    --
    -- We skipped all of the whitespace. Make sure we are actually on
    -- data that can be part of an integer.
    --
    if not Ada.Characters.Handling.Is_Digit(Char) and Char /= '-' and Char /=
        '+' then

      --
      -- This is not an integer...
      --
      Ada.Streams.Stream_IO.Set_Index(
        File => File,
        To   => Current_Index);

      raise IO_Exceptions.Data_Error;

    else

      --
      -- So, since we have now stopped after reading the first
      -- character that will be part of the integer expression,
      -- the start in the file of this integer data is just before
      -- the current file position.
      First_Index_Of_Integer := Ada.Streams.Stream_IO.Index(File) - 1;

      --
      -- Each time through this loop we read another character from
      -- the file until we come to a non-digit character.
      loop

        Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
        exit when not Ada.Characters.Handling.Is_Digit(Char) or Ada.
          Streams.Stream_IO.End_Of_FIle(File);

      end loop;

      --
      -- We exited the above loop after reading a character that should
      -- not be part of the integer so it ends just before this
      -- character.
      --
      Last_Index_Of_Integer := Ada.Streams.Stream_IO.Index(File) - 1;


      --
      -- We now know the bounds in the stream of where this integer is.
      -- Declare a perfect length string and read in the characters
      -- from the integer.
      --
      declare
        Integer_As_String : String (1 .. Integer (Last_Index_Of_Integer -
          First_Index_Of_Integer));
      begin

        Ada.Streams.Stream_IO.Set_Index(
          File => File,
          To   => First_Index_Of_Integer);

        String'Read(Ada.Streams.Stream_IO.Stream(File), Integer_As_String);

        Item := Integer_Type'Value(Integer_As_String);

      end;


    end if;

  end Read_ASCII_Integer_From_Stream;


  --
  -- Now, create a few instances of the generic that allows us
  -- to read an ASCII integer from the stream
  --
  procedure Read_ASCII_Column_From_Stream is
  new Read_ASCII_Integer_From_Stream(Column_Index_Type);

   procedure Read_ASCII_Row_From_Stream is
  new Read_ASCII_Integer_From_Stream(Row_Index_Type);


  procedure Read_ASCII_Pixel_Value_From_Stream is
  new Read_ASCII_Integer_From_Stream(Pixel_Value_Type);




  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Read_Single_Whitespace
  --
  -- PURPOSE    : Reads a single character from the given file.
  --
  -- NOTES      : This should probably really check to see that what was
  --              read was actuall whitespace.
  --
  -----------------------------------------------------------------------------

  procedure Read_Single_Whitespace (
        File : in out Ada.Streams.Stream_IO.File_Type ) is

    Char : Character;

  begin
    Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
  end Read_Single_Whitespace;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE  : Open_File_And_Read_Header
  --
  -- PURPOSE    : Opens a PPM file with the given name and reads the
  --              header information from the file.
  --
  -- NOTES      : None
  --
  -----------------------------------------------------------------------------

  procedure Open_File_And_Read_Header (
        Name            : in     String;
        Columns         :    out Column_Index_Type;
        Rows            :    out Row_Index_Type;
        Max_Color_Value :    out Pixel_Value_Type;
        Pixel_Data_Is   :    out Pixel_Value_Storage_Type;
        File            : in out Ada.Streams.Stream_IO.File_Type ) is


    Magic_String : String (1 .. 2);


  begin

    Ada.Streams.Stream_IO.Open(
      File => File,
      Mode => Ada.Streams.Stream_IO.In_File,
      Name => Name);

    String'Read(Ada.Streams.Stream_IO.Stream(File), Magic_String);

    if Magic_String = ASCII_PPM_Magic_String then

      Pixel_Data_Is := ASCII_Data;

    elsif Magic_String = Raw_Bits_PPM_Magic_String then

      Pixel_Data_Is := Raw_Bits;

    else

      Ada.Streams.Stream_IO.Close(File);
      raise Not_A_PPM_File_Error;

    end if;

    Skip_Whitespace_And_Comments(File);

    Read_ASCII_Column_From_Stream(
      File => File,
      Item => Columns);

    Skip_Whitespace_And_Comments(File);

    Read_ASCII_Row_From_Stream(
      File => File,
      Item => Rows);

    Skip_Whitespace_And_Comments(File);

    Read_ASCII_Pixel_Value_From_Stream(
      File => File,
      Item => Max_Color_Value);

    Read_Single_Whitespace(File => File);


  end Open_File_And_Read_Header;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE : Read_PPM
  --
  -- PURPOSE   : Read a portable pixmap file from a filename specified in
  --             Name into the user structure Image.
  --
  -- NOTES     : In addition to the standard I/O exceptions, this procedure
  --             will raise Not_A_PPM_File_Error if it is determined that
  --             the file is not a PPM file.
  --
  -----------------------------------------------------------------------------

  procedure Read_PPM (
        Name  : in     String;
        Image : in out Image_Type ) is

    File            : Ada.Streams.Stream_IO.File_Type;
    Columns         : Column_Index_Type;
    Rows            : Row_Index_Type;
    Pixel_Data_Is   : Pixel_Value_Storage_Type;
    Pixel_Value     : Color_Channels_Type;
    Max_Color_Value : Pixel_Value_Type;
    Temp_Channel    : Unsigned_Byte;

  begin

    Open_File_And_Read_Header
      (
      Name            => Name,
      Columns         => Columns,
      Rows            => Rows,
      Max_Color_Value => Max_Color_Value,
      Pixel_Data_Is   => Pixel_Data_Is,
      File            => File);

    Set_Size(
      Image     => Image,
      Columns   => Columns,
      Rows      => Rows,
      Max_Color => Max_Color_Value);

    if Pixel_Data_Is = ASCII_Data then

      --
      -- Each time through this loop, a row of pixel data is
      -- read (one value for each channel)
      --
      for Row in 1 .. Rows loop

        --
        -- Each time through this loop, a pixel's three color channel
        -- values are read from the input file and the Set_Pixel routine
        -- is called.
        --
        for Column in 1 .. Columns loop

          Read_ASCII_Pixel_Value_From_Stream(
            File => File,
            Item => Pixel_Value.Red);

          Read_ASCII_Pixel_Value_From_Stream(
            File => File,
            Item => Pixel_Value.Green);

          Read_ASCII_Pixel_Value_From_Stream(
            File => File,
            Item => Pixel_Value.Blue);

          Set_Pixel(
            Image  => Image,
            Column => Column,
            Row    => Row,
            Value  => Pixel_Value);

        end loop;
      end loop;


    else
      --
      -- Each time through this loop, a row of pixel data is
      -- read (one value for each channel)
      --
      for Row in 1 .. Rows  loop

        --
        -- Each time through this loop, a pixel's three color channel
        -- values are read from the input file and the Set_Pixel routine
        -- is called.
        --
        for Column in 1 .. Columns loop

          Unsigned_Byte'Read(Ada.Streams.Stream_IO.Stream(File),
            Temp_Channel);
          Pixel_Value.Red := Pixel_Value_Type(Temp_Channel);

          Unsigned_Byte'Read(Ada.Streams.Stream_IO.Stream(File),
            Temp_Channel);
          Pixel_Value.Green := Pixel_Value_Type(Temp_Channel);

          Unsigned_Byte'Read(Ada.Streams.Stream_IO.Stream(File),
            Temp_Channel);
          Pixel_Value.Blue := Pixel_Value_Type(Temp_Channel);

          Set_Pixel(
            Image  => Image,
            Column => Column,
            Row    => Row,
            Value  => Pixel_Value);

        end loop;
      end loop;

    end if;

    Ada.Streams.Stream_IO.Close(File);

  exception

    when E:others =>

       Text_Io.Put_Line(Ada.Exceptions.Exception_Name(E));
       Text_Io.Put_line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));


       if Ada.Streams.Stream_IO.Is_Open(File) then
          Ada.Streams.Stream_IO.Close(File);
       end if;

       --
       -- This may be a little bit of wishful thinking since we could
       -- certainly have a big in the code that caused this but...
       --
       raise Not_A_PPM_File_Error;

  end Read_PPM;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE : Create_PPM_File_And_Header
  --
  -- PURPOSE   : Creates a new PPM file and writes the given header
  --             information to it.
  --
  -- NOTES     : None
  --
  -----------------------------------------------------------------------------

  procedure Create_PPM_File_And_Header (
        Name                  : in     String;
        Max_Pixel_Value       : in     Pixel_Value_Type;
        Columns               : in     Column_Index_Type;
        Rows                  : in     Row_Index_Type;
        Store_Pixel_Values_As : in     Pixel_Value_Storage_Type;
        File                  : in out Ada.Streams.Stream_IO.File_Type ) is

  begin

    if Max_Pixel_Value > Max_Value_For_Raw_Bits_File then

      raise File_Format_Protocol_Violation_Error;

    end if;


    Ada.Streams.Stream_IO.Create(
      File => File,
      Name => Name);

    if Store_Pixel_Values_As = ASCII_Data then

      String'Write(Ada.Streams.Stream_IO.Stream(File),
        ASCII_PPM_Magic_String & End_Of_Line);

    else

      String'Write(Ada.Streams.Stream_IO.Stream(File),
        Raw_Bits_PPM_Magic_String & End_Of_Line);

    end if;

    String'Write(Ada.Streams.Stream_IO.Stream(File),"# Width" &
      End_Of_Line);

    String'Write(Ada.Streams.Stream_IO.Stream(File),Column_Index_Type'
      Image (
        Columns) & End_Of_Line);

    String'Write(Ada.Streams.Stream_IO.Stream(File),  "# Height" &
      End_Of_Line);

    String'Write(Ada.Streams.Stream_IO.Stream(File),Row_Index_Type'
      Image(
        Rows) & End_Of_Line);

    String'Write(Ada.Streams.Stream_IO.Stream(File),
      "# Max color component value" & End_Of_Line);

    --
    -- Note that only a single whitespace character is allowed after the
    -- max pixel value so we have to use character and take
    -- no chances that we stick a string on the end of it.
    --
    String'Write(Ada.Streams.Stream_IO.Stream(File),
                 Pixel_Value_Type'Image (Max_Pixel_Value) & ASCII.LF);

  end Create_PPM_File_And_Header;


  -----------------------------------------------------------------------------
  --
  -- PROCEDURE : Write_PPM
  --
  -- PURPOSE   : Creates a new PPM file in the file specified by Name based
  --             on the given Image data.
  --
  --             Max_Pixel_Value must contain the largest value in any channel
  --             of the data. No excplit checking is done during the write
  --             of the data to verify that the data does not exceed this value
  --             but you will either get an exception (if certain run-time
  --             checks are enabled) or more likely result in an invalid
  --             PPM file.
  --
  --             Store_Pixel_Value_As defines the format for the pixel
  --             data. If Max_Pixel_Value > Max_Value_For_Raw_Bits_File,
  --             then Store_Pixel_Value_As must be ASCII_Data.
  --
  -- NOTES     : In addition to the standard IO type exceptions, this procedure
  --             will raise File_Format_Protocol_Violation_Error if
  --             Max_Pixel_Value > Max_Value_For_Raw_Bits_File and
  --             Store_Pixel_Value_As is not ASCII_Data.
  --
  -----------------------------------------------------------------------------


  procedure Write_PPM (
        Name                  : in     String;
        Max_Pixel_Value       : in     Pixel_Value_Type;
        Columns               : in     Column_Index_Type;
        Rows                  : in     Row_Index_Type;
        Store_Pixel_Values_As : in     Pixel_Value_Storage_Type;
        Image                 : in     Image_Type                ) is

    File         : Ada.Streams.Stream_IO.File_Type;
    Channels     : Color_Channels_Type;
    Temp_Channel : Unsigned_Byte;


  begin

    Create_PPM_File_And_Header(
      Name                  => Name,
      Max_Pixel_Value       => Max_Pixel_Value,
      Columns               => Columns,
      Rows                  => Rows,
      Store_Pixel_Values_As => Store_Pixel_Values_As,
      File                  => File);


    if Store_Pixel_Values_As = ASCII_Data then

      --
      -- Each time through this loop, a row of pixel data is
      -- writen (one value for each channel) and a new line
      -- indication is added at the end of the row.
      --
      for Row in 1 .. Rows  loop
        --
        -- Each time through this loop, a pixel's three color channel
        -- values are written to the output file.
        --
        for Column in 1 .. Columns loop

          Channels := Get_Pixel(
            Image  => Image,
            Column => Column,
            Row    => Row);

          String'Write(Ada.Streams.Stream_IO.Stream(File),
            Pixel_Value_Type'Image (Channels.Red) & ' ');

          String'Write(Ada.Streams.Stream_IO.Stream(File),
            Pixel_Value_Type'Image (Channels.Green) & ' ');

          String'Write(Ada.Streams.Stream_IO.Stream(File),
            Pixel_Value_Type'Image (Channels.Blue) & "   ");

          String'Write(Ada.Streams.Stream_IO.Stream(File), End_Of_Line);

        end loop;

      end loop;


    else -- Data is Raw_Bits

      --
      -- Each time through this loop, a row of pixel data is
      -- writen (one value for each channel)
      --
      for Row in 1 .. Rows  loop

        --
        -- Each time through this loop, a pixel's three color channel
        -- values are written to the output file.
        --
        for Column in 1 .. Columns  loop

          Channels := Get_Pixel(
            Image  => Image,
            Column => Column,
            Row    => Row);

          Temp_Channel := Unsigned_Byte(Channels.Red);
          Unsigned_Byte'Write(Ada.Streams.Stream_IO.Stream(File),
            Temp_Channel);

          Temp_Channel := Unsigned_Byte(Channels.Green);
          Unsigned_Byte'Write(Ada.Streams.Stream_IO.Stream(File),
            Temp_Channel);

          Temp_Channel := Unsigned_Byte(Channels.Blue);
          Unsigned_Byte'Write(Ada.Streams.Stream_IO.Stream(File),
            Temp_Channel);

        end loop;
      end loop;

    end if;

    Ada.Streams.Stream_IO.Close(FIle);

  exception

    when E:others =>

      if Ada.Streams.Stream_IO.Is_Open(File) then
        Ada.Streams.Stream_IO.Close(FIle);
      end if;

      raise;

  end Write_Ppm;

end Open_Image.PPM_IO;

