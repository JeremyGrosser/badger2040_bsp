with Ada.Unchecked_Conversion;
with Badger2040; use Badger2040;
with HAL; use HAL;

procedure Demo is
   subtype Column is Integer range 1 .. INKY.Width;
   subtype Row    is Integer range 1 .. INKY.Height;

   type Color is (White, Black);
   type Pixels is array (Row, Column) of Color
      with Component_Size => 1;
   Image : Pixels;

   subtype Pixel_Data is UInt8_Array (1 .. Pixels'Size / 8);
   function To_Pixel_Data is new Ada.Unchecked_Conversion
      (Source => Pixels, Target => Pixel_Data);

   procedure Draw_Cross
      (X : Column;
       Y : Row;
       C : Color;
       Width : Positive := 5)
   is
   begin
      for DX in (-1) * Width .. Width loop
         if X + DX in Column'Range then
            Image (Y, X + DX) := C;
         end if;
      end loop;

      for DY in (-1) * Width .. Width loop
         if Y + DY in Row'Range then
            Image (Y + DY, X) := C;
         end if;
      end loop;
   end Draw_Cross;

   X_Step_Size : constant Column := Column'Last / 4;
   Y_Step_Size : constant Row := Row'Last / 8;
   X : Column;
   Y : Row;
begin
   Badger2040.Initialize;

   loop
      Image := (others => (others => White));
      INKY.Draw (To_Pixel_Data (Image));

      X := Column'First;
      while X + X_Step_Size <= Column'Last loop
         X := X + X_Step_Size;
         Y := Row'First;
         while Y + Y_Step_Size <= Row'Last loop
            Y := Y + Y_Step_Size;
            Draw_Cross (X, Y, Black, 2);
            INKY.Draw (To_Pixel_Data (Image));
         end loop;
      end loop;
   end loop;

   --  loop
   --     Set_LED (128);
   --     Delays.Delay_Milliseconds (100);
   --     Set_LED (0);
   --     Delays.Delay_Milliseconds (100);
   --  end loop;
end Demo;
