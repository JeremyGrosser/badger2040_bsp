--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;
with Badger2040;
with Tiny_Text;

procedure Badger_Demo is
   use Badger2040;
   Text : Tiny_Text.Text_Buffer;
begin
   Badger2040.Initialize;

   Text.Initialize (INKY.Hidden_Buffer (1), INKY.Width, INKY.Height);
   Text.Clear;
   Text.Put ("Hello, Ada!");

   loop
      for I in UInt8'Range loop
         Badger2040.Set_LED (I);
         Delays.Delay_Milliseconds (2);
      end loop;

      for I in reverse UInt8'Range loop
         Badger2040.Set_LED (I);
         Delays.Delay_Milliseconds (2);
      end loop;
   end loop;
end Badger_Demo;
