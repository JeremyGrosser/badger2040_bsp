--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;
with HAL.Time;
with RP.GPIO;
with HAL.SPI;

package UC8151 is

   type Device
      (Port              : not null HAL.SPI.Any_SPI_Port;
       Delays            : not null HAL.Time.Any_Delays;
       CS, DC, BUSY, RST : not null access RP.GPIO.GPIO_Point;
       Width             : Natural;
       Height            : Natural)
   is tagged record
      Errors : Natural := 0;
   end record;

   procedure Initialize
      (This : in out Device);

   procedure Draw
      (This  : in out Device;
       Image : UInt8_Array)
   with Pre => Image'Size = This.Width * This.Height;

   procedure Transmit
      (This : in out Device;
       Data : UInt8_Array);
   --  Increments This.Errors if Transmit fails.

   function Is_Busy
      (This : Device)
      return Boolean;

   procedure Reset
      (This : in out Device);

end UC8151;
