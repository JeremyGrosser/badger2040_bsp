--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with RP.ROM.Floating_Point;
with RP.Clock;

package body Badger2040 is
   procedure Initialize is
      use RP.GPIO;
   begin
      RP.Clock.Initialize (XOSC_Frequency);
      Delays.Enable;

      ENABLE_3V3.Configure (Output);
      ENABLE_3V3.Set;
      VBUS_DETECT.Configure (Input);

      SWA.Configure (Input, Pull_Down);
      SWB.Configure (Input, Pull_Down);
      SWC.Configure (Input, Pull_Down);
      SW_UP.Configure (Input, Pull_Down);
      SW_DOWN.Configure (Input, Pull_Down);
      USER_SW.Configure (Input, Pull_Down);
      Update_Button_State;

      USER_LED.Configure (Output, Floating, RP.GPIO.PWM);
      RP.PWM.Initialize;
      RP.PWM.Set_Frequency (USER_LED_PWM.Slice, 10_000_000);
      RP.PWM.Set_Interval (USER_LED_PWM.Slice, RP.PWM.Period'Last);
      RP.PWM.Set_Duty_Cycle (USER_LED_PWM.Slice, USER_LED_PWM.Channel, 0);
      RP.PWM.Enable (USER_LED_PWM.Slice);

      INKY_CS.Configure (Output, Pull_Up);
      INKY_DC.Configure (Output, Pull_Up);
      INKY_RES.Configure (Output, Pull_Up);
      INKY_BUSY.Configure (Input, Floating);
      MOSI.Configure (Output, Floating, RP.GPIO.SPI);
      MISO.Configure (Output, Floating, RP.GPIO.SPI);
      SCLK.Configure (Output, Floating, RP.GPIO.SPI);
      INKY_SPI.Configure;
      INKY_SPI.Set_Speed (12_000_000);
      INKY.Initialize;
   end Initialize;

   procedure Update_Button_State is
   begin
      Button_State (A) := SWA.Get;
      Button_State (B) := SWB.Get;
      Button_State (C) := SWC.Get;
      Button_State (Up) := SW_UP.Get;
      Button_State (Down) := SW_DOWN.Get;
      Button_State (User) := USER_SW.Get;
   end Update_Button_State;

   function powf
      (X, Y : Float)
      return Float
   is
      --  Based on sdcc/device/lib/powf.c
      --  Not very accurate, but good enough for gamma curves.
      use RP.ROM.Floating_Point;
   begin
      if Y = 0.0 then
         return 1.0;
      elsif Y = 1.0 then
         return X;
      elsif X <= 0.0 then
         return 0.0;
      else
         return fexp (fln (X) * Y);
      end if;
   end powf;

   procedure Set_LED
      (Brightness : HAL.UInt8)
   is
      use RP.PWM;
      Gamma : constant Float := 2.8;
      V     : Float;
   begin
      V := Float (Brightness) / 256.0;
      V := powf (V, Gamma);
      V := V * Float (Period'Last);
      V := V + 0.5;
      RP.PWM.Set_Duty_Cycle (USER_LED_PWM.Slice, USER_LED_PWM.Channel, Period (V));
   end Set_LED;
end Badger2040;
