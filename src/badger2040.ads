--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--

--  Pimoroni Badger 2040 (PIM607)
--
--  https://shop.pimoroni.com/products/badger-2040
--  https://cdn.shopify.com/s/files/1/0174/1800/files/badger_2040_schematic.pdf
with RP2040_SVD.UART;
with RP2040_SVD.SPI;
with RP2040_SVD.I2C;

with RP.I2C_Master;
with RP.Timer;
with RP.UART;
with RP.GPIO;
with RP.PWM;
with RP.SPI;
with RP;

with UC8151;
with HAL;

package Badger2040 is
   UART_TX     : aliased RP.GPIO.GPIO_Point := (Pin => 0);
   UART_RX     : aliased RP.GPIO.GPIO_Point := (Pin => 1);
   UART        : aliased RP.UART.UART_Port (0, RP2040_SVD.UART.UART0_Periph'Access);

   I2C_INT     : aliased RP.GPIO.GPIO_Point := (Pin => 3);
   I2C_SDA     : aliased RP.GPIO.GPIO_Point := (Pin => 4);
   I2C_SCL     : aliased RP.GPIO.GPIO_Point := (Pin => 5);
   I2C         : aliased RP.I2C_Master.I2C_Master_Port (0, RP2040_SVD.I2C.I2C0_Periph'Access);

   ENABLE_3V3  : aliased RP.GPIO.GPIO_Point := (Pin => 10);

   SW_DOWN     : aliased RP.GPIO.GPIO_Point := (Pin => 11);
   SWA         : aliased RP.GPIO.GPIO_Point := (Pin => 12);
   SWB         : aliased RP.GPIO.GPIO_Point := (Pin => 13);
   SWC         : aliased RP.GPIO.GPIO_Point := (Pin => 14);
   SW_UP       : aliased RP.GPIO.GPIO_Point := (Pin => 15);

   MISO        : aliased RP.GPIO.GPIO_Point := (Pin => 16);
   INKY_CS     : aliased RP.GPIO.GPIO_Point := (Pin => 17);
   SCLK        : aliased RP.GPIO.GPIO_Point := (Pin => 18);
   MOSI        : aliased RP.GPIO.GPIO_Point := (Pin => 19);
   INKY_DC     : aliased RP.GPIO.GPIO_Point := (Pin => 20);
   INKY_RES    : aliased RP.GPIO.GPIO_Point := (Pin => 21);
   USER_SW     : aliased RP.GPIO.GPIO_Point := (Pin => 23);
   VBUS_DETECT : aliased RP.GPIO.GPIO_Point := (Pin => 24);
   INKY_SPI    : aliased RP.SPI.SPI_Port (0, RP2040_SVD.SPI.SPI0_Periph'Access);

   USER_LED     : aliased RP.GPIO.GPIO_Point := (Pin => 25);
   USER_LED_PWM : constant RP.PWM.PWM_Point := RP.PWM.To_PWM (USER_LED);

   INKY_BUSY   : aliased RP.GPIO.GPIO_Point := (Pin => 26);
   VREF_POWER  : aliased RP.GPIO.GPIO_Point := (Pin => 27);
   VREF_1V24   : aliased RP.GPIO.GPIO_Point := (Pin => 28);
   VBAT_SENSE  : aliased RP.GPIO.GPIO_Point := (Pin => 29);

   XOSC_Frequency : constant RP.Hertz := 12_000_000;

   type Button is (A, B, C, Up, Down, User);
   Button_State : array (Button) of Boolean;

   Delays : aliased RP.Timer.Delays;

   INKY : UC8151.Device
      (Port    => INKY_SPI'Access,
       Delays  => Delays'Access,
       CS      => INKY_CS'Access,
       DC      => INKY_DC'Access,
       BUSY    => INKY_BUSY'Access,
       RST     => INKY_RES'Access,
       Width   => 128,
       Height  => 296);

   procedure Initialize;
   procedure Update_Button_State;
   procedure Set_LED (Brightness : HAL.UInt8);

private

   function powf
      (X, Y : Float)
      return Float;

end Badger2040;
