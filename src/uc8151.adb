--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
pragma Warnings (Off, "literal ""*"" is not referenced");
with Ada.Unchecked_Conversion;

package body UC8151 is

   generic
      Cmd : UInt8;
      type Register_Type is private;
   procedure Command_With_Value
      (This  : in out Device;
       Value : Register_Type);

   generic
      Cmd : UInt8;
   procedure Command
      (This : in out Device);

   procedure Command_With_Value
      (This  : in out Device;
       Value : Register_Type)
   is
      subtype Register_Data is UInt8_Array (1 .. Value'Size / 8);
      function To_Register_Data is new Ada.Unchecked_Conversion
         (Source => Register_Type, Target => Register_Data);
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Transmit (UInt8_Array'(1 => Cmd));
      This.DC.Set;
      This.Transmit (To_Register_Data (Value));
      This.CS.Set;
   end Command_With_Value;

   procedure Command
      (This : in out Device)
   is
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Transmit (UInt8_Array'(1 => Cmd));
      This.CS.Set;
   end Command;

   type PSR_RES_Field is
      (RES_96x230,
       RES_96x252,
       RES_128x296,
       RES_160x296)
      with Size => 2;

   type PSR_LUT_Field is
      (LUT_OTP, LUT_REG)
      with Size => 1;

   type PSR_FORMAT_Field is
      (Black_White_Red, Black_White)
      with Size => 1;

   type PSR_SCAN_Field is
      (Scan_Down, Scan_Up)
      with Size => 1;

   type PSR_SHIFT_Field is
      (Shift_Left, Shift_Right)
      with Size => 1;

   type PSR_RESET_Field is
      (Soft_Reset, No_Reset)
      with Size => 1;

   type PSR_Register is record
      RES     : PSR_RES_Field    := RES_96x230;
      LUT     : PSR_LUT_Field    := LUT_OTP;
      FORMAT  : PSR_FORMAT_Field := Black_White_Red;
      SCAN    : PSR_SCAN_Field   := Scan_Down;
      SHIFT   : PSR_SHIFT_Field  := Shift_Right;
      BOOSTER : Boolean          := True;
      RESET   : PSR_RESET_Field  := No_Reset;
   end record
      with Size => 8;
   for PSR_Register use record
      RES      at 0 range 6 .. 7;
      LUT      at 0 range 5 .. 5;
      FORMAT   at 0 range 4 .. 4;
      SCAN     at 0 range 3 .. 3;
      SHIFT    at 0 range 2 .. 2;
      BOOSTER  at 0 range 1 .. 1;
      RESET    at 0 range 0 .. 0;
   end record;
   procedure PSR is new Command_With_Value (16#00#, PSR_Register);

   type PWR_VDS_Field is (VDS_External, VDS_Internal)
      with Size => 1;
   type PWR_VDG_Field is (VDG_External, VDG_Internal)
      with Size => 1;
   type PWR_VCOM_Field is (VCOM_VD, VCOM_VG)
      with Size => 1;
   type PWR_VGHL_Field is (VGHL_16V, VGHL_15V, VGHL_14V, VGHL_13V)
      with Size => 2;

   type PWR_Register is record
      VDS  : PWR_VDS_Field := VDS_Internal;
      VDG  : PWR_VDG_Field := VDG_Internal;
      VCOM : PWR_VCOM_Field := VCOM_VD;
      VGHL : PWR_VGHL_Field := VGHL_16V;
      VDH  : UInt6 := 2#100110#;
      VDL  : UInt6 := 2#100110#;
      VDHR : UInt6 := 2#000011#;
   end record
      with Size => 6 * 8;
   for PWR_Register use record
      VDS   at 0 range 1 .. 1;
      VDG   at 0 range 0 .. 0;
      VCOM  at 1 range 2 .. 2;
      VGHL  at 2 range 0 .. 1;
      VDH   at 3 range 0 .. 5;
      VDL   at 4 range 0 .. 5;
      VDHR  at 5 range 0 .. 5;
   end record;
   procedure PWR is new Command_With_Value (16#01#, PWR_Register);

   type BTST_START_Field is (Start_10ms, Start_20ms, Start_30ms, Start_40ms)
      with Size => 2;
   type BTST_OFF_Field is
      (Off_0_27us, Off_0_34us, Off_0_40us, Off_0_54us, Off_0_80us, Off_1_54us,
       Off_3_34us, Off_6_58us)
      with Size => 3;

   type BTST_Phase_Register is record
      START    : BTST_START_Field := Start_10ms;
      STRENGTH : UInt3 := 2;
      OFF      : BTST_OFF_Field := Off_6_58us;
   end record
      with Size => 8;
   for BTST_Phase_Register use record
      START    at 0 range 6 .. 7;
      STRENGTH at 0 range 3 .. 5;
      OFF      at 0 range 0 .. 2;
   end record;

   type BTST_Phase is (A, B, C);
   type BTST_Register is array (BTST_Phase) of BTST_Phase_Register
      with Component_Size => 8, Size => 24;
   procedure BTST is new Command_With_Value (16#06#, BTST_Register);

   type PFS_Register is record
      T_VDS_OFF : UInt2 := 0;
   end record
      with Size => 8;
   for PFS_Register use record
      T_VDS_OFF at 0 range 4 .. 5;
   end record;
   procedure PFS is new Command_With_Value (16#03#, PFS_Register);

   type TSE_Offset_Field is range -8 .. 7
      with Size => 4;

   type TSE_Register is record
      External : Boolean := False;
      Offset   : TSE_Offset_Field := 0;
   end record
      with Size => 8;
   for TSE_Register use record
      External at 0 range 7 .. 7;
      Offset   at 0 range 0 .. 3;
   end record;
   procedure TSE is new Command_With_Value (16#41#, TSE_Register);

   type TCON_Register is record
      S2G, G2S : UInt4;
   end record
      with Size => 8;
   for TCON_Register use record
      S2G at 0 range 4 .. 7;
      G2S at 0 range 0 .. 3;
   end record;
   procedure TCON is new Command_With_Value (16#60#, TCON_Register);

   type CDI_Register is record
      VBD : UInt2;
      DDX : UInt2;
      CDI : UInt4;
   end record
      with Size => 8;
   for CDI_Register use record
      VBD at 0 range 6 .. 7;
      DDX at 0 range 4 .. 5;
      CDI at 0 range 0 .. 3;
   end record;
   procedure CDI is new Command_With_Value (16#50#, CDI_Register);

   type PLL_Register is record
      M, N : UInt3;
   end record
      with Size => 8;
   for PLL_Register use record
      M at 0 range 3 .. 5;
      N at 0 range 0 .. 2;
   end record;
   procedure PLL is new Command_With_Value (16#30#, PLL_Register);

   procedure PON is new Command (16#04#);
   procedure POF is new Command (16#02#);

   procedure PTOUT is new Command (16#92#);
   procedure DSP is new Command (16#11#);
   procedure DRF is new Command (16#12#);

   DTM2 : constant UInt8 := 16#13#;

   function Is_Busy
      (This : Device)
      return Boolean
   is (not This.BUSY.Get);

   procedure Wait
      (This : Device)
   is
   begin
      while This.Is_Busy loop
         null;
      end loop;
   end Wait;

   procedure Reset
      (This : in out Device)
   is
   begin
      This.RST.Clear;
      This.Delays.Delay_Milliseconds (100);
      This.RST.Set;
      This.Delays.Delay_Milliseconds (100);
      This.Wait;
   end Reset;

   procedure Transmit
      (This : in out Device;
       Data : UInt8_Array)
   is
      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.Port.Transmit (SPI_Data_8b (Data), Status);
      if Status /= Ok then
         This.Errors := This.Errors + 1;
      end if;
   end Transmit;

   procedure Initialize
      (This : in out Device)
   is
   begin
      This.CS.Set;
      This.Reset;

      PSR (This, (RES      => RES_128x296,
                  LUT      => LUT_OTP,
                  FORMAT   => Black_White,
                  SCAN     => Scan_Down,
                  SHIFT    => Shift_Right,
                  BOOSTER  => True,
                  RESET    => No_Reset));

      PWR (This, (VDS      => VDS_Internal,
                  VDG      => VDG_Internal,
                  VCOM     => VCOM_VD,
                  VGHL     => VGHL_16V,
                  VDH      => 2#101011#,
                  VDL      => 2#101011#,
                  VDHR     => 2#101011#));
      PON (This);
      Wait (This);

      BTST (This, ((A .. C => (START      => Start_10ms,
                               STRENGTH   => 2,
                               OFF        => Off_6_58us))));
      PFS (This, (T_VDS_OFF => 0));
      TSE (This, (External => False,
                  Offset   => 0));
      TCON (This, (S2G => 2, G2S => 2));
      CDI (This, (VBD => 2#01#,
                  DDX => 2#01#,
                  CDI => 2#1100#));
      PLL (This, (M => 7, N => 2));
      POF (This);
      This.Wait;
   end Initialize;

   procedure Pixel_Data
      (This : in out Device;
       Cmd  : UInt8;
       Data : UInt8_Array)
   is
      function Bit_Reverse
         (V : UInt8)
         return UInt8
      is
         X : UInt8 := V;
      begin
         --  Hacker's Delight, Warren, 2nd Ed (p. 129)
         X := Shift_Left (X and 16#55#, 1) or Shift_Right (X and 16#AA#, 1);
         X := Shift_Left (X and 16#33#, 2) or Shift_Right (X and 16#CC#, 2);
         X := Shift_Left (X and 16#0F#, 4) or Shift_Right (X and 16#F0#, 4);
         return X;
      end Bit_Reverse;

      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Cmd), Status);
      if Status /= Ok then
         This.Errors := This.Errors + 1;
      end if;

      This.DC.Set;

      for D of Data loop
         This.Port.Transmit (SPI_Data_8b'(1 => Bit_Reverse (D)), Status);
         if Status /= Ok then
            This.Errors := This.Errors + 1;
         end if;
      end loop;
      This.CS.Set;
      This.Wait;
   end Pixel_Data;

   procedure Draw
      (This  : in out Device;
       Image : UInt8_Array)
   is
   begin
      This.Wait;
      PON (This);
      PTOUT (This);
      This.Pixel_Data (DTM2, Image);
      DSP (This);
      DRF (This);
      This.Wait;
      POF (This);
   end Draw;

end UC8151;
