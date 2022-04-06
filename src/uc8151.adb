package body UC8151 is

   overriding
   function Hidden_Buffer
      (This  : in out Device;
       Layer : Positive)
       return not null HAL.Bitmap.Any_Bitmap_Buffer
   is (This.Buffer'Unchecked_Access);

   function Is_Busy
      (This : Device)
      return Boolean
   is (not This.BUSY.Get);

   procedure Busy_Wait
      (This : in out Device)
   is
   begin
      while This.Is_Busy loop
         null;
      end loop;
   end Busy_Wait;

   procedure Reset
      (This : in out Device)
   is
   begin
      This.RST.Clear;
      This.Delays.Delay_Milliseconds (10);
      This.RST.Set;
      This.Delays.Delay_Milliseconds (10);
      This.Busy_Wait;
   end Reset;

   procedure Default_LUTs
      (This : in out Device)
   is
   begin
      This.Command (LUT_VCOM,
         (16#00#, 16#64#, 16#64#, 16#37#, 16#00#, 16#01#, 16#00#, 16#8c#,
          16#8c#, 16#00#, 16#00#, 16#04#, 16#00#, 16#64#, 16#64#, 16#37#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#));
      This.Command (LUT_WW,
         (16#54#, 16#64#, 16#64#, 16#37#, 16#00#, 16#01#, 16#60#, 16#8c#,
          16#8c#, 16#00#, 16#00#, 16#04#, 16#a8#, 16#64#, 16#64#, 16#37#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BW,
         (16#54#, 16#64#, 16#64#, 16#37#, 16#00#, 16#01#, 16#60#, 16#8c#,
          16#8c#, 16#00#, 16#00#, 16#04#, 16#a8#, 16#64#, 16#64#, 16#37#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_WB,
         (16#a8#, 16#64#, 16#64#, 16#37#, 16#00#, 16#01#, 16#60#, 16#8c#,
          16#8c#, 16#00#, 16#00#, 16#04#, 16#54#, 16#64#, 16#64#, 16#37#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BB,
         (16#a8#, 16#64#, 16#64#, 16#37#, 16#00#, 16#01#, 16#60#, 16#8c#,
          16#8c#, 16#00#, 16#00#, 16#04#, 16#54#, 16#64#, 16#64#, 16#37#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Busy_Wait;
   end Default_LUTs;

   procedure Medium_LUTs
      (This : in out Device)
   is
   begin
      This.Command (LUT_VCOM,
         (16#00#, 16#16#, 16#16#, 16#0d#, 16#00#, 16#01#, 16#00#, 16#23#,
          16#23#, 16#00#, 16#00#, 16#02#, 16#00#, 16#16#, 16#16#, 16#0d#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#));
      This.Command (LUT_WW,
         (16#54#, 16#16#, 16#16#, 16#0d#, 16#00#, 16#01#, 16#60#, 16#23#,
          16#23#, 16#00#, 16#00#, 16#02#, 16#a8#, 16#16#, 16#16#, 16#0d#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BW,
         (16#54#, 16#16#, 16#16#, 16#0d#, 16#00#, 16#01#,
          16#60#, 16#23#, 16#23#, 16#00#, 16#00#, 16#02#,
          16#a8#, 16#16#, 16#16#, 16#0d#, 16#00#, 16#01#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
      This.Command (LUT_WB,
         (16#a8#, 16#16#, 16#16#, 16#0d#, 16#00#, 16#01#, 16#60#, 16#23#,
          16#23#, 16#00#, 16#00#, 16#02#, 16#54#, 16#16#, 16#16#, 16#0d#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BB,
         (16#a8#, 16#16#, 16#16#, 16#0d#, 16#00#, 16#01#, 16#60#, 16#23#,
          16#23#, 16#00#, 16#00#, 16#02#, 16#54#, 16#16#, 16#16#, 16#0d#,
          16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Busy_Wait;
   end Medium_LUTs;

   procedure Fast_LUTs
      (This : in out Device)
   is
   begin
      This.Command (LUT_VCOM,
         (16#00#, 16#04#, 16#04#, 16#07#, 16#00#, 16#01#, 16#00#, 16#0c#,
          16#0c#, 16#00#, 16#00#, 16#02#, 16#00#, 16#04#, 16#04#, 16#07#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#));
      This.Command (LUT_WW,
         (16#54#, 16#04#, 16#04#, 16#07#, 16#00#, 16#01#, 16#60#, 16#0c#,
          16#0c#, 16#00#, 16#00#, 16#02#, 16#a8#, 16#04#, 16#04#, 16#07#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BW,
         (16#54#, 16#04#, 16#04#, 16#07#, 16#00#, 16#01#, 16#60#, 16#0c#,
          16#0c#, 16#00#, 16#00#, 16#02#, 16#a8#, 16#04#, 16#04#, 16#07#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_WB,
         (16#a8#, 16#04#, 16#04#, 16#07#, 16#00#, 16#01#, 16#60#, 16#0c#,
          16#0c#, 16#00#, 16#00#, 16#02#, 16#54#, 16#04#, 16#04#, 16#07#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BB,
         (16#a8#, 16#04#, 16#04#, 16#07#, 16#00#, 16#01#, 16#60#, 16#0c#,
          16#0c#, 16#00#, 16#00#, 16#02#, 16#54#, 16#04#, 16#04#, 16#07#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (PLL, To_Bytes (PLL_Register'(M => 7, N => 1))); --  200 Hz
      This.Busy_Wait;
   end Fast_LUTs;

   procedure Turbo_LUTs
      (This : in out Device)
   is
   begin
      This.Command (LUT_VCOM,
         (16#00#, 16#01#, 16#01#, 16#02#, 16#00#, 16#01#, 16#00#, 16#02#,
          16#02#, 16#00#, 16#00#, 16#02#, 16#00#, 16#02#, 16#02#, 16#03#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#));
      This.Command (LUT_WW,
         (16#54#, 16#01#, 16#01#, 16#02#, 16#00#, 16#01#, 16#60#, 16#02#,
          16#02#, 16#00#, 16#00#, 16#02#, 16#a8#, 16#02#, 16#02#, 16#03#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BW,
         (16#54#, 16#01#, 16#01#, 16#02#, 16#00#, 16#01#, 16#60#, 16#02#,
          16#02#, 16#00#, 16#00#, 16#02#, 16#a8#, 16#02#, 16#02#, 16#03#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_WB,
         (16#a8#, 16#01#, 16#01#, 16#02#, 16#00#, 16#01#, 16#60#, 16#02#,
          16#02#, 16#00#, 16#00#, 16#02#, 16#54#, 16#02#, 16#02#, 16#03#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (LUT_BB,
         (16#a8#, 16#01#, 16#01#, 16#02#, 16#00#, 16#01#, 16#60#, 16#02#,
          16#02#, 16#00#, 16#00#, 16#02#, 16#54#, 16#02#, 16#02#, 16#03#,
          16#00#, 16#02#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#));
      This.Command (PLL, To_Bytes (PLL_Register'(M => 7, N => 1))); -- 200 Hz
      This.Busy_Wait;
   end Turbo_LUTs;

   procedure Initialize
      (This : in out Device)
   is
   begin
      This.CS.Set;
      This.RST.Set;
      This.Setup;

      This.Buffer.Actual_Width := Device_Width;
      This.Buffer.Actual_Height := Device_Height;
      This.Buffer.Actual_Color_Mode := HAL.Bitmap.M_1;
      This.Buffer.Currently_Swapped := True;
      This.Buffer.Addr := This.Frame_Buffer'Address;
      This.Initialize_Layer
         (Layer  => 1,
          Mode   => HAL.Bitmap.M_1,
          X      => 0,
          Y      => 0,
          Width  => Device_Width,
          Height => Device_Height);
      This.Init := True;
   end Initialize;

   procedure Setup
      (This  : in out Device;
       Speed : Update_Speed := Default)
   is
   begin
      This.Reset;

      This.Command (PSR, To_Bytes (PSR_Register'
         (RES     => RES_128x296,
          LUT     => (if Speed = Default then LUT_OTP else LUT_REG),
          FORMAT  => Black_White,
          SCAN    => Scan_Up,
          SHIFT   => Shift_Right,
          BOOSTER => True,
          RESET   => No_Reset)));

      This.Set_Update_Speed (Speed);

      This.Command (PWR, To_Bytes (PWR_Register'
         (VDS  => VDS_Internal,
          VDG  => VDG_Internal,
          VCOM => VCOM_VD,
          VGHL => VGHL_16V,
          VDH  => 2#101011#,
          VDL  => 2#101011#,
          VDHR => 2#101011#)));

      This.Power_On (Wait => True);

      This.Command (BTST, To_Bytes (BTST_Register'
         (A .. C => (START => Start_10ms, STRENGTH => 2, OFF => Off_6_58us))));

      This.Command (PFS, To_Bytes (PFS_Register'
         (T_VDS_OFF => 0)));

      This.Command (TSE, To_Bytes (TSE_Register'
         (External => False,
          Offset   => 0)));

      This.Command (TCON, To_Bytes (TCON_Register'(16#22#)));

      This.Set_Invert (True);

      This.Command (PLL, To_Bytes (PLL_Register'
         (M => 7, N => 2))); --  100 Hz

      This.Power_Off (Wait => True);
   end Setup;

   procedure Command
      (This : in out Device;
       Reg  : Register;
       Data : UInt8_Array)
   is
      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Register'Enum_Rep (Reg)), Status);
      if Status = Ok then
         This.DC.Set;
         This.Port.Transmit (SPI_Data_8b (Data), Status);
      end if;
      This.CS.Set;
   end Command;

   procedure Command
      (This : in out Device;
       Reg  : Register)
   is
      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Register'Enum_Rep (Reg)), Status);
      This.CS.Set;
   end Command;

   procedure Power_On
      (This : in out Device;
       Wait : Boolean)
   is
   begin
      This.Command (PON);
      if Wait then
         This.Busy_Wait;
      end if;
   end Power_On;

   procedure Power_Off
      (This : in out Device;
       Wait : Boolean)
   is
   begin
      This.Command (POF);
      if Wait then
         This.Busy_Wait;
      end if;
   end Power_Off;

   procedure Write
      (This : in out Device;
       Data : UInt8_Array)
   is
      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Set;
      This.Port.Transmit (SPI_Data_8b (Data), Status);
      This.CS.Set;
   end Write;

   procedure Set_Invert
      (This : in out Device;
       Enabled : Boolean)
   is
   begin
      if Enabled then
         This.Command (CDI, To_Bytes (CDI_Register'
            (VBD => 2#01#,
             DDX => 2#01#,
             CDI => 2#1100#)));
      else
         This.Command (CDI, To_Bytes (CDI_Register'
            (VBD => 2#01#,
             DDX => 2#00#,
             CDI => 2#1100#)));
      end if;
   end Set_Invert;

   procedure Set_Update_Speed
      (This  : in out Device;
       Speed : Update_Speed)
   is
   begin
      This.Speed := Speed;
      case Speed is
         --  Default LUTs are burned into the display's OTP memory, so we don't
         --  need to send them.
         when Default => null;
         when Medium  => This.Medium_LUTs;
         when Fast    => This.Fast_LUTs;
         when Turbo   => This.Turbo_LUTs;
      end case;
   end Set_Update_Speed;

   function Update_Time
      (This : Device)
      return Natural
   is
   begin
      case This.Speed is
         when Default => return 4500;
         when Medium  => return 2000;
         when Fast    => return 800;
         when Turbo   => return 250;
      end case;
   end Update_Time;

   procedure Update
      (This     : in out Device;
       Blocking : Boolean := True)
   is
      subtype Frame_Bytes is UInt8_Array (1 .. (Device_Width * Device_Height) / 8);
      function To_Bytes is new Ada.Unchecked_Conversion
         (Source => Pixels,
          Target => Frame_Bytes);
   begin
      This.Power_On (Wait => Blocking);
      This.Command (PTOU); --  Disable partial mode
      This.Command (DTM2, To_Bytes (This.Frame_Buffer));
      This.Command (DSP); --  Data stop
      This.Command (DRF); --  Start display refresh
      This.Power_Off (Wait => Blocking);
   end Update;

   overriding
   function Max_Layers
      (This : Device)
      return Positive
   is (1);

   overriding
   function Supported
      (This : Device;
       Mode : HAL.Framebuffer.FB_Color_Mode)
       return Boolean
   is
      use HAL.Bitmap;
   begin
      return Mode = M_1;
   end Supported;

   overriding
   procedure Set_Orientation
      (This        : in out Device;
       Orientation : HAL.Framebuffer.Display_Orientation)
   is null;

   overriding
   procedure Set_Mode
      (This : in out Device;
       Mode : HAL.Framebuffer.Wait_Mode)
   is null;

   overriding
   function Initialized
      (This : Device)
      return Boolean
   is (This.Init);

   overriding
   function Width
      (This : Device)
      return Positive
   is (Device_Width);

   overriding
   function Height
      (This : Device)
      return Positive
   is (Device_Height);

   overriding
   function Swapped
      (This : Device)
      return Boolean
   is (False);

   overriding
   procedure Set_Background
      (This    : Device;
       R, G, B : UInt8)
   is null;

   overriding
   procedure Initialize_Layer
      (This   : in out Device;
       Layer  : Positive;
       Mode   : HAL.Framebuffer.FB_Color_Mode;
       X      : Natural := 0;
       Y      : Natural := 0;
       Width  : Positive := Positive'Last;
       Height : Positive := Positive'Last)
   is null;

   overriding
   function Initialized
      (This  : Device;
       Layer : Positive)
       return Boolean
   is (True);

   overriding
   procedure Update_Layer
      (This      : in out Device;
       Layer     : Positive;
       Copy_Back : Boolean := False)
   is
   begin
      This.Update;
   end Update_Layer;

   overriding
   procedure Update_Layers
      (This : in out Device)
   is
   begin
      This.Update;
   end Update_Layers;

   overriding
   function Color_Mode
      (This  : Device;
       Layer : Positive)
       return HAL.Framebuffer.FB_Color_Mode
   is (HAL.Bitmap.M_1);

   overriding
   function Pixel_Size
      (Display : Device;
       Layer   : Positive)
       return Positive
   is (1);

end UC8151;
