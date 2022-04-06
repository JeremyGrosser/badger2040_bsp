with Ada.Unchecked_Conversion;
with Memory_Mapped_Bitmap;
with HAL.Framebuffer;
with HAL.Bitmap;
with HAL; use HAL;
with HAL.Time;
with RP.GPIO;
with HAL.SPI;

package UC8151 is

   Device_Width  : constant := 296;
   Device_Height : constant := 128;
   subtype Column is Positive range 1 .. Device_Width;
   subtype Row    is Positive range 1 .. Device_Height;

   type Pixel is mod 2
      with Size => 1;

   type Pixels is array (Column, Row) of Pixel
      with Component_Size => 1;

   type Update_Speed is (Default, Medium, Fast, Turbo);

   type Device
      (Port              : not null HAL.SPI.Any_SPI_Port;
       Delays            : not null HAL.Time.Any_Delays;
       CS, DC, BUSY, RST : not null access RP.GPIO.GPIO_Point)
   is new HAL.Framebuffer.Frame_Buffer_Display with private;

   procedure Initialize
      (This : in out Device);

   procedure Update
      (This : in out Device;
       Blocking : Boolean := True);

private

   type Device
      (Port              : not null HAL.SPI.Any_SPI_Port;
       Delays            : not null HAL.Time.Any_Delays;
       CS, DC, BUSY, RST : not null access RP.GPIO.GPIO_Point)
   is new HAL.Framebuffer.Frame_Buffer_Display with record
      Frame_Buffer : aliased Pixels;
      Buffer       : aliased Memory_Mapped_Bitmap.Memory_Mapped_Bitmap_Buffer;
      Speed        : Update_Speed := Default;
      Init         : Boolean := False;
   end record;

   type Register is
      (PSR, PWR, POF, PFS, PON, PMES, BTST, DSLP, DTM1, DSP, DRF, DTM2,
       LUT_VCOM, LUT_WW, LUT_BW, LUT_WB, LUT_BB, PLL, TSC, TSE, TSW, TSR, CDI,
       LPD, TCON, TRES, REV, FLG, AMV, VV, VDCS, PTL, PTIN, PTOU, PGM, APG,
       ROTP, CCSET, PWS, TSSET);
   for Register use
      (PSR      => 16#00#,
       PWR      => 16#01#,
       POF      => 16#02#,
       PFS      => 16#03#,
       PON      => 16#04#,
       PMES     => 16#05#,
       BTST     => 16#06#,
       DSLP     => 16#07#,
       DTM1     => 16#10#,
       DSP      => 16#11#,
       DRF      => 16#12#,
       DTM2     => 16#13#,
       LUT_VCOM => 16#20#,
       LUT_WW   => 16#21#,
       LUT_BW   => 16#22#,
       LUT_WB   => 16#23#,
       LUT_BB   => 16#24#,
       PLL      => 16#30#,
       TSC      => 16#40#,
       TSE      => 16#41#,
       TSW      => 16#42#,
       TSR      => 16#43#,
       CDI      => 16#50#,
       LPD      => 16#51#,
       TCON     => 16#60#,
       TRES     => 16#61#,
       REV      => 16#70#,
       FLG      => 16#71#,
       AMV      => 16#80#,
       VV       => 16#81#,
       VDCS     => 16#82#,
       PTL      => 16#90#,
       PTIN     => 16#91#,
       PTOU     => 16#92#,
       PGM      => 16#a0#,
       APG      => 16#a1#,
       ROTP     => 16#a2#,
       CCSET    => 16#e0#,
       PWS      => 16#e3#,
       TSSET    => 16#e5#);

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
      SCAN    : PSR_SCAN_Field   := Scan_Up;
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

   subtype PSR_Bytes is UInt8_Array (1 .. PSR_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => PSR_Register,
       Target => PSR_Bytes);

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

   subtype PWR_Bytes is UInt8_Array (1 .. PWR_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => PWR_Register,
       Target => PWR_Bytes);

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

   subtype BTST_Bytes is UInt8_Array (1 .. BTST_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => BTST_Register,
       Target => BTST_Bytes);

   type PFS_Register is record
      T_VDS_OFF : UInt2 := 0;
   end record
      with Size => 8;
   for PFS_Register use record
      T_VDS_OFF at 0 range 4 .. 5;
   end record;
   subtype PFS_Bytes is UInt8_Array (1 .. PFS_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => PFS_Register,
       Target => PFS_Bytes);

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

   subtype TSE_Bytes is UInt8_Array (1 .. TSE_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => TSE_Register,
       Target => TSE_Bytes);

   type TCON_Register is new UInt8;
   subtype TCON_Bytes is UInt8_Array (1 .. TCON_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => TCON_Register,
       Target => TCON_Bytes);

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

   subtype CDI_Bytes is UInt8_Array (1 .. CDI_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => CDI_Register,
       Target => CDI_Bytes);

   type PLL_Register is record
      M, N : UInt3;
   end record
      with Size => 8;
   for PLL_Register use record
      M at 0 range 3 .. 5;
      N at 0 range 0 .. 2;
   end record;
   subtype PLL_Bytes is UInt8_Array (1 .. PLL_Register'Size / 8);
   function To_Bytes is new Ada.Unchecked_Conversion
      (Source => PLL_Register,
       Target => PLL_Bytes);

   procedure Command
      (This : in out Device;
       Reg  : Register;
       Data : UInt8_Array);

   procedure Command
      (This : in out Device;
       Reg  : Register);

   procedure Busy_Wait
      (This : in out Device);

   function Is_Busy
      (This : Device)
      return Boolean;

   procedure Reset
      (This : in out Device);

   procedure Setup
      (This  : in out Device;
       Speed : Update_Speed := Default);

   procedure Power_Off
      (This : in out Device;
       Wait : Boolean);

   procedure Power_On
      (This : in out Device;
       Wait : Boolean);

   procedure Default_LUTs
      (This : in out Device);

   procedure Medium_LUTs
      (This : in out Device);

   procedure Fast_LUTs
      (This : in out Device);

   procedure Turbo_LUTs
      (This : in out Device);

   procedure Write
      (This : in out Device;
       Data : UInt8_Array);

   procedure Set_Invert
      (This    : in out Device;
       Enabled : Boolean);

   procedure Set_Update_Speed
      (This  : in out Device;
       Speed : Update_Speed);

   function Update_Time
      (This : Device)
      return Natural;

   overriding
   function Max_Layers
      (This : Device)
      return Positive;

   overriding
   function Supported
      (This : Device;
       Mode : HAL.Framebuffer.FB_Color_Mode)
       return Boolean;

   overriding
   procedure Set_Orientation
      (This        : in out Device;
       Orientation : HAL.Framebuffer.Display_Orientation);

   overriding
   procedure Set_Mode
      (This : in out Device;
       Mode : HAL.Framebuffer.Wait_Mode);

   overriding
   function Initialized
      (This : Device)
      return Boolean;

   overriding
   function Width
      (This : Device)
      return Positive;

   overriding
   function Height
      (This : Device)
      return Positive;

   overriding
   function Swapped
      (This : Device)
      return Boolean;

   overriding
   procedure Set_Background
      (This    : Device;
       R, G, B : UInt8);

   overriding
   procedure Initialize_Layer
      (This   : in out Device;
       Layer  : Positive;
       Mode   : HAL.Framebuffer.FB_Color_Mode;
       X      : Natural := 0;
       Y      : Natural := 0;
       Width  : Positive := Positive'Last;
       Height : Positive := Positive'Last);

   overriding
   function Initialized
      (This  : Device;
       Layer : Positive)
       return Boolean;

   overriding
   procedure Update_Layer
      (This      : in out Device;
       Layer     : Positive;
       Copy_Back : Boolean := False);

   overriding
   procedure Update_Layers
      (This : in out Device);

   overriding
   function Color_Mode
      (This  : Device;
       Layer : Positive)
       return HAL.Framebuffer.FB_Color_Mode;

   overriding
   function Hidden_Buffer
      (This  : in out Device;
       Layer : Positive)
       return not null HAL.Bitmap.Any_Bitmap_Buffer;

   overriding
   function Pixel_Size
      (Display : Device;
       Layer   : Positive)
       return Positive;

end UC8151;
