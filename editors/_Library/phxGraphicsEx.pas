unit phxGraphicsEx;
 {$mode Delphi}
interface

uses Graphics, Types, LCLType, LCLIntf, Classes, SysUtils, Controls, ImgList,

  phxGraphics,
  phxTexture,
  phxDevice;
 // phxTypes,
//  phxClasses,
 // phxDevice,
 // phxGraphics,
//  phxImage


procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap); overload;
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; Background: TBitmap ); overload;
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; Background: TColor ); overload;

procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; const Rect: TRect); overload;
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; const Rect: TRect; Background: TBitmap ); overload;
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; const Rect: TRect; Background: TColor  ); overload;


procedure DrawBitmap(Bitmap: TPHXBitmap; Dest: TBitmap); overload;
procedure DrawBitmap(Bitmap: TPHXBitmap; Dest: TBitmap; Background: TBitmap ); overload;

// Draw a transparent pattern to the canvas
procedure DrawTransparent(const Canvas: TCanvas; const Rect: TRect; const Size: Integer);

procedure BitmapToTexture(const Bitmap: TBitmap; const Mask: TBitmap; const Texture: TPHXTexture);


type

//------------------------------------------------------------------------------
TPHXColorChannel = (
  chRed,
  chGreen,
  chBlue,
  chAlpha,
  chColor
  );
//------------------------------------------------------------------------------
TPHXTextureEx = class helper for TPHXTexture
  public
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap); overload;
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap; Background: TBitmap ); overload;
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap; Background: TColor ); overload;
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap; Channel: TPHXColorChannel); overload;

    // Read the version of a texture
    class function ReadVersion(const FileName: String ): Integer; overload;
    class function ReadVersion(const Stream  : TStream): Integer; overload;

   // procedure LoadVersion1(const FileName: String); overload;

    //procedure LoadVersion1(Stream  : TStream); overload;
    procedure LoadTexture(Bitmap: TBitmap); overload;
    procedure LoadTexture(ImageList: TImageList; Index: Integer); overload;
  end;


//------------------------------------------------------------------------------
TPHXBitmapEx = class helper for TPHXBitmap
  public
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap); overload;
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap; Background: TBitmap ); overload;
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap; Background: TColor ); overload;
    // Convert the texture to a TBitmap
    procedure ToBitmap(Dest: TBitmap; Channel: TPHXColorChannel); overload;

    // Convert the bitmap to a TBitmap
    procedure Draw(Dest: TBitmap); overload;
    // Convert the bitmap to a TBitmap
    procedure Draw(Dest: TBitmap; Background: TBitmap ); overload;
  end;
      {
//------------------------------------------------------------------------------
TPHXImageEx = class helper for TPHXImage
  public
  procedure Draw(Dest: TBitmap); overload;
  procedure Draw(Dest: TBitmap; Background: TBitmap ); overload;
  procedure Draw(Dest: TBitmap; Background: TColor ); overload;

  procedure DrawPattern(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; PatternIndex: Integer); overload;
  procedure DrawPattern(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; PatternIndex: Integer; Zoom: Single); overload;
  procedure DrawPattern(Dest: TCanvas; Buffer: TBitmap; const Rect: TRect; const PatternIndex: Integer); overload;
end;      }

//------------------------------------------------------------------------------
TBitmapEx = class helper for TBitmap
  public
    class function CreateBackground(const Size: Integer): TBitmap;
  end;

Function ScanLine(Texture: TPHXTexture; Line: Integer): PByte; overload;
Function ScanLine(Bitmap  : TPHXBitmap; Line: Integer): PByte; overload;

function CreateTransparentImage(const Size: Integer): TBitmap;

implementation

//------------------------------------------------------------------------------
Function ScanLine(Texture: TPHXTexture; Line: Integer): PByte;
var PixelSize: Integer;
begin
  PixelSize:= GetPixelFormatSize(Texture.Format);

  Result:= @Texture.Pixels^[Line * Texture.Width * PixelSize];
end;

//------------------------------------------------------------------------------
Function ScanLine(Bitmap: TPHXBitmap; Line: Integer): PByte;
var PixelSize: Integer;
begin
  PixelSize:= GetPixelFormatSize(Bitmap.Format);

  Result:= @Bitmap.Pixels^[Line * Bitmap.Width * PixelSize];
end;

//------------------------------------------------------------------------------
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Texture.Width;
  Dest.Height      := Texture.Height;

  for y := 0 to Texture.Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Texture.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; Background: TBitmap );
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Texture.Width;
  Dest.Height      := Texture.Height;

  if (Background.Width > 0) and (Background.Height > 0) then
  begin
    Y:=0;
    while Y < Dest.Height do
    begin
      X:=0;
      while X < Dest.Width do
      begin
        Dest.Canvas.Draw(X,Y, Background);

        Inc(X, Background.Width);
      end;
      Inc(Y, Background.Height);
   end;
  end;

  for y := 0 to Texture.Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Texture.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; Background: TColor );
var X,Y  : Integer;
var DstColor: TRGBQuad;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Texture.Width;
  Dest.Height      := Texture.Height;

  for y := 0 to Texture.Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Texture.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor.rgbRed  := Trunc( GetRValue(Background) * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbGreen:= Trunc( GetGValue(Background) * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbBlue := Trunc( GetBValue(Background) * (1- Alpha) + SrcColor.Blue  * (Alpha));
      DstColor.rgbReserved:= 0;

      PRGBQuad(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; const Rect: TRect);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Rect.Right  - Rect.Left;
  Dest.Height      := Rect.Bottom - Rect.Top;

  for y := Rect.Top to Rect.Bottom do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);

    SrcPixel:= SrcPixel + GetPixelFormatSize(Texture.Format) * Rect.Left;

    for x := Rect.Left to Rect.Right - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; const Rect: TRect; Background: TBitmap );
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Rect.Right  - Rect.Left;
  Dest.Height      := Rect.Bottom - Rect.Top;

  if (Background.Width > 0) and (Background.Height > 0) then
  begin
    Y:=0;
    while Y < Dest.Height do
    begin
      X:=0;
      while X < Dest.Width do
      begin
        Dest.Canvas.Draw(X,Y, Background);

        Inc(X, Background.Width);
      end;
      Inc(Y, Background.Height);
   end;
  end;

  for y := Rect.Top to Rect.Bottom do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);

    SrcPixel:= SrcPixel + GetPixelFormatSize(Texture.Format) * Rect.Left;

    for x := Rect.Left to Rect.Right - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure DrawTexture(Texture: TPHXTexture; Dest: TBitmap; const Rect: TRect; Background: TColor  ); overload;
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Rect.Right  - Rect.Left;
  Dest.Height      := Rect.Bottom - Rect.Top;

  for y := Rect.Top to Rect.Bottom do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);

    SrcPixel:= SrcPixel + GetPixelFormatSize(Texture.Format) * Rect.Left;

    for x := Rect.Left to Rect.Right - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor.rgbtRed  := Trunc( GetRValue(Background) * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc( GetGValue(Background) * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc( GetBValue(Background) * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure BitmapToTexture(const Bitmap: TBitmap; const Mask: TBitmap; const Texture: TPHXTexture);
var SetPixel: TSetPixel;
var X,Y  : Integer;
var DstColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixelB: PByte;
var SrcPixelM: PByte;
var SrcColor: TRGBTriple;
var SrcAlpha: TRGBTriple;
begin
  Texture.Height:= Bitmap.Height;
  Texture.Width := Bitmap.Width;
  Texture.Format:= pfRGBA;

  SetPixel:= GetPixelFormatSetter(Texture.Format);

  Bitmap.PixelFormat:= pf24bit;
  Mask  .PixelFormat:= pf24bit;

  for y := 0 to Texture.Height - 1 do
  begin
    DstPixel:= ScanLine(Texture, Y);

    SrcPixelB:= Bitmap.Scanline[y];
    SrcPixelM:= Mask  .Scanline[y];
    for x := 0 to Texture.Width - 1 do
    begin
      SrcColor:= PRGBTriple(SrcPixelB)^;
      SrcAlpha:= PRGBTriple(SrcPixelM)^;

      DstColor.Red  := SrcColor.rgbtRed;
      DstColor.Green:= SrcColor.rgbtGreen;
      DstColor.Blue := SrcColor.rgbtBlue;
      DstColor.Alpha:= SrcAlpha.rgbtRed;

      SetPixel(DstPixel, DstColor);

      Inc(SrcPixelB, 3);//  pf24bit
      Inc(SrcPixelM, 3);//  pf24bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure DrawBitmap(Bitmap: TPHXBitmap; Dest: TBitmap);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Bitmap.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Bitmap.Width;
  Dest.Height      := Bitmap.Height;

  for y := 0 to Bitmap.Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Bitmap, Y);
    for x := 0 to Bitmap.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure DrawBitmap(Bitmap: TPHXBitmap; Dest: TBitmap; Background: TBitmap );
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Bitmap.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Bitmap.Width;
  Dest.Height      := Bitmap.Height;

  if (Background.Width > 0) and (Background.Height > 0) then
  begin
    Y:=0;
    while Y < Bitmap.Height do
    begin
      X:=0;
      while X < Bitmap.Width do
      begin
        Dest.Canvas.Draw(X,Y, Background);
        Inc(X, Background.Width);
      end;
      Inc(Y, Background.Height);
   end;
  end;

  for y := 0 to Bitmap.Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Bitmap, Y);
    for x := 0 to Bitmap.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure DrawTransparent(const Canvas: TCanvas; const Rect: TRect; const Size: Integer);
var ix: Integer;
var iy: Integer;
var px: Integer;
var py: Integer;
var DstRect: TRect;
begin
  iy:= 0;
  py:= Rect.Top;
  while py < Rect.Bottom do
  begin
    ix:= 0;
    px:= Rect.Left;
    while px < Rect.Right do
    begin
      if (ix + iy) mod 2 = 0 then
      begin
        Canvas.Brush.Color:= clSilver;
      end else
      begin
        Canvas.Brush.Color:= clWhite;
      end;

      DstRect:= Classes.Rect(PX, PY, PX + Size, PY + Size);

      if DstRect.Right  > Rect.Right  then DstRect.Right := Rect.Right;
      if DstRect.Bottom > Rect.Bottom then DstRect.Bottom:= Rect.Bottom;

      Canvas.FillRect(DstRect);

      Inc(ix);
      Inc(px, Size);
    end;
    Inc(iy);
    Inc(py, Size);
  end;
end;

//------------------------------------------------------------------------------
function CreateTransparentImage(const Size: Integer): TBitmap;
begin
  Result:= TBitmap.Create;
  Result.Width      := Size * 2;
  Result.Height     := Size * 2;
  Result.PixelFormat:= pf32bit;

  DrawTransparent(Result.Canvas, Result.Canvas.ClipRect, Size);
end;







// TPHXTextureEx
//==============================================================================
procedure TPHXTextureEx.ToBitmap(Dest: TBitmap);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXTextureEx.ToBitmap(Dest: TBitmap; Background: TBitmap );
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  if (Background.Width > 0) and (Background.Height > 0) then
  begin
    Y:=0;
    while Y < Dest.Height do
    begin
      X:=0;
      while X < Dest.Width do
      begin
        Dest.Canvas.Draw(X,Y, Background);

        Inc(X, Background.Width);
      end;
      Inc(Y, Background.Height);
   end;
  end;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Y);

    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXTextureEx.ToBitmap(Dest: TBitmap; Background: TColor );
var X,Y  : Integer;
var DstColor: TRGBQuad;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Self.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor.rgbRed  := Trunc( GetRValue(Background) * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbGreen:= Trunc( GetGValue(Background) * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbBlue := Trunc( GetBValue(Background) * (1- Alpha) + SrcColor.Blue  * (Alpha));
      DstColor.rgbReserved:= 0;

      PRGBQuad(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTextureEx.ToBitmap(Dest: TBitmap; Channel: TPHXColorChannel);
var X,Y  : Integer;
var DstColor: TRGBQuad;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Self.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      case Channel of
        chRed:
        begin
          DstColor.rgbRed     := SrcColor.Red;
          DstColor.rgbGreen   := SrcColor.Red;
          DstColor.rgbBlue    := SrcColor.Red;
          DstColor.rgbReserved:= 0;
        end;
        chGreen:
        begin
          DstColor.rgbRed     := SrcColor.Green;
          DstColor.rgbGreen   := SrcColor.Green;
          DstColor.rgbBlue    := SrcColor.Green;
          DstColor.rgbReserved:= 0;
        end;
        chBlue:
        begin
          DstColor.rgbRed     := SrcColor.Blue;
          DstColor.rgbGreen   := SrcColor.Blue;
          DstColor.rgbBlue    := SrcColor.Blue;
          DstColor.rgbReserved:= 0;
        end;
        chAlpha:
        begin
          DstColor.rgbRed     := SrcColor.Alpha;
          DstColor.rgbGreen   := SrcColor.Alpha;
          DstColor.rgbBlue    := SrcColor.Alpha;
          DstColor.rgbReserved:= 0;
        end;
        chColor:
        begin
          DstColor.rgbRed     := SrcColor.Red;
          DstColor.rgbGreen   := SrcColor.Green;
          DstColor.rgbBlue    := SrcColor.Blue;
          DstColor.rgbReserved:= 0;
        end;
      end;
      PRGBQuad(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
class function TPHXTextureEx.ReadVersion(const FileName: String): Integer;
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result:= ReadVersion(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
class function TPHXTextureEx.ReadVersion(const Stream: TStream): Integer;
var Header : TPHXTextureHeader;
begin
  Header.Ident := #0#0#0#0#0#0;
  Header.Version:= 0;


  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXTEX') then
  begin
    raise Exception.Create('Not a valid Phoenix texture.');
  end;

  Result:= Header.Version;
end;


//------------------------------------------------------------------------------
procedure TPHXTextureEx.LoadTexture(Bitmap: TBitmap);
var X,Y  : Integer;
var DstColor: TPHXPixel;
var SrcColor: PRGBTriple;
var DstPixel: PByte;
//var SrcPixel: PByte;

var SetPixel: TSetPixel;
begin
  Self.Format := pfRGB;
  Self.Width  := Bitmap.Width;
  Self.Height := Bitmap.Height;

  SetPixel:= GetPixelFormatSetter(Format);

  for y := 0 to Height - 1 do
  begin
    DstPixel:= ScanLine(Y);
    SrcColor:= Bitmap.Scanline[y];
    for x := 0 to Width - 1 do
    begin
      //PRGBTriple(DstPixel)^:= DstColor;
     // SrcColor:=

      DstColor.Red  := SrcColor.rgbtRed;
      DstColor.Green:= SrcColor.rgbtGreen;
      DstColor.Blue := SrcColor.rgbtBlue;

      SetPixel(DstPixel, DstColor);

      Inc(SrcColor, 3);//  pf24bit
    end;
  end;
end;

// TPHXTextureEx
//------------------------------------------------------------------------------
procedure TPHXTextureEx.LoadTexture(ImageList: TImageList; Index: Integer);
var Bitmap: TBitmap;
begin
  Bitmap:= TBitmap.Create;
  try
    Bitmap.PixelFormat:= pf24bit;

    ImageList.GetBitmap(Index, Bitmap);

    LoadTexture(Bitmap);
  finally
    Bitmap.Free;
  end;

  Build;
end;



                        {
//------------------------------------------------------------------------------
procedure TPHXTextureEx.LoadVersion1(const FileName: String);
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadVersion1(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTextureEx.LoadVersion1(Stream: TStream);
var Header: TPHXTextureHeader;
var FileExt: AnsiString;
var Graphic: TPHXGraphic;
var Importer: TPHXGraphicImporter;

var AWidth : Integer;
var AHeight: Integer;
var AFormat: TPHXPixelFormat;
var ASize   : Integer;
var APixels: PByteArray;
begin
  Header.Ident := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXTEX') then
  begin
    raise Exception.Create('Not a valid Phoenix texture.');
  end;

  If (Header.Version <> PHXTEXTURE_VERSION) then
  begin
    raise Exception.CreateFmt('Texture version missmatch [File: %d Code: %d].', [Header.Version, PHXTEXTURE_VERSION]);
  end;
  Name:= ReadStr(Stream);

  Stream.Read(AWidth      , SizeOf(Integer));
  Stream.Read(AHeight     , SizeOf(Integer));
  Stream.Read(AFormat     , SizeOf(TPHXPixelFormat));
  Stream.Read(ASize       , SizeOf(Integer));

//  SetSize(AWidth, AHeight, AFormat);

  Settings.LoadFromStream(Stream);

  ReadStr(Stream, FileExt);

  if GraphicFormats.FindImporter(FileExt, Importer) then
  begin
 //   ReAllocMem(Pixels, 0);

    Importer.LoadGraphic(Stream, FileExt, Graphic);

   // FWidth := Graphic.Width;
   // FHeight:= Graphic.Height;
   // //FFormat:= Graphic.Format;
  //  FSize  := Graphic.Size;
   // FPixels:= Graphic.Pixels;

    Import(Graphic);
  end else
  begin
    ReAllocMem(APixels, ASize);

    Stream.Read(APixels^, ASize);

    Import(AWidth, AHeight, AFormat, APixels);
  end;

  Compressor:= GraphicFormats.FindExporter(FileExt);

  Build;
end;

                }




// TPHXBitmapEx
//==============================================================================
procedure TPHXBitmapEx.ToBitmap(Dest: TBitmap);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Self.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBitmapEx.ToBitmap(Dest: TBitmap; Background: TBitmap );
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  if (Background.Width > 0) and (Background.Height > 0) then
  begin
    Y:=0;
    while Y < Dest.Height do
    begin
      X:=0;
      while X < Dest.Width do
      begin
        Dest.Canvas.Draw(X,Y, Background);

        Inc(X, Background.Width);
      end;
      Inc(Y, Background.Height);
   end;
  end;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Self.ScanLine(Y);

    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBitmapEx.ToBitmap(Dest: TBitmap; Background: TColor );
var X,Y  : Integer;
var DstColor: TRGBQuad;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Self.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor.rgbRed  := Trunc( GetRValue(Background) * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbGreen:= Trunc( GetGValue(Background) * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbBlue := Trunc( GetBValue(Background) * (1- Alpha) + SrcColor.Blue  * (Alpha));
      DstColor.rgbReserved:= 0;

      PRGBQuad(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmapEx.ToBitmap(Dest: TBitmap; Channel: TPHXColorChannel);
var X,Y  : Integer;
var DstColor: TRGBQuad;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Self.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      case Channel of
        chRed:
        begin
          DstColor.rgbRed     := SrcColor.Red;
          DstColor.rgbGreen   := SrcColor.Red;
          DstColor.rgbBlue    := SrcColor.Red;
          DstColor.rgbReserved:= 0;
        end;
        chGreen:
        begin
          DstColor.rgbRed     := SrcColor.Green;
          DstColor.rgbGreen   := SrcColor.Green;
          DstColor.rgbBlue    := SrcColor.Green;
          DstColor.rgbReserved:= 0;
        end;
        chBlue:
        begin
          DstColor.rgbRed     := SrcColor.Blue;
          DstColor.rgbGreen   := SrcColor.Blue;
          DstColor.rgbBlue    := SrcColor.Blue;
          DstColor.rgbReserved:= 0;
        end;
        chAlpha:
        begin
          DstColor.rgbRed     := SrcColor.Alpha;
          DstColor.rgbGreen   := SrcColor.Alpha;
          DstColor.rgbBlue    := SrcColor.Alpha;
          DstColor.rgbReserved:= 0;
        end;
        chColor:
        begin
          DstColor.rgbRed     := SrcColor.Red;
          DstColor.rgbGreen   := SrcColor.Green;
          DstColor.rgbBlue    := SrcColor.Blue;
          DstColor.rgbReserved:= 0;
        end;
      end;
      PRGBQuad(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBitmapEx.Draw(Dest: TBitmap);
begin
  DrawBitmap(Self, Dest);
end;

//------------------------------------------------------------------------------
procedure TPHXBitmapEx.Draw(Dest, Background: TBitmap);
begin
  DrawBitmap(Self, Dest, Background);
end;


// TBitmapEx
//------------------------------------------------------------------------------
class function TBitmapEx.CreateBackground(const Size: Integer): TBitmap;
begin
  Result:= CreateTransparentImage(Size);
end;


       {
// TPHXImageEx
//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest: TBitmap);
begin
  DrawTexture(Self.Texture, Dest);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest, Background: TBitmap);
begin
  DrawTexture(Self.Texture, Dest, Background);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest: TBitmap; Background: TColor);
begin
  DrawTexture(Self.Texture, Dest, Background);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.DrawPattern(Dest: TCanvas; Buffer: TBitmap; X,Y: Integer; PatternIndex: Integer);
var SrcRect: TRect;
var DstRect: TRect;
begin
  if (PatternIndex < 0) or (PatternIndex >= Patterns.Count) then Exit;

  SrcRect.Left  := Patterns[PatternIndex].X;
  SrcRect.Top   := Patterns[PatternIndex].Y;
  SrcRect.Right := Patterns[PatternIndex].X + Patterns[PatternIndex].Width;
  SrcRect.Bottom:= Patterns[PatternIndex].Y + Patterns[PatternIndex].Height;

  DstRect.Left  :=  X;
  DstRect.Top    := Y;
  DstRect.Right  := X + Patterns[PatternIndex].Width;
  DstRect.Bottom := Y + Patterns[PatternIndex].Height;

  Dest.CopyRect(DstRect, Buffer.Canvas, SrcRect);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.DrawPattern(Dest: TCanvas; Buffer: TBitmap; X, Y, PatternIndex: Integer; Zoom: Single);
var SrcRect: TRect;
var DstRect: TRect;
begin
  if (PatternIndex < 0) or (PatternIndex >= Patterns.Count) then Exit;

  SrcRect.Left  := Patterns[PatternIndex].X;
  SrcRect.Top   := Patterns[PatternIndex].Y;
  SrcRect.Right := Patterns[PatternIndex].X + Patterns[PatternIndex].Width;
  SrcRect.Bottom:= Patterns[PatternIndex].Y + Patterns[PatternIndex].Height;

  DstRect.Left  :=  X;
  DstRect.Top    := Y;
  DstRect.Right  := X + Round(Patterns[PatternIndex].Width  * Zoom);
  DstRect.Bottom := Y + Round(Patterns[PatternIndex].Height * Zoom);

  Dest.CopyRect(DstRect, Buffer.Canvas, SrcRect);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.DrawPattern(Dest: TCanvas; Buffer: TBitmap; const Rect: TRect; const PatternIndex: Integer);
var Pattern: TPHXPattern;
var X,Y: Integer;
begin
  if( PatternIndex >= 0) and (PatternIndex < Patterns.Count) then
  begin
    Pattern:= Patterns.List^[PatternIndex];

    X:= Rect.Left + ((Rect.Right  - Rect.Left) - Pattern.Width ) div 2;
    Y:= Rect.Top  + ((Rect.Bottom - Rect.Top ) - Pattern.Height) div 2;

    DrawPattern(Dest, Buffer, X,Y, PatternIndex);
  end;
end;

      }



end.
