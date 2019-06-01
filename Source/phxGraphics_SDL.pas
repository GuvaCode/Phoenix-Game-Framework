////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxGraphics_SDL;
//< Support for loading textures using SDL_Image

interface

{$I phxConfig.inc}

uses Types, Classes, SysUtils,

  phxTypes,
	phxGraphics,

	sdl,
  sdl_image;

type

// The graphic filer for SDL_Image
//------------------------------------------------------------------------------
TPHXSDLImageFiler = class(TPHXGraphicFiler)
  private
  protected
    // Read a graphic from a stream
    procedure LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);  override;
    // Write a graphic image to a stream
    procedure SaveGraphic(Stream: TStream; const Name: String; const Graphic: TPHXGraphic);  override;
  public
    // Creates the FreeImage graphics filer
    constructor Create;

    // Register the supported file formats for this graphic filer
    procedure RegisterFileFormats; override;

    // Returns if this filer supports reading of a format
    function SupportsReading(const Filename: string): Boolean; override;
    // Returns if this filer supports writing of a format
    function SupportsWriting(const Filename: string): Boolean; override;
  end;

// The global instance of the loader
var Filer: TPHXSDLImageFiler;

implementation

{$REGION 'TSDL_RWops'}

//------------------------------------------------------------------------------
function RWopsSeek( context: PSDL_RWops; offset: Integer; whence: Integer ): Integer; cdecl;
var Stream: TStream;
begin
  Stream:= TStream(context.unknown.data1);

  Result:= Stream.Seek(Offset, whence)
end;

//------------------------------------------------------------------------------
function RWopsRead( context: PSDL_RWops; Ptr: Pointer; size: Integer; maxnum : Integer ): Integer;  cdecl;
var Stream: TStream;
begin
  Stream:= TStream(context.unknown.data1);

  Result:= Stream.Read(Ptr^, Size* maxnum);
end;

//------------------------------------------------------------------------------
function RWopsWrite( context: PSDL_RWops; Ptr: Pointer; size: Integer; num: Integer ): Integer; cdecl;
var Stream: TStream;
begin
  Stream:= TStream(context.unknown.data1);

   Result:= Stream.Write(Ptr^, Size * num);
end;

//------------------------------------------------------------------------------
function RWopsClose( context: PSDL_RWops ): Integer; cdecl;
begin
  Result:= 0;
end;

{$ENDREGION}

{$REGION 'PSDL_Surface Conversion'}

// Converts a grayscale Bitmap to a TPHXGraphic
//------------------------------------------------------------------------------
procedure SurfaceToGraphic8(const Surface: PSDL_Surface; out Graphic: TPHXGraphic);
var X, Y: Integer;
var Pix : PByte ;
var Line: PByteArray;
var DstPixel: PByteArray;
var R,G,B: Byte;
begin
  Graphic.Width  := Surface.w;
  Graphic.Height := Surface.h;
  Graphic.Pixels := GetMemory(Graphic.Width * Graphic.Height * 1);
  Graphic.Format := pfAlpha;

  DstPixel:= @Graphic.Pixels^;
  for Y:=0 to Graphic.Height-1 do
  begin
    Line:= @PByteArray(Surface.pixels)[Y  * Graphic.Width * Surface.format.BytesPerPixel];
    for X:=0 to Graphic.Width-1 do
    begin
      Pix:= @Line[X * Surface.format.BytesPerPixel];

      SDL_GetRGB( PCardinal(Pix)^, Surface.format, @R, @G, @B);

      DstPixel^[0]:= R;

      Inc( PByte(DstPixel), 1);
   end;
  end;
end;

// Converts a RGB Bitmap to a TPHXGraphic
//------------------------------------------------------------------------------
procedure SurfaceToGraphic24(const Surface: PSDL_Surface; out Graphic: TPHXGraphic);
var X, Y: Integer;
var Pix : PByte ;
var Line: PByteArray;
var DstPixel: PByteArray;
var R,G,B: Byte;
begin
  Graphic.Width  := Surface.w;
  Graphic.Height := Surface.h;
  Graphic.Pixels := GetMemory(Graphic.Width * Graphic.Height * 3);
  Graphic.Format := pfRGB;

  DstPixel:= @Graphic.Pixels^;
  for Y:=0 to Graphic.Height-1 do
  begin
 //   Line := @PByteArray(Surface.pixels)[Y * Surface.pitch];
    Line := @PByteArray(Surface.pixels)[Y  * Graphic.Width * Surface.format.BytesPerPixel];
    for X:=0 to Graphic.Width-1 do
    begin
      Pix:=@Line[X * Surface.format.BytesPerPixel];

      SDL_GetRGB( PCardinal(Pix)^, Surface.format, @R, @G, @B);

      DstPixel^[0]:= R;
      DstPixel^[1]:= G;
      DstPixel^[2]:= B;

      Inc( PByte(DstPixel), 3);
   end;
  end;
end;

// Converts a RGBA Bitmap to a TPHXGraphic
//------------------------------------------------------------------------------
procedure SurfaceToGraphic32(const Surface: PSDL_Surface; out Graphic: TPHXGraphic);
var X, Y: Integer;
var Pix : PByte ;
var Line: PByteArray;
var DstPixel: PByteArray;
var R,G,B, A: Byte;
begin
  Graphic.Width  := Surface.w;
  Graphic.Height := Surface.h;
  Graphic.Pixels := GetMemory(Graphic.Width * Graphic.Height * 4);
  Graphic.Format := pfRGBA;

  DstPixel:= @Graphic.Pixels^;
  for Y:=0 to Graphic.Height-1 do
  begin
//    Line := @PByteArray(Surface.pixels)[Y * Surface.pitch];
    Line := @PByteArray(Surface.pixels)[Y  * Graphic.Width * Surface.format.BytesPerPixel];
    for X:=0 to Graphic.Width-1 do
    begin
      Pix:=@Line[X * Surface.format.BytesPerPixel];

      SDL_GetRGBA(PCardinal(Pix)^, Surface.format, @R, @G, @B, @A);

      DstPixel^[0]:= R;
      DstPixel^[1]:= G;
      DstPixel^[2]:= B;
      DstPixel^[3]:= A;

      Inc( PByte(DstPixel), 4);
   end;
  end;
end;
    
{$ENDREGION}

{$REGION 'TPHXSDLImageFiler'}

// TPHXSDLImageFiler
//==============================================================================
constructor TPHXSDLImageFiler.Create;
begin
  inherited Create;
end;


//------------------------------------------------------------------------------
procedure TPHXSDLImageFiler.RegisterFileFormats;
begin
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.TGA', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.BMP', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.PNM', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.XPM', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.XCF', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.PCX', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.GIF', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.JPG', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.TIF', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.LBM', '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '*.PNG', '');

  TPHXGraphicFiler.RegisterFileFormat(Self, '*', '');
end;

//------------------------------------------------------------------------------
procedure TPHXSDLImageFiler.LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);
var Image: PSDL_Surface;
var rwop: TSDL_RWops;
var Kind: String;
begin
  // Reset graphics
  Graphic.Width := 0;
  Graphic.Height:= 0;
  Graphic.Format:= pfNone;
  Graphic.Pixels:= nil;


  rwop.seek:= {$IFDEF FPC}@{$ENDIF}RWopsSeek;
  rwop.read:= {$IFDEF FPC}@{$ENDIF}RWopsRead;
  rwop.write:= {$IFDEF FPC}@{$ENDIF}RWopsWrite;
  rwop.close:= {$IFDEF FPC}@{$ENDIF}RWopsClose;

  rwop.unknown.data1:= Stream;

  Kind:= UpperCase( Copy(ExtractFileExt(Name), 2, MaxInt));

  Image:= IMG_LoadTyped_RW(@rwop, 0, PAnsiChar(AnsiString(Kind)));

  if not Assigned(Image) then
  begin
    raise Exception.CreateFmt('Could not load the image "%s":'#13'%s', [Name, IMG_GetError()]);
  end;

  case Image.format.BitsPerPixel of
    8: begin
  //   SDL_ConvertSurface(Image,
      SurfaceToGraphic8(Image, Graphic);
    end;
    24: begin
      SurfaceToGraphic24(Image, Graphic);
    end;
    32:begin
      SurfaceToGraphic32(Image, Graphic);
    end;
  end;

  SDL_FreeSurface(Image);
end;


//------------------------------------------------------------------------------
procedure TPHXSDLImageFiler.SaveGraphic(Stream: TStream; const Name: String; const Graphic: TPHXGraphic);
begin
end;



//------------------------------------------------------------------------------
function TPHXSDLImageFiler.SupportsReading(const Filename: string): Boolean;
begin
  Result:= True;
end;

//------------------------------------------------------------------------------
function TPHXSDLImageFiler.SupportsWriting(const Filename: string): Boolean;
begin
  Result:= False;
end;

{$ENDREGION}














initialization
  Filer:= TPHXSDLImageFiler.Create;
finalization
  Filer.Free;
end.
