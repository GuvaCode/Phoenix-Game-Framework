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
unit phxGraphics_FreeImage;
//< Support for loading and saving textures using FreeImage
//  http://freeimage.sourceforge.net/

interface

{$I phxConfig.inc}

uses Types, Classes, SysUtils,

  phxTypes,
	phxGraphics,

	FreeImage;

type

// The graphic filer for FreeImage
//------------------------------------------------------------------------------
TPHXFreeImageFiler = class(TPHXGraphicFiler)
  private
    IO: FreeImageIO;

    procedure LoadBitmap(const DIB: PFIBITMAP; var   Graphic: TPHXGraphic); overload;
    procedure SaveBitmap(out   DIB: PFIBITMAP; const Graphic: TPHXGraphic); overload;
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
var Filer: TPHXFreeImageFiler;

implementation


{$REGION 'FreeImage Utils'}

//------------------------------------------------------------------------------
function FI_ReadProc(buffer : pointer; size : Cardinal; count : Cardinal; handle : fi_handle) : Cardinal; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(handle);

  Result:= Stream.Read(Buffer^, Size * Count);
end;

//------------------------------------------------------------------------------
function FI_WriteProc(buffer : pointer; size, count : Cardinal; handle : FI_Handle) : Cardinal; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(handle);

  Result:=Stream.Write(Buffer^, Size * Count);
end;

//------------------------------------------------------------------------------
function FI_SeekProc(handle : fi_handle; offset : longint; origin : integer) : integer; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(handle);

  case Origin of
    SEEK_SET: Result:=Stream.Seek(Offset, soFromBeginning);
    SEEK_CUR: Result:=Stream.Seek(Offset, soFromCurrent);
    SEEK_END: Result:=Stream.Seek(Offset, soFromEnd);
    else      Result:=0;
  end;
end;

//------------------------------------------------------------------------------
function FI_TellProc(handle : fi_handle) : LongInt; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(handle);

  Result:=Stream.Position;
end;

{$ENDREGION}

{$REGION 'TPHXFreeImageFiler'}

// TPHXFreeImageFiler
//==============================================================================
constructor TPHXFreeImageFiler.Create;
begin
  inherited Create;

  with IO do
  begin
    read_proc := {$IFDEF FPC}@{$ENDIF}FI_ReadProc;
    write_proc:= {$IFDEF FPC}@{$ENDIF}FI_WriteProc;
    seek_proc := {$IFDEF FPC}@{$ENDIF}FI_SeekProc;
    tell_proc := {$IFDEF FPC}@{$ENDIF}FI_TellProc;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFreeImageFiler.RegisterFileFormats;
begin
  TPHXGraphicFiler.RegisterFileFormat(Self, '.bmp'  , 'Windows Bitmaps');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.cut'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.dds'  , 'Microsoft DirectDraw Surface');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.exr'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.g3'   , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.gif'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.hdr'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.ico'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.iff'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.lbm'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.j2k'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.j2c'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.jng'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.jp2'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.jpg'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.jif'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.jpeg' , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.jpe'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.koa'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.pbm'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.pcd'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.pcx'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.pgm'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.png'  , 'Portable Network Graphics');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.ppm'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.psd'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.ras'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.sgi'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.tga'  , 'Truevision TGA');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.targa', 'Truevision TGA');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.tif'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.tiff' , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.wap'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.wbmp' , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.wbm'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.xbm'  , '');
  TPHXGraphicFiler.RegisterFileFormat(Self, '.xpm'  , '');
end;

//------------------------------------------------------------------------------
procedure TPHXFreeImageFiler.LoadBitmap(const DIB: PFIBITMAP; var Graphic: TPHXGraphic);
var Width   : Cardinal;
var Height  : Cardinal;
var Pitch   : Cardinal;
var BPP     : Cardinal;
var Bits    : PByte;
var SrcPixel: PByteArray;
var DstPixel: PByteArray;
var X,Y     : Integer;
begin
 Width := FreeImage_GetWidth(dib);
 Height:= FreeImage_GetHeight(dib);
 Pitch := FreeImage_GetPitch(dib);

 If FreeImage_GetImageType(dib) <> FIT_BITMAP then
 begin
   Exit;
 end;

 FreeImage_FlipVertical(DIB);

 BPP:= FreeImage_GetBPP(DIB);
 case BPP of
   32:
   // RGBA
   begin
     Graphic.Width  := Width;
     Graphic.Height := Height;
     Graphic.Format := pfRGBA;
     Graphic.Size   := Width * Height * 4;
     Graphic.Pixels := GetMemory(Graphic.Size);

     Bits:= FreeImage_GetBits(dib);

     DstPixel:= Graphic.Pixels;
     for Y:=0 to Height - 1 do
     begin
       SrcPixel:= PByteArray(Bits);
       For X:=0 to Width - 1 do
       begin
         DstPixel^[0]:= SrcPixel^[FI_RGBA_RED   ];
         DstPixel^[1]:= SrcPixel^[FI_RGBA_GREEN ];
         DstPixel^[2]:= SrcPixel^[FI_RGBA_BLUE  ];
         DstPixel^[3]:= SrcPixel^[FI_RGBA_ALPHA ];

         Inc( PByte(DstPixel), 4);
         Inc( PByte(SrcPixel), 4);
       end;
       Inc(Bits, Pitch);
      end;
   end;
   24:
   // RGB
   begin
     Graphic.Width  := Width;
     Graphic.Height := Height;
     Graphic.Format := pfRGB;
     Graphic.Size   := Width * Height * 3;
     Graphic.Pixels := GetMemory(Graphic.Size);

     Bits:= FreeImage_GetBits(dib);

     DstPixel:= Graphic.Pixels;
     for Y:=0 to Height - 1 do
     begin
       SrcPixel:= PByteArray(Bits);
       For X:=0 to Width - 1 do
       begin
         DstPixel^[0]:= SrcPixel^[FI_RGBA_RED   ];
         DstPixel^[1]:= SrcPixel^[FI_RGBA_GREEN ];
         DstPixel^[2]:= SrcPixel^[FI_RGBA_BLUE  ];

         Inc( PByte(DstPixel), 3);
         Inc( PByte(SrcPixel), 3);
       end;
       Inc(Bits, Pitch);
      end;
   end;
   8:
   // Gray scale image
   begin
     Graphic.Width  := Width;
     Graphic.Height := Height;
     Graphic.Format := pfAlpha;
     Graphic.Size   := Width * Height * 1;
     Graphic.Pixels := GetMemory(Graphic.Size);

     Bits:= FreeImage_GetBits(dib);

     DstPixel:= Graphic.Pixels;
     for Y:=0 to Height - 1 do
     begin
       SrcPixel:= PByteArray(Bits);
       For X:=0 to Width - 1 do
       begin
         DstPixel^[0]:= SrcPixel^[0];

         Inc( PByte(DstPixel), 1);
         Inc( PByte(SrcPixel), 1);
       end;
       Inc(Bits, Pitch);
      end;
   end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFreeImageFiler.SaveBitmap(out DIB: PFIBITMAP; const Graphic: TPHXGraphic);
var BytesPerPixel: Integer;
var Pitch        : Cardinal;
var Bits         : PByte;
var SrcPixel     : PByteArray;
var DstPixel     : PByteArray;
var X,Y          : Integer;
begin
  BytesPerPixel:= GetPixelFormatSize(Graphic.Format);

  case BytesPerPixel of
    // RGBA
   //=======================
    4:
    begin
     DIB := FreeImage_Allocate(Graphic.Width, Graphic.Height, 32, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);

     Pitch:= FreeImage_GetPitch(dib);
     Bits := FreeImage_GetBits(dib);

     SrcPixel:= Graphic.Pixels;
     for Y:=0 to Graphic.Height - 1 do
     begin
       DstPixel:= PByteArray(Bits);
       For X:=0 to Graphic.Width - 1 do
       begin
         DstPixel^[FI_RGBA_RED  ]:= SrcPixel^[0];
         DstPixel^[FI_RGBA_GREEN]:= SrcPixel^[1];
         DstPixel^[FI_RGBA_BLUE ]:= SrcPixel^[2];
         DstPixel^[FI_RGBA_ALPHA]:= SrcPixel^[3];

         Inc( PByte(DstPixel), 4);
         Inc( PByte(SrcPixel), 4);
       end;
       Inc(Bits, Pitch);
      end;
      FreeImage_FlipVertical(DIB);
    end;
    // RGB
   //=======================
    3:
    begin
     DIB := FreeImage_Allocate(Graphic.Width, Graphic.Height, 24, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);

     Pitch:= FreeImage_GetPitch(dib);
     Bits := FreeImage_GetBits(dib);

     SrcPixel:= Graphic.Pixels;
     for Y:=0 to Graphic.Height - 1 do
     begin
       DstPixel:= PByteArray(Bits);
       For X:=0 to Graphic.Width - 1 do
       begin
         DstPixel^[FI_RGBA_RED  ]:= SrcPixel^[0];
         DstPixel^[FI_RGBA_GREEN]:= SrcPixel^[1];
         DstPixel^[FI_RGBA_BLUE ]:= SrcPixel^[2];

         Inc( PByte(DstPixel), 3);
         Inc( PByte(SrcPixel), 3);
       end;
       Inc(Bits, Pitch);
      end;
      FreeImage_FlipVertical(DIB);
    end;
    // Luminance etc
    //=======================
    1:
    begin
     DIB := FreeImage_Allocate(Graphic.Width, Graphic.Height, 8, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);

     Pitch:= FreeImage_GetPitch(dib);
     Bits := FreeImage_GetBits(dib);

     SrcPixel:= Graphic.Pixels;
     for Y:=0 to Graphic.Height - 1 do
     begin
       DstPixel:= PByteArray(Bits);
       For X:=0 to Graphic.Width - 1 do
       begin
         DstPixel^[0 ]:= SrcPixel^[0];

         Inc( PByte(DstPixel), 1);
         Inc( PByte(SrcPixel), 1);
       end;
       Inc(Bits, Pitch);
      end;
      FreeImage_FlipVertical(DIB);
    end;
    // Unsupported
    //=======================
    else
    begin
      DIB:= nil;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFreeImageFiler.LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);
var FIF : FREE_IMAGE_FORMAT;
var DIB : PFIBITMAP;
begin
  FreeImage_Initialise(False);

  // Reset graphics
  Graphic.Width := 0;
  Graphic.Height:= 0;
  Graphic.Format:= pfNone;
  Graphic.Pixels:= nil;

  // Get the image format from memory
  FIF:= FreeImage_GetFileTypeFromHandle(@IO, Stream);
  // IF unknown image format get it from the name
  if FIF = FIF_UNKNOWN then
  begin
    FIF:= FreeImage_GetFIFFromFilenameU(PWideChar(Name));
  end;

  if (FIF = FIF_UNKNOWN) or not FreeImage_FIFSupportsReading(FIF) then
  begin
    //TPHXLog.Error('TPHXFreeImageImporter.LoadGraphic', _LINE_, 'Unknown or unsupported texture format: "%s"', [FileName]);

    raise Exception.Create('Unknown or unsupported texture format:' + #13 +Name);
  end;
  //Stream.Seek(0, soFromBeginning);

  DIB:= FreeImage_LoadFromHandle(FIF, @IO, Stream, 0);

  if Assigned(DIB) then
  begin
     LoadBitmap(DIB, Graphic);

     FreeImage_Unload(DIB);
  end else
  begin
    //TPHXLog.Error('TPHXFreeImageImporter.LoadGraphic', _LINE_, 'Failed to load texture "%s".', [FileName]);

    raise Exception.CreateFmt('Failed to load texture "%s".' ,[Name]);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXFreeImageFiler.SaveGraphic(Stream: TStream; const Name: String; const Graphic: TPHXGraphic);
var FIF : FREE_IMAGE_FORMAT;
var DIB : PFIBITMAP;
begin
  FreeImage_Initialise(False);

  // Get the image format
  FIF:= FreeImage_GetFIFFromFilenameU(PWideChar(WideString(Name)));

  if (FIF = FIF_UNKNOWN) then
  begin
    //TPHXLog.Error('TPHXFreeImageImporter.SaveGraphic', _LINE_, 'Unknown texture format: %s', [FileName]);

    raise Exception.Create('Unknown texture format:' + #13 + Name);
  end;

  if not FreeImage_FIFSupportsWriting(FIF) then
  begin
   // TPHXLog.Error('TPHXFreeImageImporter.SaveGraphic', _LINE_, 'Unknown or unsupported texture format: %s', [FileName]);

    raise Exception.Create('Unknown or unsupported texture format:' + #13 + Name);
  end;

  SaveBitmap(DIB, Graphic);

  if Assigned(DIB) then
  begin
    FreeImage_SaveToHandle(FIF, DIB, @io, Stream, 0);

    FreeImage_Unload(DIB);
  end else
  begin
    raise Exception.Create('Texture saving failed:' + #13 + Name);
  end;
end;

//------------------------------------------------------------------------------
function TPHXFreeImageFiler.SupportsReading(const Filename: string): Boolean;
var FIF : FREE_IMAGE_FORMAT;
begin
  FIF:= FreeImage_GetFIFFromFilenameU(PWideChar(WideString(FileName)));

  Result:= FreeImage_FIFSupportsReading(FIF);
end;

//------------------------------------------------------------------------------
function TPHXFreeImageFiler.SupportsWriting(const Filename: string): Boolean;
var FIF : FREE_IMAGE_FORMAT;
begin
  FIF:= FreeImage_GetFIFFromFilenameU(PWideChar(WideString(FileName)));

  Result:= FreeImage_FIFSupportsWriting(FIF);
end;

{$ENDREGION}


initialization
  Filer:= TPHXFreeImageFiler.Create;
finalization
  Filer.Free;
end.
