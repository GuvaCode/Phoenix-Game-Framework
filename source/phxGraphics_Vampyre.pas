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
{$I phxConfig.inc}

unit phxGraphics_Vampyre;
{<
  @abstract(Support for loading and saving textures using Vampyre Imaging Library)
  http://imaginglib.sourceforge.net/
  Download vampyre from http://prdownloads.sourceforge.net/imaginglib/imaginglib0264.zip?download
  and put it into the search path
}


interface



uses Types, Classes, SysUtils,
      phxGraphics {$IFDEF PHX_VAMPIRE} ,
      ImagingTypes, Imaging, ImagingClasses, ImagingUtility  {$ENDIF} ;


{$IFDEF PHX_VAMPIRE}
type
// The graphic filer for the Vampyre Imaging Library
//------------------------------------------------------------------------------
TPHXImagingFiler = class(TPHXGraphicFiler)
  private
    procedure LoadImage(const Image: TSingleImage; var   Graphic: TPHXGraphic);
    procedure SaveImage(const Image: TSingleImage; const Graphic: TPHXGraphic);
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
var Filer: TPHXImagingFiler;
 {$ENDIF}
implementation
 {$IFDEF PHX_VAMPIRE}
//------------------------------------------------------------------------------
function GetPhoenixFormat(Format: TImageFormat): TPHXPixelFormat;
begin
  case Format of
    ifA8R8G8B8, ifX8R8G8B8: Result:= pfRGBA;
    ifR8G8B8              : Result:= pfRGB;
    ifGray8               : Result:= pfAlpha;
  else
    Result := pfNone;
  end;
end;


//------------------------------------------------------------------------------
function GetImagingFormat(Format: TPHXPixelFormat): TImageFormat;
begin
  case Format of
    pfRGBA: Result := ifA8R8G8B8;
    pfRGB: Result := ifR8G8B8;
    pfAlpha: Result := ifGray8;
  else
    Result := ifUnknown;
  end;
end;


{$REGION 'TPHXImagingFiler'}

// TPHXImagingFiler
//==============================================================================
constructor TPHXImagingFiler.Create;
begin
  inherited Create;
end;

//------------------------------------------------------------------------------
procedure TPHXImagingFiler.RegisterFileFormats;
begin
  TPHXGraphicFiler.RegisterFileFormat(Self, '*', '');
end;

//------------------------------------------------------------------------------
procedure TPHXImagingFiler.LoadImage(const Image: TSingleImage; var Graphic: TPHXGraphic);
begin
  Graphic.Width := Image.Width;
  Graphic.Height:= Image.Height;
  Graphic.Format:= GetPhoenixFormat(Image.Format);

  if Graphic.Format = pfNone then
  begin
    // No compatible format found -> convert to ARGB8
    Image.Format := ifA8R8G8B8;
    Graphic.Format := pfRGBA;
  end;

  // Convert from RGB to BGR
  Image.SwapChannels(ChannelRed, ChannelBlue);

  Graphic.Size := Image.Size;

  GetMem(Graphic.Pixels, Graphic.Size);

  Move(Image.Bits^, Graphic.Pixels^, Graphic.Size);
end;

//------------------------------------------------------------------------------
procedure TPHXImagingFiler.SaveImage(const Image: TSingleImage; const Graphic: TPHXGraphic);
var Format: TImageFormat;
begin
  Format:= GetImagingFormat(Graphic.Format);

  if Format <> ifUnknown then
  begin
    Image.RecreateImageData(Graphic.Width, Graphic.Height, Format);

    Move(Graphic.Pixels^, Image.Bits^, Graphic.Size);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImagingFiler.LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);
var Img: TSingleImage;
begin
  // Reset graphics
  Graphic.Width := 0;
  Graphic.Height:= 0;
  Graphic.Format:= pfNone;
  Graphic.Pixels:= nil;

  Img:= TSingleImage.CreateFromStream(Stream);
  try
    LoadImage(Img, Graphic);
  finally
    Img.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImagingFiler.SaveGraphic(Stream: TStream; const Name: String; const Graphic: TPHXGraphic);
var Img: TSingleImage;
begin
  Img:= TSingleImage.Create;
  try
    SaveImage(Img, Graphic);

    Img.SaveToStream(GetFileExt(Name), Stream);
  finally
    Img.Free;
  end;
end;
//------------------------------------------------------------------------------
function TPHXImagingFiler.SupportsReading(const Filename: string): Boolean;
begin
  Result:= True;
end;
//------------------------------------------------------------------------------
function TPHXImagingFiler.SupportsWriting(const Filename: string): Boolean;
begin
  Result:= True;
end;
{$ENDREGION}

initialization
  Filer:= TPHXImagingFiler.Create;
finalization
  Filer.Free;
  {$ENDIF}
end.

