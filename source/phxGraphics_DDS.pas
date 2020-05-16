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
unit phxGraphics_DDS;
//< Support for loading dds textures

interface

uses Classes, SysUtils,

  phxTypes,
  phxGraphics,
  phxTexture;

type

// Graphic filer for loading DDS images
//------------------------------------------------------------------------------
TPHXDDSFiler = class(TPHXGraphicFiler)
  private
  protected
    // Read a graphic from a stream
    procedure LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);  override;
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

var Filer: TPHXDDSFiler;

implementation

// Microsoft DirectDraw Surface
// http://msdn.microsoft.com/en-us/library/windows/desktop/bb943991%28v=vs.85%29.aspx

//
// This unit is part of the GLScene Project, http://glscene.org
//
{: DDS<p>

   Simple DDS (Microsoft DirectDraw Surface) format support
   for Delphi.<p>

   Note:<br>
   Only the main surface is loaded (mipmap levels, volume
   textures and cubic environment maps are currently
   ignored). Saves out to 24 or 32 bit uncompressed only.<p>

   Supported pixel formats:<ul>
      <li>16 bit (565, 1555, 4444)
      <li>24 bit (888)
      <li>32 bit (8888)
      <li>DXT1 (alpha and color)
      <li>DXT3
      <li>DXT5
   </ul>

   <b>History : </b><font size=-1><ul>
      <li>03/09/04 - SG - Delphi 5 compatibilty fixes (Ivan Lee Herring)
      <li>01/09/04 - SG - Added support for DXTC compression (DXT1, DXT3 and DXT5)
      <li>31/08/04 - SG - Added support for 16 bit textures,
                          it should support most uncompressed files now
      <li>31/08/04 - SG - Creation
   </ul></font>
}



{$REGION 'DDS Constants'}

const
  // DDS
  DDS_MAGIC = $20534444;

  DDSD_CAPS        = $00000001;
  DDSD_HEIGHT      = $00000002;
  DDSD_WIDTH       = $00000004;
  DDSD_PITCH       = $00000008;
  DDSD_PIXELFORMAT = $00001000;
  DDSD_MIPMAPCOUNT = $00020000;
  DDSD_LINEARSIZE  = $00080000;
  DDSD_DEPTH       = $00800000;

  DDPF_ALPHAPIXELS = $00000001;
  DDPF_FOURCC      = $00000004;
  DDPF_RGB         = $00000040;

  DDSCAPS_COMPLEX  = $00000008;
  DDSCAPS_TEXTURE  = $00001000;
  DDSCAPS_MIPMAP   = $00400000;

  DDSCAPS2_CUBEMAP           = $00000200;
  DDSCAPS2_CUBEMAP_POSITIVEX = $00000400;
  DDSCAPS2_CUBEMAP_NEGATIVEX = $00000800;
  DDSCAPS2_CUBEMAP_POSITIVEY = $00001000;
  DDSCAPS2_CUBEMAP_NEGATIVEY = $00002000;
  DDSCAPS2_CUBEMAP_POSITIVEZ = $00004000;
  DDSCAPS2_CUBEMAP_NEGATIVEZ = $00008000;
  DDSCAPS2_VOLUME            = $00200000;


  FOURCC_DXT1 = $31545844; // 'DXT1'
  FOURCC_DXT3 = $33545844; // 'DXT3'
  FOURCC_DXT5 = $35545844; // 'DXT5'

{$ENDREGION}

{$REGION 'DDS Types'}

type
  TDDPIXELFORMAT = record
    dwSize,
    dwFlags,
    dwFourCC,
    dwRGBBitCount,
    dwRBitMask: Cardinal;
    dwGBitMask: Cardinal;
    dwBBitMask: Cardinal;
    dwABitMask: Cardinal;
  end;

  TDDCAPS2 = record
    dwCaps1,
    dwCaps2 : Cardinal;
    Reserved : array[0..1] of Cardinal;
  end;

  TDDSURFACEDESC2 = record
    dwSize,
    dwFlags,
    dwHeight,
    dwWidth,
    dwPitchOrLinearSize,
    dwDepth,
    dwMipMapCount : Cardinal;
    dwReserved1 : array[0..10] of Cardinal;
    ddpfPixelFormat : TDDPIXELFORMAT;
    ddsCaps : TDDCAPS2;
    dwReserved2 : Cardinal;
  end;

  TDDSHeader = record
    Magic : Cardinal;
    SurfaceFormat : TDDSURFACEDESC2;
  end;

  PFOURCC = ^TFOURCC;
  TFOURCC = array[0..3] of AnsiChar;

  EDDSException = class(Exception);

{$ENDREGION}

{$REGION 'DDS Decoding'}

// http://en.wikipedia.org/wiki/S3_Texture_Compression

// DecodeColor565
//------------------------------------------------------------------------------
procedure DecodeColor565(col : Word; var r,g,b : Byte);
begin
   r:=col and $1F;
   g:=(col shr 5) and $3F;
   b:=(col shr 11) and $1F;
end;

// DecodeDXT1toBitmap32
//------------------------------------------------------------------------------
procedure DecodeDXT1toBitmap32(encData, decData : PByteArray; w,h : Integer; var trans : Boolean);
var
   x,y,i,j,k,select : Integer;
   col0, col1 : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bitmask : Cardinal;
   temp : PByte;
   r0,g0,b0,r1,g1,b1 : Byte;
begin
   trans:=False;

   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PByte(encData);
   for y:=0 to (h div 4)-1 do
   begin
      for x:=0 to (w div 4)-1 do
      begin
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;

         if col0>col1 then
         begin
            colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
            colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
            colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
            colors[2][3]:=$FF;
            colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
            colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
            colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
            colors[3][3]:=$FF;
         end else
         begin
            trans:=True;
            colors[2][0]:=(colors[0][0]+colors[1][0]) div 2;
            colors[2][1]:=(colors[0][1]+colors[1][1]) div 2;
            colors[2][2]:=(colors[0][2]+colors[1][2]) div 2;
            colors[2][3]:=$FF;
            colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
            colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
            colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
            colors[3][3]:=0;
         end;

         k:=0;
         for j:=0 to 3 do
         begin
            for i:=0 to 3 do
            begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);

               if ((4*x+i)<w) and ((4*y+j)<h) then
               begin
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               end;
               Inc(k);
            end;
         end;
      end;
   end;
end;

// DecodeDXT3toBitmap32
//------------------------------------------------------------------------------
procedure DecodeDXT3toBitmap32(encData, decData : PByteArray; w,h : Integer);
var
   x,y,i,j,k,select : Integer;
   col0, col1, wrd : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bitmask, offset : Cardinal;
   temp : PByte;
   r0,g0,b0,r1,g1,b1 : Byte;
   alpha : array[0..3] of Word;
begin
   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PByte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         alpha[0]:=PWord(temp)^;    Inc(temp, 2);
         alpha[1]:=PWord(temp)^;    Inc(temp, 2);
         alpha[2]:=PWord(temp)^;    Inc(temp, 2);
         alpha[3]:=PWord(temp)^;    Inc(temp, 2);
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;
         colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
         colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
         colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
         colors[2][3]:=$FF;
         colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
         colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
         colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
         colors[3][3]:=$FF;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

         for j:=0 to 3 do begin
            wrd:=alpha[j];
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData^[offset]:=wrd and $0F;
                  decData^[offset]:=decData^[offset] or (decData^[offset] shl 4);
               end;
               wrd:=wrd shr 4;
            end;
         end;

      end;
   end;
end;

// DecodeDXT5toBitmap32
//------------------------------------------------------------------------------
procedure DecodeDXT5toBitmap32(encData, decData : PByteArray; w,h : Integer);
var
   x,y,i,j,k,select : Integer;
   col0, col1 : Word;
   colors : array[0..3] of array[0..3] of Byte;
   bits, bitmask, offset : Cardinal;
   temp, alphamask : PByte;
   r0,g0,b0,r1,g1,b1 : Byte;
   alphas : array[0..7] of Byte;
begin
   if not (Assigned(encData) and Assigned(decData)) then exit;

   temp:=PByte(encData);
   for y:=0 to (h div 4)-1 do begin
      for x:=0 to (w div 4)-1 do begin
         alphas[0]:=temp^; Inc(temp);
         alphas[1]:=temp^; Inc(temp);
         alphamask:=temp; Inc(temp, 6);
         col0:=PWord(temp)^;        Inc(temp, 2);
         col1:=PWord(temp)^;        Inc(temp, 2);
         bitmask:=PCardinal(temp)^; Inc(temp, 4);

         DecodeColor565(col0,r0,g0,b0);
         DecodeColor565(col1,r1,g1,b1);

         colors[0][0]:=r0 shl 3;
         colors[0][1]:=g0 shl 2;
         colors[0][2]:=b0 shl 3;
         colors[0][3]:=$FF;
         colors[1][0]:=r1 shl 3;
         colors[1][1]:=g1 shl 2;
         colors[1][2]:=b1 shl 3;
         colors[1][3]:=$FF;
         colors[2][0]:=(2*colors[0][0]+colors[1][0]+1) div 3;
         colors[2][1]:=(2*colors[0][1]+colors[1][1]+1) div 3;
         colors[2][2]:=(2*colors[0][2]+colors[1][2]+1) div 3;
         colors[2][3]:=$FF;
         colors[3][0]:=(colors[0][0]+2*colors[1][0]+1) div 3;
         colors[3][1]:=(colors[0][1]+2*colors[1][1]+1) div 3;
         colors[3][2]:=(colors[0][2]+2*colors[1][2]+1) div 3;
         colors[3][3]:=$FF;

         k:=0;
         for j:=0 to 3 do begin
            for i:=0 to 3 do begin
               select:=(bitmask and (3 shl (k*2))) shr (k*2);
               if ((4*x+i)<w) and ((4*y+j)<h) then
                  PCardinal(@decData[((4*y+j)*w+(4*x+i))*4])^:=Cardinal(colors[select]);
               Inc(k);
            end;
         end;

         if (alphas[0] > alphas[1]) then begin
            alphas[2]:=(6*alphas[0]+1*alphas[1]+3) div 7;
            alphas[3]:=(5*alphas[0]+2*alphas[1]+3) div 7;
            alphas[4]:=(4*alphas[0]+3*alphas[1]+3) div 7;
            alphas[5]:=(3*alphas[0]+4*alphas[1]+3) div 7;
            alphas[6]:=(2*alphas[0]+5*alphas[1]+3) div 7;
            alphas[7]:=(1*alphas[0]+6*alphas[1]+3) div 7;
         end else begin
            alphas[2]:=(4*alphas[0]+1*alphas[1]+2) div 5;
            alphas[3]:=(3*alphas[0]+2*alphas[1]+2) div 5;
            alphas[4]:=(2*alphas[0]+3*alphas[1]+2) div 5;
            alphas[5]:=(1*alphas[0]+4*alphas[1]+2) div 5;
            alphas[6]:=0;
            alphas[7]:=$FF;
         end;

         bits:=PCardinal(alphamask)^;
         for j:=0 to 1 do begin
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData^[Offset]:=alphas[bits and 7];
               end;
               bits:=bits shr 3;
            end;
         end;

         Inc(alphamask, 3);
         bits:=PCardinal(alphamask)^;
         for j:=2 to 3 do begin
            for i:=0 to 3 do begin
               if (((4*x+i)<w) and ((4*y+j)<h)) then begin
                  offset:=((4*y+j)*w+(4*x+i))*4+3;
                  decData^[offset]:=alphas[bits and 7];
               end;
               bits:=bits shr 3;
            end;
         end;

      end;
   end;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
function GetBitsFromMask(Mask : Cardinal) : Byte;
var i, temp : Integer;
begin
  if Mask=0 then
  begin
    Result:=0;
    Exit;
  end;
  temp:=Mask;
  for i:=0 to 31 do
  begin
    if (Temp and 1) = 1 then break;
    temp:=temp shr 1;
  end;
  Result:=i;
end;

//------------------------------------------------------------------------------
procedure LoadCompressed(Stream: TStream; var Graphic: TPHXGraphic; const Header: TDDSHeader);
var buf, decoded : PByteArray;
var trans : Boolean;
var Row: Integer;
begin
  trans:= False;

  with Header.SurfaceFormat do
  begin
    GetMem(buf, dwPitchOrLinearSize);
    GetMem(decoded, dwWidth * dwHeight * 4);

    Stream.Read(buf[0], dwPitchOrLinearSize);
    try
      case ddpfPixelFormat.dwFourCC of
        FOURCC_DXT1:
        begin
          DecodeDXT1toBitmap32(buf, decoded, dwWidth, dwHeight, trans);
          // Transparent:=trans;
        end;
        FOURCC_DXT3:
        begin
          DecodeDXT3toBitmap32(buf, decoded, dwWidth, dwHeight);
          //Transparent:=True;
        end;
        FOURCC_DXT5:
        begin
          DecodeDXT5toBitmap32(buf, decoded, dwWidth, dwHeight);
          //Transparent:=True;
        end;
        else
        begin
          raise EDDSException.CreateFmt('Unsupported compression type: %s', [PFOURCC(@ddpfPixelFormat.dwFourCC)^]);
        end;
      end;

      for Row:=0 to dwHeight-1 do
      begin
        System.Move(decoded[4*Row* Integer(dwWidth)], PCardinal(Graphic.ScanLine(Row))^, dwWidth * 4);
      end;

    finally
      FreeMem(decoded);
      FreeMem(buf);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure LoadUncompressed(Stream: TStream; var Graphic: TPHXGraphic; const Header: TDDSHeader);
var rowSize, ddsPixelSize, imgPixelSize : Integer;
var buf : PByteArray;
var col : PCardinal;
var RedShift  , RedMult : Byte;
var GreenShift, GreenMult : Byte;
var BlueShift , BlueMult : Byte;
var AlphaShift, AlphaMult : Byte;
var x,y: Integer;
begin
  imgPixelSize:=4;
  ddsPixelSize:=(header.SurfaceFormat.ddpfPixelFormat.dwRGBBitCount div 8);
  rowSize:=ddsPixelSize*Integer(header.SurfaceFormat.dwWidth);

  RedShift  := GetBitsFromMask(header.SurfaceFormat.ddpfPixelFormat.dwRBitMask);
  GreenShift:= GetBitsFromMask(header.SurfaceFormat.ddpfPixelFormat.dwGBitMask);
  BlueShift := GetBitsFromMask(header.SurfaceFormat.ddpfPixelFormat.dwBBitMask);
  AlphaShift:= GetBitsFromMask(header.SurfaceFormat.ddpfPixelFormat.dwABitMask);

  //   if Transparent then
  //    AlphaShift:=GetBitsFromMask(ddpfPixelFormat.dwRGBAlphaBitMask)
  // else
  //   AlphaShift:=0;


  if (header.SurfaceFormat.ddpfPixelFormat.dwRBitMask shr RedShift)>0 then
  begin
    RedMult:= 255 div (header.SurfaceFormat.ddpfPixelFormat.dwRBitMask shr RedShift);
  end else
  begin
    RedMult:=1;
  end;

  if (header.SurfaceFormat.ddpfPixelFormat.dwGBitMask shr GreenShift)>0 then
  begin
    GreenMult:= 255 div (header.SurfaceFormat.ddpfPixelFormat.dwGBitMask shr GreenShift);
  end else
  begin
    GreenMult:=1;
  end;

  if (header.SurfaceFormat.ddpfPixelFormat.dwBBitMask shr BlueShift)>0 then
  begin
    BlueMult:= 255 div (header.SurfaceFormat.ddpfPixelFormat.dwBBitMask shr BlueShift);
  end else
  begin
    BlueMult:=1;
  end;

  if (header.SurfaceFormat.ddpfPixelFormat.dwABitMask shr AlphaShift)>0 then
  begin
    AlphaMult:=255 div (Header.SurfaceFormat.ddpfPixelFormat.dwABitMask shr AlphaShift);
  end else
  begin
    AlphaMult:=1;
  end;

  GetMem(buf, rowSize);

  for y:=0 to Header.SurfaceFormat.dwHeight-1 do
  begin
    Stream.Read(buf[0], rowSize);
    for x:=0 to Header.SurfaceFormat.dwWidth-1 do
    begin
       col:=@buf[ddsPixelSize*y];

       case Graphic.Format of
         pfRGB:
         begin
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+0]:= BlueMult  * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwBBitMask) shr BlueShift;
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+1]:= GreenMult * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwGBitMask) shr GreenShift;
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+2]:= RedMult   * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwRBitMask) shr RedShift;
         end;
         pfRGBA:
         begin
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+0]:= BlueMult  * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwBBitMask) shr BlueShift;
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+1]:= GreenMult * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwGBitMask) shr GreenShift;
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+2]:= RedMult   * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwRBitMask) shr RedShift;
           PByteArray(Graphic.ScanLine(y))^[imgPixelSize*x+3]:= AlphaMult * (col^ and Header.SurfaceFormat.ddpfPixelFormat.dwABitMask) shr AlphaShift;
        end;
       end;

      // if Transparent then begin

       //if Transparent then
       //   PByteArray(ScanLine[j])^[imgPixelSize*i+3]:=
        //     AlphaMult*(col^ and ddpfPixelFormat.dwRGBAlphaBitMask) shr AlphaShift;            end;
    end;
  end;

  FreeMem(buf);
end;


{$REGION 'TPHXDDSFiler'}

// TPHXDDSFiler
//==============================================================================
constructor TPHXDDSFiler.Create;
begin
  inherited Create;
end;

//------------------------------------------------------------------------------
procedure TPHXDDSFiler.RegisterFileFormats;
begin
  TPHXGraphicFiler.RegisterFileFormat(Self, '.dds', 'Microsoft DirectDraw Surface');
end;

//------------------------------------------------------------------------------
procedure TPHXDDSFiler.LoadGraphic(Stream: TStream; const Name: String; out Graphic: TPHXGraphic);
var Header: TDDSHeader;
var Format: TPHXPixelFormat;
begin
  Stream.Read(Header, Sizeof(TDDSHeader));

  // Check header
  if Header.Magic <> DDS_MAGIC then
  begin
    raise EDDSException.Create('Invalid DDS file');
  end;

  Format:= pfRGB;
  with header.SurfaceFormat do
  begin

    if (ddsCaps.dwCaps1 and DDSCAPS_TEXTURE) = 0 then
    begin
      raise EDDSException.Create('Unsupported DDSCAPS settings');
    end;

    if (ddpfPixelFormat.dwFlags and DDPF_ALPHAPIXELS) > 0 then
    begin
      Format:= pfRGBA;
    end;

    Graphic.Resize(dwWidth, dwHeight, Format);

    if (ddpfPixelFormat.dwFlags and DDPF_FOURCC) > 0 then
    begin
      LoadCompressed(Stream, Graphic, Header);
    end else
    begin
      LoadUncompressed(Stream, Graphic, Header);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXDDSFiler.SupportsReading(const Filename: string): Boolean;
begin
  Result:= SameText( ExtractFileExt(FileName), '.dds');
end;

//------------------------------------------------------------------------------
function TPHXDDSFiler.SupportsWriting(const Filename: string): Boolean;
begin
  Result:= False;
end;

{$ENDREGION}



(*
// SaveToStream
//
procedure TDDSImage.SaveToStream(stream : TStream);
var
   magic : TFOURCC;
   header : TDDSHeader;
   i, rowSize : Integer;
begin
   FillChar(header, SizeOf(TDDSHeader), 0);
   magic:='DDS ';
   header.magic:=Cardinal(magic);
   with header.SurfaceFormat do begin
      dwSize:=124;
      dwFlags:=DDSD_CAPS +
               DDSD_PIXELFORMAT +
               DDSD_WIDTH +
               DDSD_HEIGHT +
               DDSD_PITCH;
      dwWidth:=Width;
      dwHeight:=Height;
      case PixelFormat of
         {$IFDEF MSWINDOWS}
         glpf24bit : begin
            ddpfPixelFormat.dwFlags:=DDPF_RGB;
            ddpfPixelFormat.dwRGBBitCount:=24;
            ddpfPixelFormat.dwRBitMask:=$00FF0000;
            ddpfPixelFormat.dwGBitMask:=$0000FF00;
            ddpfPixelFormat.dwBBitMask:=$000000FF;
         end;
         {$ENDIF}
         glpf32bit : begin
            ddpfPixelFormat.dwFlags:=DDPF_RGB;
            ddpfPixelFormat.dwRGBBitCount:=32;
            ddpfPixelFormat.dwRBitMask:=$00FF0000;
            ddpfPixelFormat.dwGBitMask:=$0000FF00;
            ddpfPixelFormat.dwBBitMask:=$000000FF;
            if Transparent then begin
               ddpfPixelFormat.dwFlags:=ddpfPixelFormat.dwFlags + DDPF_ALPHAPIXELS;
               ddpfPixelFormat.dwRGBAlphaBitMask:=$FF000000;
            end;
         end;
      else
         raise EDDSException.Create('Unsupported pixel format format');
      end;
      rowSize:=(ddpfPixelFormat.dwRGBBitCount div 8)*dwWidth;
      dwPitchOrLinearSize:=dwHeight*Cardinal(rowSize);
      ddsCaps.dwCaps1:=DDSCAPS_TEXTURE;
      stream.Write(header, SizeOf(TDDSHeader));
      for i:=0 to Height-1 do
         stream.Write(ScanLine[i]^, rowSize);
   end;
end;
*)



initialization
  Filer:= TPHXDDSFiler.Create;
finalization
  Filer.Free;
end.

