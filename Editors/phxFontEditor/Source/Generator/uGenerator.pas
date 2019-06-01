unit uGenerator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  XMLDoc, msxmldom, XMLIntf, xmldom, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Types, Classes, Graphics, Math,
//  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
//  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Mask, JvExMask, JvSpin , Math,

  Generics.Collections,


//  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx;

type

//------------------------------------------------------------------------------
TGeneratorLayout = (
  laPacked,
  laGrid
);
//------------------------------------------------------------------------------
TGeneratorQuality = (
  quNone,
  quSmooth,
  quCleartype
);
//------------------------------------------------------------------------------
TGeneratorSettings = class
  private
    FFont      : String;
    FSize      : Integer;
    FStyle     : TFontStyles;
    FCharacters: TList<Cardinal>;

    FForegrond : TColor;
    FBackground: TColor;

    FPadding: Integer;
    FLayout : TGeneratorLayout;
    FQuality: TGeneratorQuality;

    FTextureWidth: Integer;
    FTextureHeight: Integer;

    FOutlineEnabled: Boolean;
    FOutlineWidth: Integer;
    FOutlineColor: TColor;
  public
    constructor Create;

    procedure SaveToXML(Node: IXMLNode);
    procedure LoadFromXML(Node: IXMLNode);

    // The name of the font
    property Font: String read FFont write FFont;
    // The font size
    property Size: Integer read FSize write FSize;
    // The font style
    property Style: TFontStyles read FStyle write FStyle;
    // List of characters to include
    property Characters: TList<Cardinal> read FCharacters;
    // Foreground (text) color
    property Foregrond: TColor read FForegrond write FForegrond;
    // Background color
    property Background: TColor read FBackground write FBackground;


    // Padding between each character
    property Padding: Integer read FPadding write FPadding;
    // Padding between each character
    property Layout: TGeneratorLayout read FLayout write FLayout;
    // Padding between each character
    property Quality: TGeneratorQuality read FQuality write FQuality;

    // Width of the font texture
    property TextureWidth: Integer read FTextureWidth write FTextureWidth;
    // Width of the font texture
    property TextureHeight: Integer read FTextureHeight write FTextureHeight;

    // Enable outline
    property OutlineEnabled: Boolean read FOutlineEnabled write FOutlineEnabled;
    // Width of the outline
    property OutlineWidth: Integer read FOutlineWidth write FOutlineWidth;
    // Color of the outline
    property OutlineColor: TColor read FOutlineColor write FOutlineColor;
  end;



// http://www.devmaster.net/forums/showthread.php?t=7991
//------------------------------------------------------------------------------
TGlyph = record
  // The unicode number of the glyph
  Code: Cardinal;
  // Vertical position in the texture
  X: Integer;
  // Horisontal position in the texture
  Y: Integer;
  // Width of the glyph in pixels
  Width: Integer;
  // Height of the glyph in pixels
  Height: Integer;
  // Offset of the glyph from the quad
  Offset: TPoint;

  Advance: Integer;

  Font: TBitmap;
  Mask: TBitmap;

  Placed: Boolean;
end;
//PCardinalArray = ^TCardinalArray;
//TCardinalArray = array[0..$00FFFFFF] of Cardinal;

//------------------------------------------------------------------------------
TPHXFontGenerator = class
  private
    FSettings: TGeneratorSettings;
    //FFont      : TFont;
    TextMetric  : TTextMetric;

    // List of generated glyphs
    FGlyphs    : TList<TGlyph>;

    FBitmapMask: TBitmap;
    FBitmapFont: TBitmap;

    function CreateWindowsFont(Handle: HDC; Size: Integer = 0): HFONT;

    // Draw outline
    procedure DrawOutline(Handle: HDC; X, Y: Integer; Character: String);

    // Generates a glyph font bitmap for a character
    function GenerateGlyphFont(Character: String; GlyphMetrics: GlyphMetrics; X,Y, Width,Height: Integer): TBitmap; overload;
    // Generates a glyph mask bitmap for a character
    function GenerateGlyphMask(Character: String; GlyphMetrics: GlyphMetrics; X,Y, Width,Height: Integer): TBitmap; overload;


    // Generate the textures
    procedure PlaceGlyphs;
    procedure PlaceGlyphsPacked;
    procedure PlaceGlyphsGrid;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Generate;

    // Update a phoenix font with the result
    procedure UpdateFont(Font: TPHXFont);

    // The font to use for the generation
    property Settings: TGeneratorSettings read FSettings;

    // The text metrix of the generated font
    property Glyphs: TList<TGlyph> read FGlyphs;
    property BitmapFont: TBitmap read FBitmapFont;
    property BitmapMask: TBitmap read FBitmapMask;
  end;




implementation

Uses  Partitions;

// Converts a list of pixels into a 24bit bitmap
//------------------------------------------------------------------------------
function GenerateBitmap(Width, Height: Integer; Pixels: Pointer): TBitmap;
var x,y   : Integer;
var SrcPix: PRGBQuad;
var DstPix: PRGBTriple;
begin
  SrcPix:= PRGBQuad(Pixels);

  Result:= TBitmap.Create;
  Result.Width      := Width;
  Result.Height     := Height;
  Result.PixelFormat:= pf24bit;
  for y := 0 to Result.Height - 1 do
  begin
    DstPix:= Result.ScanLine[y];
    for x := 0 to Result.Width - 1 do
    begin
      DstPix.rgbtRed  := SrcPix^.rgbRed;
      DstPix.rgbtGreen:= SrcPix^.rgbGreen;
      DstPix.rgbtBlue := SrcPix^.rgbBlue;

      Inc(SrcPix);
      Inc(DstPix);
    end;
  end;
end;


{$REGION 'TGeneratorSettings'}

//==============================================================================
constructor TGeneratorSettings.Create;
var Index: Integer;
begin
  FFont      := 'Tahoma';
  FSize      := 16;
  FStyle     := [TFontStyle.fsBold];
  FCharacters:= TList<Cardinal>.Create;

  FPadding:= 1;
  FLayout := laPacked;
  FQuality:= quSmooth;

  FTextureWidth  := 512;
  FTextureHeight := 512;

  FOutlineEnabled:= False;
  FOutlineWidth  := 1;
  FOutlineColor  := clBlack;

  for Index := 32 to 126 do
  begin
    FCharacters.Add(Index);
  end;

  for Index := 65 to 90 do
  begin
   // FCharacters.Add(Index);
  end;

end;

//------------------------------------------------------------------------------
procedure TGeneratorSettings.LoadFromXML(Node: IXMLNode);
begin
end;

//------------------------------------------------------------------------------
procedure TGeneratorSettings.SaveToXML(Node: IXMLNode);
begin
  Node.Attributes['Font']:= Font;
end;


{$ENDREGION}

{$REGION 'TPHXFontGenerator'}

const
 mat2: TMat2 = (eM11: (Fract: 0; Value: 1); eM12: (fract: 0; Value: 0); eM21: (Fract: 0; Value: 0); eM22: (fract: 0; Value: 1));
 IdentityMat: TMat2 = (eM11: (Fract: 0; Value: 1); eM12: (fract: 0; Value: 0); eM21: (Fract: 0; Value: 0); eM22: (fract: 0; Value: 1));

// TPHXFontGenerator
//------------------------------------------------------------------------------
constructor TPHXFontGenerator.Create;
begin
  FSettings  := TGeneratorSettings.Create;

  FBitmapMask:= TBitmap.Create;
  FBitmapFont:= TBitmap.Create;

  FGlyphs    := TList<TGlyph>.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXFontGenerator.Destroy;
begin
  Clear;

  FSettings.Free;

  FBitmapMask.Free;
  FBitmapFont.Free;

  FGlyphs.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXFontGenerator.Clear;
var Index    : Integer;
begin
	for Index := 0 to Glyphs.Count - 1 do
  begin
    Glyphs[Index].Font.Free;
    Glyphs[Index].Mask.Free;
  end;
  Glyphs.Clear;
end;


//------------------------------------------------------------------------------
function TPHXFontGenerator.CreateWindowsFont(Handle: HDC; Size: Integer = 0): HFONT;
var fnWeight: Integer;
var dwItalic: Cardinal;
var dwUnderline: Cardinal;
var dwQuality: Cardinal;
begin
  if Size = 0 then
  begin
    Size:= Settings.Size;
  end;

    // Bold text
  if TFontStyle.fsBold in Settings.Style then
  begin
    fnWeight:= FW_BOLD;
  end else
  begin
    fnWeight:= FW_THIN;
  end;
  // Itallic text
  if TFontStyle.fsItalic in Settings.Style then
  begin
    dwItalic:= 1;
  end else
  begin
    dwItalic:= 0;
  end;
  // Itallic text
  if TFontStyle.fsUnderline in Settings.Style then
  begin
    dwUnderline:= 1;
  end else
  begin
    dwUnderline:= 0;
  end;

  //    NONANTIALIASED_QUALITY, //PROOF_QUALITY, //CLEARTYPE_QUALITY,//ANTIALIASED_QUALITY,//PROOF_QUALITY,
  case Settings.Quality of
    quNone     : dwQuality:= NONANTIALIASED_QUALITY;
    quSmooth   : dwQuality:= ANTIALIASED_QUALITY;
    quCleartype: dwQuality:= CLEARTYPE_QUALITY;
    else         dwQuality:= NONANTIALIASED_QUALITY;
  end;

	Result:= Windows.CreateFont(
		-Size,
		0,0,0,
		fnWeight,
    dwItalic,
    dwUnderline,
    0,
    ANSI_CHARSET,
    OUT_DEFAULT_PRECIS,
    CLIP_DEFAULT_PRECIS,
    dwQuality,

    DEFAULT_PITCH or FF_DONTCARE,
    PChar(Settings.Font)
  );

  SelectObject(Handle, Result);

  SetMapMode(Handle, MM_TEXT);
  SetBkColor(Handle, RGB($00, $00, $00));

  SetTextColor(Handle, RGB($FF, $FF, $FF));
  SetTextAlign(Handle, TA_TOP or TA_LEFT);
end;


(*


When rendering this character, the bitmap contains the character in the region:
top = 0, left = 0, width = nCharW, height = nCharH
The character should be drawn at:
x = draw_x + glyphMetrics.gmptGlyphOrigin.x, y = draw_y + tm.tmAscent - glyphMetrics.gmptGlyphOrigin.y
Where draw_x and draw_y are where you want to draw the character normally
After drawing the character, the cursor should move right from the last draw_x by glyphMetrics.gmCellIncX.

*)

// http://www.gamedev.net/topic/584754-help-with-bitmap-font-rendering/
//------------------------------------------------------------------------------
procedure TPHXFontGenerator.Generate;
var hDC  : Windows.HDC;
var hFont: Windows.HFONT;
//var TextMetric: TTextMetric;
//var CellWidth : Integer;
//var CellHeight: Integer;
var Index     : Integer;
var Character : Integer;

var Glyph      : TGlyph;
var GlyphMetric: TGlyphMetrics;
var GlyphAbc   : TABC;
var GlyphPos   : TPoint;
begin
  Clear;

  hDC:= CreateCompatibleDC(0);
  try
    hFont:= CreateWindowsFont(hDC);

     // http://msdn.microsoft.com/en-us/library/dd145132(v=VS.85).aspx
    // Get the text metrics to get the worst case character size
    GetTextMetrics(hDC, TextMetric);

    //CellWidth  := TextMetric.tmMaxCharWidth;
   // CellHeight := TextMetric.tmHeight;
    // Get the size for each characterr
    for Index := 0 to Settings.Characters.Count - 1 do
    begin
      Character:= Settings.Characters[Index];

      GetGlyphOutline(hDC, Character, GGO_METRICS, GlyphMetric, 0, nil, IdentityMat);
      // http://msdn.microsoft.com/en-us/library/dd144857(VS.85).aspx
      GetCharABCWidths(hDC, Character, Character, GlyphAbc);

      Glyph.Placed   := False;
      Glyph.Code     := Character;
      Glyph.X        := 0;
      Glyph.Y        := 0;
      Glyph.Width    := GlyphMetric.gmBlackBoxX + (2 * Settings.Padding);
      Glyph.Height   := GlyphMetric.gmBlackBoxY + (2 * Settings.Padding);
      Glyph.Advance  := GlyphMetric.gmCellIncX;

      Glyph.Offset.X := Settings.Padding -   GlyphMetric.gmptGlyphOrigin.x;
      Glyph.Offset.Y := Settings.Padding + ( GlyphMetric.gmptGlyphOrigin.y - TextMetric.tmAscent);

      // Offset character by it's origin, so (0, 0) contains the top left pixel
      GlyphPos.X :=  -                        GlyphMetric.gmptGlyphOrigin.x;
      GlyphPos.Y :=  - (TextMetric.tmAscent - GlyphMetric.gmptGlyphOrigin.y);

      if Settings.OutlineEnabled then
      begin
        GlyphPos.X:= GlyphPos.X + Settings.OutlineWidth;
        GlyphPos.Y:= GlyphPos.Y + Settings.OutlineWidth;

        Glyph.Width := Glyph.Width  + ( 2 * Settings.OutlineWidth);
        Glyph.Height:= Glyph.Height + ( 2 * Settings.OutlineWidth);

        Glyph.Offset.X:= Glyph.Offset.X + Settings.OutlineWidth;
        Glyph.Offset.Y:= Glyph.Offset.Y + Settings.OutlineWidth;
      end;

      // Generate the glyph image
      Glyph.Font:= GenerateGlyphFont( Chr(Character) , GlyphMetric, GlyphPos.X, GlyphPos.Y, Glyph.Width, Glyph.Height);
      Glyph.Mask:= GenerateGlyphMask( Chr(Character) , GlyphMetric, GlyphPos.X, GlyphPos.Y, Glyph.Width, Glyph.Height);

      Glyphs.Add(Glyph);
    end;
  finally
    DeleteDC(hDC);
  end;

  PlaceGlyphs;
end;

//------------------------------------------------------------------------------
procedure TPHXFontGenerator.DrawOutline(Handle: HDC; X, Y: Integer; Character: String);
var Index: Integer;
var Rect : TRect;
//var Font :  HFONT;
begin
  for Index := 1 to Settings.OutlineWidth do
  begin
    Rect.Left  := X - Index;
    Rect.Top   := Y - Index;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X + Index;
    Rect.Top   := Y - Index;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X - Index;
    Rect.Top   := Y + Index;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X + Index;
    Rect.Top   := Y + Index;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X - Index;
    Rect.Top   := Y;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X + Index;
    Rect.Top   := Y;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X;
    Rect.Top   := Y + Index;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);

    Rect.Left  := X;
    Rect.Top   := Y + Index;
    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);
  end;
        {
  Font:= CreateWindowsFont(Handle, Settings.Size + 2 * Settings.OutlineWidth);
  try
    Rect.Left  := X - Settings.OutlineWidth;
    Rect.Top   := Y - Settings.OutlineWidth;

    SetTextColor(Handle, ColorToRGB(Settings.OutlineColor));

    DrawText(Handle, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);
  finally
    DeleteObject(Font);
  end;     }

end;

//------------------------------------------------------------------------------
function TPHXFontGenerator.GenerateGlyphFont(Character: String; GlyphMetrics: GlyphMetrics; X,Y, Width, Height: Integer): TBitmap;
var hdc: Windows.HDC;
var bmi: BITMAPINFO;
var hbmp: HBITMAP;
var Brush : HBRUSH;
var hFont:  Windows.HFONT;
var Pixels: Pointer;
var Index : Integer;
var Rect  : TRect;
begin
  ZeroMemory(@bmi.bmiHeader, sizeof(BITMAPINFOHEADER));
  bmi.bmiHeader.biSize        := sizeof(BITMAPINFOHEADER);
  bmi.bmiHeader.biWidth       :=  Width;
  bmi.bmiHeader.biHeight      := -Height;
  bmi.bmiHeader.biPlanes      := 1;
  bmi.bmiHeader.biBitCount    := 32;
  bmi.bmiHeader.biCompression := BI_RGB;

  Rect.Left  := X;
  Rect.Right := X + Width;
  Rect.Top   := Y;
  Rect.Bottom:= Y + Height;

  hDC:= CreateCompatibleDC(0);
  try
    hbmp:= CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, Pixels, 0, 0);
    try
      SelectObject(hdc,hbmp);

      // Fill the whole bitmap
      Brush:= CreateSolidBrush(ColorToRGB(Settings.Background));
      try
         FillRect(hdc, Types.Rect( 0, 0, Width, Height), Brush);
      finally
        DeleteObject (Brush);
      end;

      hFont:= CreateWindowsFont(hDC);
      try
        SetBkColor(hDC, ColorToRGB(Settings.Background)); // Settings.Background
        SetBkMode(hDc, TRANSPARENT);

        if Settings.OutlineEnabled then
        begin
          SetTextColor(hDC, ColorToRGB(Settings.OutlineColor));

          DrawOutline(hDC, X, Y, Character);
        end;

        SetTextColor(hDC, ColorToRGB(Settings.Foregrond));

        Rect.Left  := X;
        Rect.Right := X + Width;
        Rect.Top   := Y;
        Rect.Bottom:= Y + Height;

        DrawText(hDC, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);
      finally
        DeleteObject(hFont);
      end;

      Result:= GenerateBitmap(Width, Height, Pixels);

    finally
      DeleteObject(hbmp);
    end;
  finally
    DeleteDC(hdc);
  end;
end;

//------------------------------------------------------------------------------
function TPHXFontGenerator.GenerateGlyphMask(Character: String; GlyphMetrics: GlyphMetrics; X,Y, Width,  Height: Integer): TBitmap;
var hdc: Windows.HDC;
var bmi: BITMAPINFO;
var hbmp: HBITMAP;
var hFont:  Windows.HFONT;
var Pixels: Pointer;
var Rect  : TRect;
begin
  ZeroMemory(@bmi.bmiHeader, sizeof(BITMAPINFOHEADER));
  bmi.bmiHeader.biSize        := sizeof(BITMAPINFOHEADER);
  bmi.bmiHeader.biWidth       :=  Width;
  bmi.bmiHeader.biHeight      := -Height;
  bmi.bmiHeader.biPlanes      := 1;
  bmi.bmiHeader.biBitCount    := 32;
  bmi.bmiHeader.biCompression := BI_RGB;

  Rect.Left  := X;
  Rect.Right := X + Width;
  Rect.Top   := Y;
  Rect.Bottom:= Y + Height;

  hDC:= CreateCompatibleDC(0);
  try
    hbmp:= CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, Pixels, 0, 0);
    try
      SelectObject(hdc,hbmp);

      // Fill the whole bitmap
      FillRect(hdc, Types.Rect( 0, 0, Width, Height), HBRUSH(GetStockObject(BLACK_BRUSH)));

      hFont:= CreateWindowsFont(hDC);
      try
        SetBkColor(hDC, ColorToRGB(clWhite));
        SetBkMode(hDc, TRANSPARENT);

        SetTextColor(hDC, ColorToRGB(clWhite));

        if Settings.OutlineEnabled then
        begin
          DrawOutline(hDC, X, Y, Character);
        end;

        // Draw the character to the HDC
        DrawText(hDC, PChar(Character), 1, Rect, DT_LEFT or DT_TOP or DT_NOCLIP or DT_NOPREFIX);
      finally
        DeleteObject(hFont);
      end;

      Result:= GenerateBitmap(Width, Height, Pixels);

    finally
      DeleteObject(hbmp);
    end;
  finally
    DeleteDC(hdc);
  end;
end;










//------------------------------------------------------------------------------
procedure TPHXFontGenerator.PlaceGlyphs;
begin
  BitmapFont.PixelFormat:= pf24bit;
  BitmapFont.Width      := Settings.TextureWidth;
  BitmapFont.Height     := Settings.TextureHeight;

  BitmapMask.PixelFormat:= pf24bit;
  BitmapMask.Width      := Settings.TextureWidth;
  BitmapMask.Height     := Settings.TextureHeight;

  BitmapFont.Canvas.Brush.Color:= Settings.Background;
  BitmapFont.Canvas.FillRect( BitmapFont.Canvas.ClipRect );

  BitmapMask.Canvas.Brush.Color:= clBlack;
  BitmapMask.Canvas.FillRect( BitmapMask.Canvas.ClipRect );

  case Settings.Layout of
    laPacked:
    begin
      PlaceGlyphsPacked;
    end;
    laGrid:
    begin
      PlaceGlyphsGrid;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFontGenerator.PlaceGlyphsPacked;
var Partition: TPartition;
var Index    : Integer;
var Glyph    : TGlyph;
var Rect     : TRect;
begin
  Partition:= TPartition.Create(0, 0, Settings.TextureWidth, Settings.TextureHeight, ePackingMode_BestFitFromNWCorner);
  try
    for Index := 0 to Glyphs.Count - 1 do
    begin
      Glyph:= Glyphs[Index];

      if Partition.Insert(Glyph.Width, Glyph.Height, Rect) then
      begin
        Glyph.Placed:= True;
        Glyph.X     := Rect.Left;
        Glyph.Y     := Rect.Top;

        BitmapFont.Canvas.Draw(Rect.Left + Settings.Padding, Rect.Top + Settings.Padding, Glyph.Font);
        BitmapMask.Canvas.Draw(Rect.Left + Settings.Padding, Rect.Top + Settings.Padding, Glyph.Mask);
      end else
      begin
        Glyph.Placed:= False;
        Glyph.X     := 0;
        Glyph.Y     := 0;
      end;

      Glyphs[Index]:= Glyph;
    end;
  finally
    Partition.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFontGenerator.PlaceGlyphsGrid;
var Index: Integer;
var Glyph: TGlyph;
var Grid : TPoint;
var X, Y : Integer;
begin
  Grid.X:= 0;
  Grid.Y:= 0;
  for Index := 0 to Glyphs.Count - 1 do
  begin
    Grid.X:= Max(Grid.X, Glyphs[Index].Width  + Settings.Padding);
    Grid.Y:= Max(Grid.Y, Glyphs[Index].Height + Settings.Padding);
  end;

  X:= 0;
  Y:= 0;
  for Index := 0 to Glyphs.Count - 1 do
  begin
    Glyph:= Glyphs[Index];

    if X + Grid.X >= Settings.TextureWidth then
    begin
      X:= 0;
      Y:= Y + Grid.Y;
    end;
    if Y + Grid.Y >= Settings.TextureHeight then
    begin
      Exit;
    end;
    Glyph.Placed:= True;
    Glyph.X     := X + Settings.Padding;
    Glyph.Y     := Y + Settings.Padding;

    BitmapFont.Canvas.Draw(Glyph.X, Glyph.Y, Glyph.Font);
    BitmapMask.Canvas.Draw(Glyph.X, Glyph.Y, Glyph.Mask);

    Glyphs[Index]:= Glyph;

    X:= X + Grid.X;
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXFontGenerator.UpdateFont(Font: TPHXFont);
var Glyph: TGlyph;
var Char : TPHXCharacter;
begin
  Font.Name := Settings.Font;
  Font.Size := Settings.Size;
  Font.Style:= TPHXFontStyles(Settings.Style);

  Font.Metric.Height    := TextMetric.tmHeight;
  Font.Metric.Offset    := TextMetric.tmDescent;
  Font.Metric.Ascent    := TextMetric.tmAscent;
  Font.Metric.Descent   := TextMetric.tmDescent;

  for Glyph in Glyphs do
  begin
    Char.ID    := Glyph.Code;
    Char.X     := Glyph.X;
    Char.Y     := Glyph.Y;
    Char.Width := Glyph.Width;
    Char.Height:= Glyph.Height;

    Char.Offset.X:= Glyph.Offset.X;
    Char.Offset.Y:= Glyph.Offset.Y;

    Char.Advance:= Glyph.Advance;

    Font.Characters.Add(Char);
  end;


  BitmapToTexture(BitmapFont, BitmapMask, Font.Texture);
end;


{$ENDREGION}




end.
