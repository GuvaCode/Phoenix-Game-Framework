unit uGenerator;

interface

uses      Partitions,
 Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Mask , Math,

  Generics.Collections,

  xmldom, XMLIntf, msxmldom, XMLDoc,

//  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx;

type

{$REGION 'TPHXFontGenerator'}

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
  //
  Origin: TPoint;

  Advance: Integer;

  Image    : TBitmap;
end;
	{
	char ascii;
	int width;
	int xoffset;
	int yoffset;
	int hpitch;
	int vpitch;
	unsigned char* data;
	int x1;
	int y1;
	int x2;
	int y2;
}

//------------------------------------------------------------------------------
TGeneratorSettings = class
  private
    FFont: String;
    FForegrond: TColor;
    FBackground: TColor;
    FSize: Integer;
    FStyle: TFontStyles;
    FCharacters: TList<Cardinal>;
    FPadding: Integer;
    FWidth: Integer;
    FHeight: Integer;
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
    property Padding: Integer read FPadding write FPadding;

    // Foreground (text) color
    property Foregrond: TColor read FForegrond write FForegrond;
    // Background color
    property Background: TColor read FBackground write FBackground;
    // Width of the font texture
    property Width: Integer read FWidth write FWidth;
    // Width of the font texture
    property Height: Integer read FHeight write FHeight;

    // List of characters to include
    property Characters: TList<Cardinal> read FCharacters;
  end;


//------------------------------------------------------------------------------
TPHXFontGenerator = class
  private
    FSettings: TGeneratorSettings;
    //FFont      : TFont;
    TextMetric  : TTextMetric;

    FGlyphs    : TList<TGlyph>;

    FBitmapMask: TBitmap;
    FBitmapFont: TBitmap;

    function CreateWindowsFont(hDC: Windows.HDC): HFONT;


    function GenerateGlyph(Character: String; GlyphMetrics: GlyphMetrics; GlyphRect: TRect; Width,Height: Integer): TBitmap; overload;
    // Generate the textures
    procedure GenerateTextures;
   public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure GenerateFont;

    // The font to use for the generation
    Property Settings: TGeneratorSettings read FSettings;

    // The text metrix of the generated font
    property Glyphs: TList<TGlyph> read FGlyphs;
    property BitmapFont: TBitmap read FBitmapFont;
    property BitmapMask: TBitmap read FBitmapMask;
  end;

//function GetTextMetric(const Font: TFont): TTextMetric;

// Finds the width and height of the specified character of the specified font
//function GetGlyphSize(const Character: Cardinal; const Font: TFont): TSize;

//
//function GetGlyphSupported(const Character: Cardinal; const Font: TFont): Boolean;




{$ENDREGION}
type


TFrmGenerator = class(TForm)
    GroupBox3: TGroupBox;
    Label3: TLabel;
    Label9: TLabel;
    edBackground: TShape;
    edForegrond: TShape;
    Label1: TLabel;
    edFont: TComboBox;
    edBold: TCheckBox;
    edItalic: TCheckBox;
    ColorDialog1: TColorDialog;
    Label2: TLabel;
    btnGenerate: TButton;
    Image1: TImage;
    edSize: TSpinEdit;
    Label4: TLabel;
    Image2: TImage;
    btnOk: TButton;
    btnCancel: TButton;
    GroupBox1: TGroupBox;
    Image3: TImage;
    edPadding: TSpinEdit;
    SpeedButton1: TSpeedButton;
    edWidth: TSpinEdit;
    edHeight: TSpinEdit;
    edUnderline: TCheckBox;
    ComboBox1: TComboBox;
    Image4: TImage;
    edBlurEnable: TCheckBox;
    edDropshadow: TCheckBox;
    edShadowSize: TSpinEdit;
    procedure SelectColor(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure edBoldClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure edSizeChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    FGenerator: TPHXFontGenerator;
    procedure GenerateShadow;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;

    procedure UpdateFont(Font: TPHXFont);
  end;

var
  FrmGenerator: TFrmGenerator;

implementation

uses uGeneratorRange;

{$R *.dfm}
var
  CodePages: TList<Cardinal>;

function EnumCodePagesProc(lpCodePageString: LPTSTR): BOOL; stdcall;
var CodePage: Cardinal;
begin
  CodePage:= StrToIntDef(lpCodePageString, 0 );

  CodePages.Add(CodePage);

  Result := True;
end;


type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..$00FFFFFF] of Cardinal;

procedure BmpGBlur(Bmp: TBitmap; radius: Single);
Type
  TRGB      = Packed Record b, g, r: Byte End;
  TRGBs     = Packed Record b, g, r: Single End;
  TRGBArray = Array[0..0] of TRGB;
Var
  MatrixRadius: Byte;
  Matrix : Array[-100..100] of Single;

  Procedure CalculateMatrix;
  Var x: Integer; Divisor: Single;
  Begin
    radius:=radius+1; // der mittel/nullpunkt muss mitgerechnet werden
    MatrixRadius:=Trunc(radius);
    If Frac(radius)=0 Then Dec(MatrixRadius);
    Divisor:=0;
    For x:=-MatrixRadius To MatrixRadius Do Begin
      Matrix[x]:=radius-abs(x);
      Divisor:=Divisor+Matrix[x];
    End;
    For x:=-MatrixRadius To MatrixRadius Do
      Matrix[x]:=Matrix[x]/Divisor;
  End;

Var
  BmpSL              : ^TRGBArray;
  BmpRGB             : ^TRGB;
  BmpCopy            : Array of Array of TRGBs;
  BmpCopyRGB         : ^TRGBs;
  R, G, B            : Single;
  BmpWidth, BmpHeight: Integer;
  x, y, mx           : Integer;
Begin
  Bmp.PixelFormat:=pf24bit;
  If radius<=0 Then radius:=1 Else If radius>99 Then radius:=99; // radius bereich 0 < radius < 99
  CalculateMatrix;
  BmpWidth:=Bmp.Width;
  BmpHeight:=Bmp.Height;
  SetLength(BmpCopy,BmpHeight,BmpWidth);
  // Alle Bildpunkte ins BmpCopy-Array schreiben und gleichzeitig HORIZONTAL blurren
  For y:=0 To Pred(BmpHeight) Do Begin
    BmpSL:=Bmp.Scanline[y];
    BmpCopyRGB:=@BmpCopy[y,0];
    For x:=0 to Pred(BmpWidth) Do Begin
      R:=0; G:=0; B:=0;
      For Mx:=-MatrixRadius To MatrixRadius Do Begin
        If x+mx<0 Then
          BmpRGB:=@BmpSL^[0]              // erster Pixel
        Else If x+mx>=BmpWidth Then
          BmpRGB:=@BmpSL^[Pred(BmpWidth)] // letzter Pixel
        Else
          BmpRGB:=@BmpSL^[x+mx];
        B:=B+BmpRGB^.b*Matrix[mx];
        G:=G+BmpRGB^.g*Matrix[mx];
        R:=R+BmpRGB^.r*Matrix[mx];
      End;
      BmpCopyRGB^.b:=B;  // Farbwerte werden im Typ Single zwischengespeichert !
      BmpCopyRGB^.g:=G;
      BmpCopyRGB^.r:=R;
      Inc(BmpCopyRGB);
    End;
  End;
  // Alle Bildpunkte zurück ins Bmp-Bitmap schreiben und gleichzeitig VERTIKAL blurren
  For y:=0 To Pred(BmpHeight) Do Begin
    BmpRGB:=Bmp.ScanLine[y];
    For x:=0 to Pred(BmpWidth) Do Begin
      R:=0; G:=0; B:=0;
      For mx:=-MatrixRadius To MatrixRadius Do Begin
        If y+mx<=0 Then
          BmpCopyRGB:=@BmpCopy[0,x]                // erster Pixel
        Else If y+mx>=BmpHeight Then
          BmpCopyRGB:=@BmpCopy[Pred(BmpHeight),x]  // letzter Pixel
        Else
          BmpCopyRGB:=@BmpCopy[y+mx,x];
        B:=B+BmpCopyRGB^.b*Matrix[mx];
        G:=G+BmpCopyRGB^.g*Matrix[mx];
        R:=R+BmpCopyRGB^.r*Matrix[mx];
      End;
      BmpRGB^.b:=Round(B);
      BmpRGB^.g:=Round(G);
      BmpRGB^.r:=Round(R);
      Inc(BmpRGB);
    End;
  End;
End;

{$REGION 'TGeneratorSettings'}

//==============================================================================
constructor TGeneratorSettings.Create;
var Index: Integer;
begin
  FFont:= 'Tahoma';
  FSize:= 16;
  FStyle:= [TFontStyle.fsBold];
  FCharacters:= TList<Cardinal>.Create;

  FWidth:= 512;
  FHeight:= 512;

  FCharacters.Clear;
  for Index := 32 to 126 do
  begin
    FCharacters.Add(Index);
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
    Glyphs[Index].Image.Free;
  end;
  Glyphs.Clear;
end;


//------------------------------------------------------------------------------
function TPHXFontGenerator.CreateWindowsFont(hDC: Windows.HDC): HFONT;
var fnWeight: Integer;
var dwItalic: Cardinal;
var dwUnderline: Cardinal;
begin
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

	Result:= Windows.CreateFont(
		-Settings.Size,
		0,0,0,
		fnWeight,
    dwItalic,
    dwUnderline,
    0,
    ANSI_CHARSET,
    OUT_DEFAULT_PRECIS,
    CLIP_DEFAULT_PRECIS,
    NONANTIALIASED_QUALITY, //PROOF_QUALITY, //CLEARTYPE_QUALITY,//ANTIALIASED_QUALITY,//PROOF_QUALITY,

    DEFAULT_PITCH or FF_DONTCARE,
    PChar(Settings.Font)
  );

  SelectObject(hDC, Result);

  SetMapMode(hDC, MM_TEXT);
  SetBkColor(hDC, RGB($00, $00, $00));

  SetTextColor(hDC, RGB($FF, $FF, $FF));
  SetTextAlign(hDC, TA_TOP or TA_LEFT);
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
procedure TPHXFontGenerator.GenerateFont;
var hDC  : Windows.HDC;
var hFont: Windows.HFONT;
//var TextMetric: TTextMetric;
var CellWidth : Integer;
var CellHeight: Integer;
var Index     : Integer;
var Character : Integer;

var Glyph       : TGlyph;
var GlyphMetrics: TGlyphMetrics;
var GlyphAbc    : TABC;
var GlyphRect   : TRect;
begin
  Clear;

  hDC:= CreateCompatibleDC(0);
  try
    hFont:= CreateWindowsFont(hDC);

     // http://msdn.microsoft.com/en-us/library/dd145132(v=VS.85).aspx
    // Get the text metrics to get the worst case character size
    GetTextMetrics(hDC, TextMetric);

    CellWidth  := TextMetric.tmMaxCharWidth;
    CellHeight := TextMetric.tmHeight;
    // Get the size for each characterr
    for Index := 0 to Settings.Characters.Count - 1 do
    begin
      Character:= Settings.Characters[Index];

      GetGlyphOutline(hDC, Character, GGO_METRICS, GlyphMetrics, 0, nil, IdentityMat);
      // http://msdn.microsoft.com/en-us/library/dd144857(VS.85).aspx
      GetCharABCWidths(hDC, Character, Character, GlyphAbc);

      // Determine where to draw character (Aim for (0, 0))
      GlyphRect.left  := 0;
      GlyphRect.right := CellWidth;
      GlyphRect.top   := 0;
      GlyphRect.bottom:= CellHeight;
      // Offset character by it's origin, so (0, 0) contains the top left pixel
      GlyphRect.Left := GlyphRect.Left - glyphMetrics.gmptGlyphOrigin.x;
      GlyphRect.Top  := GlyphRect.Top  - (TextMetric.tmAscent - glyphMetrics.gmptGlyphOrigin.y);

      Glyph.Code     := Character;
      Glyph.X        := 0;
      Glyph.Y        := 0;
      Glyph.Width    := GlyphMetrics.gmBlackBoxX + 2 * Settings.Padding;
      Glyph.Height   := GlyphMetrics.gmBlackBoxY + 2 * Settings.Padding;
      Glyph.Advance  := glyphMetrics.gmCellIncX;

      Glyph.Origin.X := Settings.Padding + glyphMetrics.gmptGlyphOrigin.x;
      Glyph.Origin.Y := Settings.Padding + TextMetric.tmAscent - glyphMetrics.gmptGlyphOrigin.y;

      Glyph.Image    := GenerateGlyph( Chr(Character) , GlyphMetrics, GlyphRect, GlyphMetrics.gmBlackBoxX, GlyphMetrics.gmBlackBoxY);

      Glyphs.Add(Glyph);
    end;
  finally
    DeleteDC(hDC);
  end;

  GenerateTextures;
end;

//------------------------------------------------------------------------------
function TPHXFontGenerator.GenerateGlyph(Character: String; GlyphMetrics: GlyphMetrics; GlyphRect: TRect; Width,  Height: Integer): TBitmap;

var hdc: Windows.HDC;
var bmi: BITMAPINFO;
var hbmp: HBITMAP;
var hFont:  Windows.HFONT;
var xpos : Integer;
var ypos : Integer;
var x,y  : Integer;
var SrcPix  : Pointer;
var DstPix  : PRGBTriple;
var Text: String;
begin
  ZeroMemory(@bmi.bmiHeader, sizeof(BITMAPINFOHEADER));
  bmi.bmiHeader.biSize        := sizeof(BITMAPINFOHEADER);
  bmi.bmiHeader.biWidth       :=  Width;
  bmi.bmiHeader.biHeight      := -Height;
  bmi.bmiHeader.biPlanes      := 1;
  bmi.bmiHeader.biBitCount    := 32;
  bmi.bmiHeader.biCompression := BI_RGB;

  hDC:= CreateCompatibleDC(0);
  try
    hbmp:= CreateDIBSection(hdc, &bmi, DIB_RGB_COLORS, SrcPix, 0, 0);
    SelectObject(hdc,hbmp);

    // Fill the whole bitmap
    FillRect(hdc, Rect( 0, 0, Width, Height), HBRUSH(GetStockObject(GRAY_BRUSH)));
    FillRect(hdc, Rect( 0, 0, Width, Height), HBRUSH(GetStockObject(BLACK_BRUSH)));

    hFont:= CreateWindowsFont(hDC);
    try
      // Draw the character to the HDC
      DrawText(hDC, PChar(Character), 1, GlyphRect, DT_LEFT or DT_NOCLIP or DT_NOPREFIX);
      //DrawText(hDC, PChar(Character), 1, GlyphRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX);
    finally
      DeleteObject(hFont);
    end;


   Result:= TBitmap.Create;
    Result.Width      := Width;
    Result.Height     := Height;
    Result.PixelFormat:= pf24bit;
    for y := 0 to Result.Height - 1 do
    begin
      DstPix:= Result.ScanLine[y];
      for x := 0 to Result.Width - 1 do
      begin
        DstPix.rgbtRed  := GetRValue( PCardinalArray(SrcPix)[X+Y*Result.Width] );
        DstPix.rgbtGreen:= GetGValue( PCardinalArray(SrcPix)[X+Y*Result.Width] );
        DstPix.rgbtBlue := GetBValue( PCardinalArray(SrcPix)[X+Y*Result.Width] );
        Inc(DstPix);
      end;
    end;

  finally
    DeleteObject(hbmp);
    DeleteDC(hdc);
  end;
end;








//------------------------------------------------------------------------------
procedure TPHXFontGenerator.GenerateTextures;
var Partition: TPartition;
var Glyph    : TGlyph;
var Index    : Integer;
var Rect    : TRect;
begin
  BitmapFont.PixelFormat:= pf24bit;
  BitmapFont.Width      := Settings.Width;
  BitmapFont.Height     := Settings.Height;

  BitmapMask.PixelFormat:= pf24bit;
  BitmapMask.Width      := Settings.Width;
  BitmapMask.Height     := Settings.Height;

  BitmapFont.Canvas.Brush.Color:= Settings.Background;
  BitmapFont.Canvas.FillRect( BitmapFont.Canvas.ClipRect );

  BitmapMask.Canvas.Brush.Color:= clSilver;// clBlack;
  BitmapMask.Canvas.Brush.Color:= clBlack;
  BitmapMask.Canvas.FillRect( BitmapMask.Canvas.ClipRect );

  Partition:= TPartition.Create(0, 0, Settings.Width, Settings.Height, ePackingMode_BestFitFromNWCorner);
  try
    for Index := 0 to Glyphs.Count - 1 do
    begin
      Glyph:= Glyphs[Index];

      if Partition.Insert(Glyph.Width, Glyph.Height, Rect) then
      begin
        Glyph.X:= Rect.Left;
        Glyph.Y:= Rect.Top;

        BitmapFont.Canvas.Brush.Color:= Settings.Foregrond;
        BitmapFont.Canvas.FillRect(Rect);

        BitmapMask.Canvas.Draw(Rect.Left + Settings.Padding, Rect.Top + Settings.Padding, Glyph.Image);

        Glyphs[Index]:= Glyph;
      end;

    end;
  finally
    Partition.Free;
  end;
end;





{$ENDREGION}




{$ENDREGION}






// TFrmGenerator
//------------------------------------------------------------------------------
constructor TFrmGenerator.Create(AOwner: TComponent);
var Index: Integer;
begin
  inherited;

  FGenerator:= TPHXFontGenerator.Create;


  edFont.Items.BeginUpdate;
  edFont.Items.Clear;
  For Index := 0 to Screen.fonts.Count -1 do
  begin
    edFont.Items.Add(Screen.Fonts.Strings[Index]);
  end;
  edFont.Items.EndUpdate;

  edFont.ItemIndex:= edFont.Items.IndexOf('Tahoma');
end;

//------------------------------------------------------------------------------
destructor TFrmGenerator.Destroy;
begin
  FGenerator.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TFrmGenerator.Execute: Boolean;
begin
 // FGenerator.Settings.Characters.Clear;
 // FGenerator.Settings.Characters.Add(Ord('W'));
 // FGenerator.Settings.Characters.Add(Ord('M'));
 // FGenerator.Settings.Characters.Add(Ord('a'));

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.UpdateFont(Font: TPHXFont);
var Glyph: TGlyph;
var Char : TPHXCharacter;
begin
  Font.Name := FGenerator.Settings.Font;
  Font.Size := FGenerator.Settings.Size;
  Font.Style:= TPHXFontStyles(FGenerator.Settings.Style);

  Font.Metric.Height    := FGenerator.TextMetric.tmHeight;
  Font.Metric.Ascent    := FGenerator.TextMetric.tmAscent;
  Font.Metric.Descent   := FGenerator.TextMetric.tmDescent;

  for Glyph in FGenerator.Glyphs do
  begin
    Char.ID    := Glyph.Code;
    Char.X     := Glyph.X;
    Char.Y     := Glyph.Y;
    Char.Width := Glyph.Width;
    Char.Height:= Glyph.Height;

    Char.XOffset:= Glyph.Origin.X;
    Char.YOffset:= Glyph.Origin.Y;

    Char.XAdvance:= Glyph.Advance;

    Font.Characters.Add(Char);
  end;


  BitmapToTexture(FGenerator.BitmapFont, FGenerator.BitmapMask, Font.Texture);
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.GenerateShadow;
var Bitmap: TBitmap;
var x,y   : Integer;
var DstPix: PRGBTriple;
var SrcPix: PRGBTriple;
begin
  Bitmap:= TBitmap.Create;
  try
    Bitmap.Assign(FGenerator.BitmapMask);

    BmpGBlur(FGenerator.BitmapMask, edShadowSize.Value);

    for y := 0 to Bitmap.Height - 1 do
    begin
      SrcPix:= Bitmap.ScanLine[y];
      DstPix:= FGenerator.BitmapMask.ScanLine[y];
      for x := 0 to Bitmap.Width - 1 do
      begin
        if (SrcPix.rgbtRed <> 0) or (SrcPix.rgbtGreen <> 0) or (SrcPix.rgbtBlue <> 0) then
        begin
          DstPix.rgbtRed  := SrcPix.rgbtRed;
          DstPix.rgbtGreen:= SrcPix.rgbtGreen;
          DstPix.rgbtBlue := SrcPix.rgbtBlue;
        end;

        Inc(DstPix);
        Inc(SrcPix);
      end;
    end;
  finally
    Bitmap.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmGenerator.btnGenerateClick(Sender: TObject);
var Index: Integer;
begin
  inherited;


  FGenerator.Settings.Font :=       edFont.Text;
  FGenerator.Settings.Size := Trunc(edSize.Value);
  FGenerator.Settings.Style:= [];
  if edBold     .Checked then FGenerator.Settings.Style:= FGenerator.Settings.Style + [TFontStyle.fsBold];
  if edItalic   .Checked then FGenerator.Settings.Style:= FGenerator.Settings.Style + [TFontStyle.fsITalic];
  if edUnderline.Checked then FGenerator.Settings.Style:= FGenerator.Settings.Style + [TFontStyle.fsUnderline];


  FGenerator.Settings.Foregrond := edForegrond.Brush.Color;
  FGenerator.Settings.Background:= edBackground.Brush.Color;

  FGenerator.Settings.Padding:= Trunc(edPadding.Value);
  FGenerator.Settings.Width:= Trunc(edWidth.Value);
  FGenerator.Settings.Height:= Trunc(edHeight.Value);

//  FGenerator.Characters.Clear;
//  for Index := 32 to 126 do
//  begin
//    FGenerator.Characters.Add(Index);
//  end;



//  Font:= TBitmap.Create;
//  Mask:= TBitmap.Create;

//  FGenerator.Generate;
  FGenerator.GenerateFont;

  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  for Index := 0 to FGenerator.Glyphs.Count - 1 do
  begin
    ComboBox1.Items.Add( Chr(FGenerator.Glyphs[Index].Code) );
  end;
  ComboBox1.Items.EndUpdate;
  ComboBox1Change(Sender);

  if edBlurEnable.Checked then
  begin
    BmpGBlur(FGenerator.BitmapMask, 1);
  end;
  if edDropshadow.Checked then
  begin
    GenerateShadow;
  end;

//  Label4.Caption:= Format('Width: %d'#13'Height: %d', [Font.Width, Font.Height]);

 //Image2.Width := FGenerator.BitmapFont.Width;
 // Image2.Height:= FGenerator.BitmapFont.Height;
  Image2.Picture.Assign(FGenerator.BitmapFont);

 // Image3.Width := FGenerator.BitmapMask.Width;
  //Image3.Height:= FGenerator.BitmapMask.Height;
  Image3.Picture.Assign(FGenerator.BitmapMask);



end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.SpeedButton1Click(Sender: TObject);
var Font: TFont;
var Character: Cardinal;
begin
  Font:= TFont.Create;
  try
    Font.Name :=       edFont.Text;
    Font.Size := Trunc(edSize.Value);
    Font.Style:= [];
    if edBold     .Checked then Font.Style:= Font.Style + [TFontStyle.fsBold];
    if edItalic   .Checked then Font.Style:= Font.Style + [TFontStyle.fsITalic];
    if edUnderline.Checked then Font.Style:= Font.Style + [TFontStyle.fsUnderline];

    if FrmGeneratorRange.Execute(Font) then
    begin
      FGenerator.Settings.Characters.Clear;
      for Character in FrmGeneratorRange.Selection.Keys do
      begin
        FGenerator.Settings.Characters.Add( Character );
      end;
    end;

    btnGenerateClick(Sender);
   finally
    Font.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.SelectColor(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Colordialog1.Color:= TShape(Sender).Brush.Color;

  if Colordialog1.Execute then
  begin
    TShape(Sender).Brush.Color:= Colordialog1.Color;

    btnGenerateClick(Sender);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edBoldClick(Sender: TObject);
begin
  btnGenerateClick(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edSizeChange(Sender: TObject);
begin
  btnGenerateClick(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.ComboBox1Change(Sender: TObject);
var Glyph: TGlyph;
begin
  if ComboBox1.ItemIndex >= 0 then
  begin
    Glyph:= FGenerator.Glyphs[ComboBox1.ItemIndex];

    Image4.Picture.Assign(Glyph.Image);
  end;
end;


initialization
  CodePages:= TList<Cardinal>.Create;

  EnumSystemCodePages(@EnumCodePagesProc, CP_SUPPORTED);
finalization
  CodePages.Free;
end.
