unit uGenerator.Dialog;

interface

uses
  LclType, LclIntf, LMessages, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons , Math,
  Generics.Collections,

//  xmldom, XMLIntf, msxmldom, XMLDoc,

//  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx,

  uGenerator,
  uGenerator.Characters, ImgList;

type

TFrmGenerator = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    ImageList1: TImageList;
    Panel2: TPanel;
    Panel1: TPanel;
    GroupBox3: TGroupBox;
    btnCharacterSelection: TSpeedButton;
    edFont: TComboBox;
    edBold: TCheckBox;
    edItalic: TCheckBox;
    edSize: TSpinEdit;
    edUnderline: TCheckBox;
    GroupBox6: TGroupBox;
    Label9: TLabel;
    Label1: TLabel;
    edBackground: TColorBox;
    edForegrond: TColorBox;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edLayout: TComboBox;
    edWidth: TComboBox;
    edHeight: TComboBox;
    edPadding: TSpinEdit;
    GroupBox2: TGroupBox;
    edOutlineEnabled: TCheckBox;
    edOutlineWidth: TSpinEdit;
    edOutlineColor: TColorBox;
    edBlurEnabled: TCheckBox;
    edBlurRadius: TSpinEdit;
    edShadowEnable: TCheckBox;
    edShadowSize: TSpinEdit;
    TabControl1: TTabControl;
    ScrollBox1: TScrollBox;
    PaintBoxPreview: TPaintBox;
    edQuality: TComboBox;
    Label2: TLabel;
    Button1: TButton;
    btnGenerate: TButton;
    procedure btnCharacterSelectionClick(Sender: TObject);
    procedure edStyleChange(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure edBlurEnableClick(Sender: TObject);
    procedure edShadowEnableClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure PaintBoxPreviewPaint(Sender: TObject);
    procedure edOutlineEnabledClick(Sender: TObject);
    procedure edOutlineColorChange(Sender: TObject);
    procedure edOutlineWidthChange(Sender: TObject);
    procedure edLayoutChange(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
    procedure edHeightChange(Sender: TObject);
    procedure edForegrondChange(Sender: TObject);
    procedure edBackgroundChange(Sender: TObject);
    procedure edPaddingChange(Sender: TObject);
    procedure edBlurEnabledClick(Sender: TObject);
    procedure edBlurRadiusChange(Sender: TObject);
    procedure edFontChange(Sender: TObject);
    procedure edSizeChange(Sender: TObject);
    procedure edQualityChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FGenerator: TPHXFontGenerator;

    procedure GenerateFont;
    procedure GenerateShadow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(Font: TPHXFont): Boolean;
  end;

var
  FrmGenerator: TFrmGenerator;

implementation

{$R *.dfm}

uses uGenerator.Debug;

var
  CodePages: TList<Cardinal>;

function EnumCodePagesProc(lpCodePageString: LPTSTR): BOOL; stdcall;
var CodePage: Cardinal;
begin
  CodePage:= StrToIntDef(lpCodePageString, 0 );

  CodePages.Add(CodePage);

  Result := True;
end;


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

  edFont.Text:= Canvas.Font.Name;

//  edFont.ItemIndex:= edFont.Items.IndexOf('Arial Unicode MS');

  edLayout .ItemIndex:= Ord(laPacked);
  edQuality.ItemIndex:= Ord(quSmooth);
end;

//------------------------------------------------------------------------------
destructor TFrmGenerator.Destroy;
begin
  FGenerator.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TFrmGenerator.Execute(Font: TPHXFont): Boolean;
begin
 // FGenerator.Settings.Characters.Clear;
 // FGenerator.Settings.Characters.Add(Ord('W'));
 // FGenerator.Settings.Characters.Add(Ord('M'));
 // FGenerator.Settings.Characters.Add(Ord('a'));
  GenerateFont;

  Result:= ShowModal = mrOk;

  if Result then
  begin
    FGenerator.UpdateFont(Font);

    Font.Initialize;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.GenerateFont;
var Index: Integer;
begin
  FGenerator.Settings.Font :=       edFont.Text;
  FGenerator.Settings.Size := Trunc(edSize.Value);
  FGenerator.Settings.Style:= [];
  if edBold     .Checked then FGenerator.Settings.Style:= FGenerator.Settings.Style + [TFontStyle.fsBold];
  if edItalic   .Checked then FGenerator.Settings.Style:= FGenerator.Settings.Style + [TFontStyle.fsITalic];
  if edUnderline.Checked then FGenerator.Settings.Style:= FGenerator.Settings.Style + [TFontStyle.fsUnderline];


  FGenerator.Settings.Foregrond := edForegrond .Selected;
  FGenerator.Settings.Background:= edBackground.Selected;

  FGenerator.Settings.Padding:= Trunc(edPadding.Value);
  FGenerator.Settings.Layout := TGeneratorLayout (edLayout .ItemIndex);
  FGenerator.Settings.Quality:= TGeneratorQuality(edQuality.ItemIndex);


  FGenerator.Settings.TextureWidth := StrToInt(edWidth.Text);
  FGenerator.Settings.TextureHeight:= StrToInt(edHeight.Text);

  FGenerator.Settings.OutlineEnabled:=          edOutlineEnabled.Checked;
  FGenerator.Settings.OutlineWidth  := StrToInt(edOutlineWidth   .Text);
  FGenerator.Settings.OutlineColor  :=          edOutlineColor   .Selected;


  FGenerator.Generate;

  if edBlurEnabled.Checked then
  begin
  //  BmpGBlur(FGenerator.BitmapMask, edBlurRadius.Value);
    BmpGBlur(FGenerator.BitmapFont, edBlurRadius.Value);
  end;

  if edShadowEnable.Checked then
  begin
    GenerateShadow;
  end;

  PaintBoxPreview.Invalidate;

  if FrmGeneratorDebug.Visible then
  begin
    FrmGeneratorDebug.Generator:= FGenerator;
  end;
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
begin
  GenerateFont;
end;



//------------------------------------------------------------------------------
procedure TFrmGenerator.btnCharacterSelectionClick(Sender: TObject);
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
procedure TFrmGenerator.edFontChange(Sender: TObject);
begin
   GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edSizeChange(Sender: TObject);
begin
   GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edStyleChange(Sender: TObject);
begin
   GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edBlurEnableClick(Sender: TObject);
begin
  btnGenerateClick(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edShadowEnableClick(Sender: TObject);
begin
  btnGenerateClick(Sender);
end;


//------------------------------------------------------------------------------
procedure TFrmGenerator.edBackgroundChange(Sender: TObject);
begin
  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edForegrondChange(Sender: TObject);
begin
  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edLayoutChange(Sender: TObject);
begin
  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edQualityChange(Sender: TObject);
begin
  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edWidthChange(Sender: TObject);
begin
  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edHeightChange(Sender: TObject);
begin
  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edPaddingChange(Sender: TObject);
begin
  GenerateFont;
end;





//------------------------------------------------------------------------------
procedure TFrmGenerator.edOutlineEnabledClick(Sender: TObject);
begin
  edOutlineWidth.Enabled:= edOutlineEnabled.Checked;
  edOutlineColor.Enabled:= edOutlineEnabled.Checked;

  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edOutlineWidthChange(Sender: TObject);
begin
  if edOutlineWidth.Value < 1 then
  begin
    edOutlineWidth.Value:= 1;
  end;
  GenerateFont;
end;


//------------------------------------------------------------------------------
procedure TFrmGenerator.edOutlineColorChange(Sender: TObject);
begin
  GenerateFont;
end;


//------------------------------------------------------------------------------
procedure TFrmGenerator.edBlurEnabledClick(Sender: TObject);
begin
  edBlurRadius.Enabled:= edBlurEnabled.Checked;

  GenerateFont;
end;

//------------------------------------------------------------------------------
procedure TFrmGenerator.edBlurRadiusChange(Sender: TObject);
begin
  if edBlurRadius.Value < 0 then
  begin
    edBlurRadius.Value:= 0;
  end;
  GenerateFont;
end;








//------------------------------------------------------------------------------
procedure TFrmGenerator.TabControl1Change(Sender: TObject);
begin
  PaintBoxPreview.Invalidate;
end;

const PreviewZoom = 2;

//------------------------------------------------------------------------------
procedure TFrmGenerator.PaintBoxPreviewPaint(Sender: TObject);
var Bitmap: TBitmap;
var Rect  : TRect;
begin
  if TabControl1.TabIndex = 0 then
  begin
    Bitmap:= FGenerator.BitmapFont;
  end else
  begin
    Bitmap:= FGenerator.BitmapMask;
  end;

  if Assigned(Bitmap) and not Bitmap.Empty  then
  begin
    PaintBoxPreview.Width := Round(Bitmap.Width  * PreviewZoom);
    PaintBoxPreview.Height:= Round(Bitmap.Height * PreviewZoom);

    with PaintBoxPreview.Canvas do
    begin
      Rect.Left  := 0;
      Rect.Top   := 0;
      Rect.Right := PaintBoxPreview.Width - 1;
      Rect.Bottom:= PaintBoxPreview.Height - 1;

      StretchDraw(Rect, Bitmap);

      //if edShowOutlines.Checked then
     // begin

      //end;

    end;
  end;

end;


//------------------------------------------------------------------------------
procedure TFrmGenerator.Button1Click(Sender: TObject);
begin
  FrmGeneratorDebug.Generator:= FGenerator;

  FrmGeneratorDebug.ShowModal;
end;






initialization
  CodePages:= TList<Cardinal>.Create;

  EnumSystemCodePages(@EnumCodePagesProc, CP_SUPPORTED);
finalization
  CodePages.Free;
end.
