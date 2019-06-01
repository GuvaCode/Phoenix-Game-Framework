unit uFont.Preview;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Vcl.Menus, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,

  phxTypes,

  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx;

type
  TFrmFontPreview = class(TFrame)
    GroupBox1: TGroupBox;
    edText: TMemo;
    Panel6: TPanel;
    pbPreview: TPaintBox;
    PopupMenu1: TPopupMenu;
    BackgroundColor1: TMenuItem;
    ColorDialog1: TColorDialog;
    procedure pbPreviewPaint(Sender: TObject);
    procedure edTextChange(Sender: TObject);
    procedure BackgroundColor1Click(Sender: TObject);
  private
    FFont: TPHXFont;
    FBuffer: TBitmap;
    FColor : TColor;

    procedure GeneratePreview(const Text: String);

    procedure SetFont(const Value: TPHXFont);

    procedure DrawCharacter(Bitmap: TPHXBitmap; const Character: TPHXCharacter; X,  Y: Integer); overload;
//    procedure DrawCharacter(Bitmap: TBitmap; const Character: TPHXCharacter; X, Y: Integer); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RedrawFont;

    procedure EnableControls(const Enabled: Boolean);

    Property Font: TPHXFont read FFont write SetFont;
    property Buffer    : TBitmap read FBuffer;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

//const COLOR_BACKGROUND1 = $00D8D8D8;
//const COLOR_BACKGROUND2 = $00C0C0C0;

// Panagrams:
// http://en.wikipedia.org/wiki/List_of_pangrams

// TFrmPreview
//==============================================================================
constructor TFrmFontPreview.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer:= TBitmap.Create;
  FColor := $00D8D8D8;

  Panel6.DoubleBuffered:= true;
end;

//------------------------------------------------------------------------------
destructor TFrmFontPreview.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edText.Enabled:= Enabled;
  edText.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.DrawCharacter(Bitmap: TPHXBitmap; const Character: TPHXCharacter; X,Y: Integer);
var DstRect: TRecti;
var SrcRect: TRecti;
begin
  DstRect.Left  := X - Character.Offset.X;
  DstRect.Top   := Y - Character.Offset.Y - Font.Metric.Offset;
  DstRect.Right := DstRect.Left + Character.Width;
  DstRect.Bottom:= DstRect.Top  + Character.Height;

  SrcRect.Left  := Character.X;
  SrcRect.Top   := Character.Y;
  SrcRect.Right := Character.X + Character.Width;
  SrcRect.Bottom:= Character.Y + Character.Height;

  Bitmap.Draw(Font.Texture.Graphic, SrcRect, DstRect );
end;
    {
//------------------------------------------------------------------------------
procedure TFrmFontPreview.DrawCharacter(Bitmap: TBitmap; const Character: TPHXCharacter; X,Y: Integer);
var DstRect: TRect;
var SrcRect: TRect;
begin
  DstRect.Left  := X;
  DstRect.Top   := Y;
  DstRect.Right := X+Character.Width;
  DstRect.Bottom:= Y+Character.Height;

  SrcRect.Left  := Character.X;
  SrcRect.Top   := Character.Y;
  SrcRect.Right := Character.X + Character.Width;
  SrcRect.Bottom:= Character.Y + Character.Height;

  Bitmap.Width := Character.Width;
  Bitmap.Height:= Character.Height;

  Bitmap.Canvas.CopyRect(DstRect, FBuffer.Canvas, SrcRect);
end;
        }
//------------------------------------------------------------------------------
procedure TFrmFontPreview.GeneratePreview(const Text: String);
var Bitmap: TPHXBitmap;
var Index : Integer;
var X, Y  : Integer;
var Character: Integer;
begin
  Bitmap:= TPHXBitmap.Create;
  Bitmap.Width := Font.TextExtent(Text).X;
  Bitmap.Height:= Font.TextExtent(Text).Y;
  Bitmap.Format:= pfRGBA;

  Bitmap.Fill(GetRValue(FColor), GetGValue(FColor), GetBValue(FColor), 0);

  X:= 0;
  Y:= 0;
  for Index := 1 to Length(text) do
  begin
    Character:= Font.Characters.Find( text[Index] );

    if Text[Index] = #13 then
    begin
      X:= 0;
      Y:= Y + Font.Metric.Height;
    end else
    if Character >= 0 then
    begin
      DrawCharacter(Bitmap, Font.Characters.List^[Character], X, Y);

      X:= X + Font.Characters.List^[Character].Advance;
    end;
   // begin
    //  X:= X+ Font.Metric.SpaceWidth;
    //end;
  end;

 // FBuffer.Assign(Bitmap);
  Bitmap.ToBitmap(FBuffer);

  Bitmap.Free;
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.SetFont(const Value: TPHXFont);
begin
  FFont := Value;

  if Assigned(FFont) then
  begin
    EnableControls(True);

    GeneratePreview(edText.Text);

   // DrawTexture(Font.Texture, FBuffer, COLOR_BACKGROUND1);
  end else
  begin
    EnableControls(False);


    FBuffer.Width:= 0;
    FBuffer.Height:= 0;
  end;

  pbPreview.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.edTextChange(Sender: TObject);
begin
  if Assigned(FFont) then
  begin
    GeneratePreview(edText.Text);
  end;

  pbPreview.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.pbPreviewPaint(Sender: TObject);
begin

  with pbPreview.Canvas do
  begin
    Brush.Style:= bsSolid;
    Brush.Color:= clBtnFace;

    FillRect(ClipRect);

    Draw(4,4, Buffer);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.RedrawFont;
begin
    if Assigned(FFont) then
    begin
      GeneratePreview(edText.Text);
    end;

    pbPreview.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmFontPreview.BackgroundColor1Click(Sender: TObject);
begin
  ColorDialog1.Color:= FColor;

  if ColorDialog1.Execute then
  begin
    FColor:= ColorDialog1.Color;

    RedrawFont;
  end;

end;


end.
