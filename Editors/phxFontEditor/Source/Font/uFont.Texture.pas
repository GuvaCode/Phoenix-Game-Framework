unit uFont.Texture;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  phxGraphics,
  phxGraphicsEx,

  phxFont,
  phxFontEx;

type
  TFrmFontTexture = class(TFrame)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    edViewTexture: TRadioButton;
    edViewBitmap: TRadioButton;
    edViewMask: TRadioButton;
    edDrawGlyphs: TCheckBox;
    ImageList2: TImageList;
    ToolBar1: TToolBar;
    btnZoomIn: TToolButton;
    btnZoom100: TToolButton;
    btnZoomOut: TToolButton;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure InvalidatePaintbox(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoom100Click(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
  private
    FFont: TPHXFont;

    FTransparent: TBitmap;
    FTexture    : TBitmap;
    FColor      : TBitmap;
    FMask       : TBitmap;
    FZoom       : Single;

    procedure SetFont(const Value: TPHXFont);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnableControls(const Enabled: Boolean);

    Property Font: TPHXFont read FFont write SetFont;
  end;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

// TFrmTexture
//------------------------------------------------------------------------------
constructor TFrmFontTexture.Create(AOwner: TComponent);
begin
  inherited;
  FTransparent:= CreateTransparentImage(4);
  FTexture    := TBitmap.Create;
  FColor      := TBitmap.Create;
  FMask       := TBitmap.Create;
  FZoom       := 1.00;
end;

//------------------------------------------------------------------------------
destructor TFrmFontTexture.Destroy;
begin
  FTransparent.Free;
  FTexture.Free;
  FColor.Free;
  FMask.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmFontTexture.btnZoom100Click(Sender: TObject);
begin
  FZoom:= 1.00;

  if Assigned(Font) then
  begin
    PaintBox1.Width := Round(Font.Texture.Width  * FZoom);
    PaintBox1.Height:= Round(Font.Texture.Height * FZoom);
    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontTexture.btnZoomInClick(Sender: TObject);
begin
  FZoom:= FZoom * 2.0;

  if Assigned(Font) then
  begin
    PaintBox1.Width := Round(Font.Texture.Width  * FZoom);
    PaintBox1.Height:= Round(Font.Texture.Height * FZoom);
    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontTexture.btnZoomOutClick(Sender: TObject);
begin
  FZoom:= FZoom * 0.5;

  if Assigned(Font) then
  begin
    PaintBox1.Width := Round(Font.Texture.Width  * FZoom);
    PaintBox1.Height:= Round(Font.Texture.Height * FZoom);
    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontTexture.InvalidatePaintbox(Sender: TObject);
begin

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmFontTexture.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edViewTexture.Enabled:= Enabled;
  edViewBitmap.Enabled:= Enabled;
  edViewMask.Enabled:= Enabled;

  edDrawGlyphs.Enabled:= Enabled;
//  edName.Color  := EnabledColors[Enabled];

end;



//------------------------------------------------------------------------------
procedure TFrmFontTexture.SetFont(const Value: TPHXFont);
begin
  if FFont <> Value then
  begin
    FFont:= Value;
    FZoom:= 1.0;
  end;

  if Assigned(Font) then
  begin
    EnableControls(True);
    Font.Texture.ToBitmap(FTexture, FTransparent);
    Font.Texture.ToBitmap(FColor  , chColor);
    Font.Texture.ToBitmap(FMask   , chAlpha);

    PaintBox1.Width := Round(Font.Texture.Width  * FZoom);
    PaintBox1.Height:= Round(Font.Texture.Height * FZoom);
  end else
  begin
    EnableControls(False);

    FTexture.Width:= 0;
    FTexture.Height:= 0;

    FColor.Width:= 0;
    FColor.Height:= 0;

    FMask.Width:= 0;
    FMask.Height:= 0;
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmFontTexture.PaintBox1Paint(Sender: TObject);
var Index: Integer;
var Rect : TRect;
var Character: TPHXCharacter;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Style:= bsSolid;
    Brush.Color:= clBtnFace;

    FillRect(ClipRect);

    Rect.Left  := 0;
    Rect.Top   := 0;
    Rect.Right := Round(FTexture.Width  * FZoom);
    Rect.Bottom:= Round(FTexture.Height * FZoom);

    if edViewTexture.Checked then StretchDraw(Rect, FTexture);
    if edViewBitmap .Checked then StretchDraw(Rect, FColor);
    if edViewMask   .Checked then StretchDraw(Rect, FMask);

    if Assigned(Font) then
    begin

      if edDrawGlyphs.Checked then
      begin
        Brush.Style:= bsClear;
        for Index := 0 to FFont.Characters.Count - 1 do
        begin
          Character:= FFont.Characters.List^[Index];

          Rect.Left  := Round(FZoom * (Character.X));
          Rect.Top   := Round(FZoom * (Character.Y));
          Rect.Right := Round(FZoom * (Character.X + Character.Width));
          Rect.Bottom:= Round(FZoom * (Character.Y + Character.Height));

          Pen  .Color:= $00A0A0A0;
          MoveTo(Rect.Left, Rect.Top); LineTo(Rect.Right, Rect.Top);
          MoveTo(Rect.Left, Rect.Top); LineTo(Rect.Left, Rect.Bottom);

          Pen  .Color:= $00808080;
          MoveTo(Rect.Left , Rect.Bottom); LineTo(Rect.Right, Rect.Bottom);
          MoveTo(Rect.Right, Rect.Top   ); LineTo(Rect.Right, Rect.Bottom);

          Font.Color:= clWhite;
          TextOut(Rect.Left + 2, Rect.Top + 2, IntToStr(Character.ID));
        end;
      end;
    end;
  end;
end;

end.
