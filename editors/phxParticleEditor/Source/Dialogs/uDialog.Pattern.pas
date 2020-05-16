unit uDialog.Pattern;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  Generics.Collections,

  phxParticle,

  phxTexture,
  phxGraphicsEx;

type
  TPatternDialog = class(TForm)
    btnOkey: TButton;
    btnCancel: TButton;
    ScrollBar1: TScrollBar;
    PaintBox1: TPaintBox;
    cbTransparent: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;    Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
    procedure PaintBox1DblClick(Sender: TObject);
  private
    FTexture   : TPHXTexture;
    FBackground: TBitmap;
    FBuffer    : TBitmap;

    FPatternSize   : TPHXParticlePatternSize;
    FPatternCount  : Integer;
    FPatternColumns: Integer;
    FPatternIndex  : Integer;
    FPatternWidth  : Integer;
    FPatternHeight : Integer;

    procedure UpdateBuffers;

    procedure GetPatternPosition(const Index: Integer; out x, y: Integer);
    function GetPatternRect(Index: Integer): TRect;

    procedure SetPatternIndex(const Value: Integer);
    procedure SetPatternSize(const Value: TPHXParticlePatternSize);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PatternAt(const X, Y: Integer): Integer;

    function Execute: Boolean;

    property Texture: TPHXTexture read FTexture write FTexture;
    property PatternSize: TPHXParticlePatternSize read FPatternSize write SetPatternSize;
    property PatternIndex: Integer read FPatternIndex write SetPatternIndex;
  end;

var
  PatternDialog: TPatternDialog;

implementation

{$R *.dfm}

// TForm1
//==============================================================================
constructor TPatternDialog.Create(AOwner: TComponent);
begin
  inherited;
  FTexture:= nil;

  FBackground:= TBitmap.Create;
  FBackground.PixelFormat:= pf32Bit;
  FBackground.Width      := 8;
  FBackground.Height     := 8;

  FBuffer:= TBitmap.Create;

  DrawTransparent(FBackground.Canvas, Rect(0,0,8,8), 4);
end;

//------------------------------------------------------------------------------
destructor TPatternDialog.Destroy;
begin
  FBackground.Free;

  FBuffer.Free;

  inherited;
end;

//------------------------------------------------------------------------------
function TPatternDialog.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;

const TEXTURE_SPACING        = 4;

//------------------------------------------------------------------------------
procedure TPatternDialog.GetPatternPosition(const Index: Integer; out x, y: Integer);
var ix: Integer;
var iy: Integer;
begin
  ix:= index mod FPatternColumns;
  iy:= index div FPatternColumns;

  x:= TEXTURE_SPACING + ix * (FPatternWidth  + TEXTURE_SPACING);
  y:= TEXTURE_SPACING + iy * (FPatternHeight + TEXTURE_SPACING+16);

  y:= y - ScrollBar1.Position;
end;


//------------------------------------------------------------------------------
function TPatternDialog.PatternAt(const X, Y: Integer): Integer;
var Index: Integer;
var y2: integer;
begin
  y2:= y + ScrollBar1.Position;

  Index:= (X  - TEXTURE_SPACING) div (FPatternWidth  + TEXTURE_SPACING) +
          (Y2 - TEXTURE_SPACING) div (FPatternHeight + TEXTURE_SPACING+16) * FPatternColumns ;

  if (Index >= 0) and (Index < FPatternCount) then
  begin
    Result:= Index;
  end else
  begin
    Result:= -1;
  end;
end;
//------------------------------------------------------------------------------
procedure TPatternDialog.FormResize(Sender: TObject);
var Height : Integer;
begin
  if Texture = nil then
  begin
    ScrollBar1.Enabled := False;

    Exit;
  end;

  FPatternColumns:= (PaintBox1.Width div (FPatternWidth + TEXTURE_SPACING));

  Height:= ( (FPatternCount div FPatternColumns + 1) * (FPatternHeight + TEXTURE_SPACING + 16) ) -  PaintBox1.Height;

  if Height > 0 then
  begin
    ScrollBar1.Enabled := True;
    ScrollBar1.Max     := Height;
    ScrollBar1.Position:= 0;
  end else
  begin
    ScrollBar1.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.UpdateBuffers;
//var Index  : Integer;
//var Texture: TPHXTexture;
//var Bitmap: TBitmap;
begin

  FormResize(nil);

  if cbTransparent.Checked then
  begin
    DrawTexture(Texture, FBuffer, FBackground);
  end else
  begin
    DrawTexture(Texture, FBuffer, clBlack);
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.PaintBox1DblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPatternIndex:= PatternAt(X,Y);

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
function TPatternDialog.GetPatternRect(Index: Integer): TRect;
var X, Y: Integer;
begin
  X:= 0;
  Y:= 0;

  case FPatternSize of
    ps1x1:
    begin
      X:= 0;
      Y:= 0;
    end;
    ps2x2:
    begin
      X:= (Index mod 2);
      Y:= (Index div 2);
    end;
    ps4x4:
    begin
      X:= (Index mod 4);
      Y:= (Index div 4);
    end;
    ps8x8:
    begin
      X:= (Index mod 8);
      Y:= (Index div 8);
    end;
  end;

  Result.Left := X * FPatternWidth;
  Result.Right:= X * FPatternWidth + FPatternWidth;

  Result.Top   := Y * FPatternHeight;
  Result.Bottom:= Y * FPatternHeight + FPatternHeight;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.PaintBox1Paint(Sender: TObject);
var Index  : Integer;
var X,Y    : Integer;
var DstRect   : TRect;
var SrcRect   : TRect;
begin
  with PaintBox1.Canvas do
  begin

    Brush.Style:= bsClear;

    Font.Style:= [fsBold];

    for Index := 0 to FPatternCount - 1 do
    begin
      GetPatternPosition(Index, X, Y);

      DstRect.Left  := x;
      DstRect.Right := x + FPatternWidth;
      DstRect.Top   := y + 16;
      DstRect.Bottom:= y + 16 + FPatternHeight;

      SrcRect  := GetPatternRect(Index);

      CopyRect(DstRect, FBuffer.Canvas, SrcRect);


      TextOut(x, y, IntToStr(Index));

      if Index = FPatternIndex then
      begin
        Pen.Color:= clRed;

        DstRect.Left   := DstRect.Left   - 2;
        DstRect.Top    := DstRect.Top    - 2;
        DstRect.Right  := DstRect.Right  + 2;
        DstRect.Bottom := DstRect.Bottom + 2;

        Rectangle(DstRect);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.ScrollBar1Change(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.cbTransparentClick(Sender: TObject);
begin
  if cbTransparent.Checked then
  begin
    DrawTexture(Texture, FBuffer, FBackground);
  end else
  begin
    DrawTexture(Texture, FBuffer, clBlack);
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.SetPatternIndex(const Value: Integer);
begin
  FPatternIndex := Value;

  UpdateBuffers;
end;

//------------------------------------------------------------------------------
procedure TPatternDialog.SetPatternSize(const Value: TPHXParticlePatternSize);
begin
  FPatternSize := Value;
  FPatternCount:= GetPatternCount(Value);

  if Assigned(FTexture) and (FTexture.Width > 0 ) and (FTexture.Height > 0) then
  begin
    case FPatternSize of
      ps1x1:
      begin
        FPatternWidth := Texture.Width;
        FPatternHeight:= Texture.Height;

        FPatternColumns:= 1;
      end;
      ps2x2:
      begin
        FPatternWidth := Texture.Width  div 2;
        FPatternHeight:= Texture.Height div 2;

        FPatternColumns:= 2;
      end;
      ps4x4:
      begin
        FPatternWidth := Texture.Width  div 4;
        FPatternHeight:= Texture.Height div 4;

        FPatternColumns:= 4;
      end;
      ps8x8:
      begin
        FPatternWidth := Texture.Width  div 8;
        FPatternHeight:= Texture.Height div 8;

        FPatternColumns:= 8;
      end;
    end;
  end else
  begin
    FPatternWidth :=0;
    FPatternHeight:=0;
  end;

  UpdateBuffers;
end;

end.
