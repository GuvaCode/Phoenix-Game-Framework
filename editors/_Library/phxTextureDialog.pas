unit phxTextureDialog;
{$mode Delphi}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,  ExtCtrls, StdCtrls,  LCLType, LCLIntf,

  Generics.Collections,

  phxTypes,
  phxTexture,
  phxGraphics,
  phxDevice;

type
  TFrmTextureDialog = class(TForm)
    PaintBox1: TPaintBox;
    ScrollBar1: TScrollBar;
    Panel1: TPanel;
    btnOkey: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    OpenImageDialog: TOpenDialog;
    cbTransparent: TCheckBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure Label1Click(Sender: TObject);
    procedure cbTransparentClick(Sender: TObject);
    procedure PaintBox1DblClick(Sender: TObject);
  private
    FDevice  : TPHXDevice;
    FTexture : TPHXTexture;
    FTextures: TPHXTextureList;

    FBackground: TBitmap;
    FBitmaps   : TList<TBitmap>;
    procedure SetTexture(const Value: TPHXTexture);
    procedure SetTextures(const Value: TPHXTextureList);
    procedure SetDevice(const Value: TPHXDevice);
  protected
    Procedure UpdateBuffers;

    procedure GetTexturePosition(const Index: Integer; out x, y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function TextureAt(const X, Y: Integer): TPHXTexture;

    property Device: TPHXDevice read FDevice write SetDevice;
    property Texture: TPHXTexture read FTexture write SetTexture;
    property Textures: TPHXTextureList read FTextures write SetTextures;
   end;

//------------------------------------------------------------------------------
TPHXTextureDialog = class(TComponent)
  private
    FDevice  : TPHXDevice;
    FTexture : TPHXTexture;
    FTextures: TPHXTextureList;
  published
  public
    function Execute: Boolean;

    property Device: TPHXDevice read FDevice write FDevice;
    property Texture: TPHXTexture read FTexture write FTexture;
    property Textures: TPHXTextureList read FTextures write FTextures;
  end;

var
  FrmTextureDialog: TFrmTextureDialog;


procedure Register;

implementation

{$R *.dfm}

Uses phxGraphicsEx;

procedure Register;
begin
  RegisterComponents('Phoenix', [TPHXTextureDialog]);
end;



// TPHXTextureDialog
//------------------------------------------------------------------------------
function TPHXTextureDialog.Execute: Boolean;
var Form: TFrmTextureDialog;
begin
  Form:= TFrmTextureDialog.Create(Owner);
  try
    Form.Device  := Self.Device;
    Form.Texture := Self.Texture;
    Form.Textures:= Self.Textures;

    Result:= Form.ShowModal = mrOk;
    if Result then
    begin
      Self.Texture:= Form.Texture;
    end;


  finally
    Form.Free;
  end;


end;

// TFrmTextureDialog
//==============================================================================
constructor TFrmTextureDialog.Create(AOwner: TComponent);
var Index: Integer;
var List : TPHXGraphicFormats;
var Filter: String;
begin
  inherited;
  List:= GraphicFormats;

  Filter:= 'All supported texture formats|';
  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + ';*' + String(List.Items[Index].Extension);
  end;
  Filter:= Filter + '|';

  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + Format('%s|*.%s|', [List.Items[Index].Extension, List.Items[Index].Extension]);
  end;


  //FDevice  : TPHXDevice;
  //FTexture : TPHXTexture;
  //FTextures := TPHXTextureList.Create(FDevice);
 // FTextures: TPHXTextureList;


  OpenImageDialog.Filter:= Filter;

  FBackground:= TBitmap.Create;
  FBackground.PixelFormat:= pf32Bit;
  FBackground.Width      := 8;
  FBackground.Height     := 8;


  FBitmaps:= TList<TBitmap>.Create;

  DrawTransparent(FBackground.Canvas, Rect(0,0,8,8), 4);
end;

//------------------------------------------------------------------------------
destructor TFrmTextureDialog.Destroy;
begin
  SetTextures(nil);

  FBitmaps.Free;

  FBackground.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.Label1Click(Sender: TObject);
var Index   : Integer;
var FileName: String;
var Texture : TPHXTexture;
begin
  if Assigned(Device) and OpenImageDialog.Execute then
  begin
    for Index := 0 to OpenImageDialog.Files.Count - 1 do
    begin
      FileName:= OpenImageDialog.Files[Index];

      Texture:= FTextures.LoadTexture(FileName);

      SetTexture(Texture);
    end;

    UpdateBuffers;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.UpdateBuffers;
var Index  : Integer;
var Texture: TPHXTexture;
var Bitmap: TBitmap;
begin
  for Bitmap in FBitmaps do
  begin
     Bitmap.Free;
  end;
  FBitmaps.Clear;

  FormResize(nil);

  if FTextures = nil then Exit;

  for Index := 0 to FTextures.Count-1 do
  begin
    Texture:= FTextures[Index];
    Bitmap:= TBitmap.Create;

    if cbTransparent.Checked then
    begin
      DrawTexture(Texture, Bitmap, FBackground);
    end else
    begin
      DrawTexture(Texture, Bitmap, clBlack);
    end;

    FBitmaps.Add(Bitmap);
  end;


  PaintBox1.Invalidate;
end;

const TEXTURE_COLUMN_COUNT   = 4;
const TEXTURE_WIDTH          = 128;
const TEXTURE_HEIGHT         = 128;
const TEXTURE_SPACING        = 4;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.GetTexturePosition(const Index: Integer; out x, y: Integer);
var ix: Integer;
var iy: Integer;
begin
  ix:= index mod TEXTURE_COLUMN_COUNT;
  iy:= index div TEXTURE_COLUMN_COUNT;

  x:= TEXTURE_SPACING + ix * (TEXTURE_WIDTH  + TEXTURE_SPACING);
  y:= TEXTURE_SPACING + iy * (TEXTURE_HEIGHT + TEXTURE_SPACING + 16);

  y:= y - ScrollBar1.Position;
end;

//------------------------------------------------------------------------------
function TFrmTextureDialog.TextureAt(const X, Y: Integer): TPHXTexture;
var Index: Integer;
var y2: integer;
begin
  y2:= y + ScrollBar1.Position;

  Index:= (X  - TEXTURE_SPACING) div (TEXTURE_WIDTH  + TEXTURE_SPACING     ) +
          (Y2 - TEXTURE_SPACING) div (TEXTURE_HEIGHT + TEXTURE_SPACING + 16) * TEXTURE_COLUMN_COUNT ;

  if Assigned(Textures) and (Index >= 0) and (Index < Textures.Count) then
  begin
     Result:= Textures[Index];
  end else
  begin
    Result:= nil;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmTextureDialog.PaintBox1DblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Texture:= TextureAt(X,Y);

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.PaintBox1Paint(Sender: TObject);
var Texture: TPHXTexture;
var Bitmap : TBitmap;
var Index  : Integer;
var X,Y    : Integer;
var Rect   : TRect;
var ScaleX : Single;
var ScaleY : Single;
begin
  with PaintBox1.Canvas do
  begin

    Brush.Style:= bsClear;

    Font.Style:= [fsBold];
    if FTextures = nil then Exit;
    for Index := 0 to Textures.Count - 1 do
    begin
      Texture:=FTextures[Index];
      Bitmap :=FBitmaps[Index];

      GetTexturePosition(Index, X, Y);

      if (Texture.Width > 0) and (Texture.Height > 0) then
      begin
        Rect.Left  := x;
        Rect.Top   := y + 16;

        ScaleX:= TEXTURE_WIDTH  / Texture.Width ;
        ScaleY:= TEXTURE_HEIGHT / Texture.Height ;

        if (Texture.Width > TEXTURE_WIDTH) or (Texture.Height > TEXTURE_HEIGHT) then
        begin
          if ScaleX <= ScaleY then
          begin
            Rect.Right := x + Round(Texture.Width  * ScaleX);
            Rect.Bottom:= y + Round(Texture.Height * ScaleX) + 16;
          end else
          begin
            Rect.Right := x + Round(Texture.Width  * ScaleY);
            Rect.Bottom:= y + Round(Texture.Height * ScaleY) + 16;
          end;
        end else
        begin
          Rect.Right := x + Texture.Width;
          Rect.Bottom:= y + Texture.Height + 16;
        end;

        StretchDraw(Rect, Bitmap);
      end;

      TextOut(x, y, Texture.Name);

      if Texture = Self.Texture then
      begin
        Pen.Color:= clRed;

        Rect.Left   := Rect.Left   - 2;
        Rect.Top    := Rect.Top    - 2;
        Rect.Right  := Rect.Right  + 2;
        Rect.Bottom := Rect.Bottom + 2;

        Rectangle(Rect);
      end;
    end;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmTextureDialog.ScrollBar1Change(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.FormResize(Sender: TObject);
var Height : Integer;
begin
  if Textures = nil then
  begin
    ScrollBar1.Enabled := False;

    Exit;
  end;

  Height:=( (Textures.Count div TEXTURE_COLUMN_COUNT + 1) * (TEXTURE_HEIGHT + TEXTURE_SPACING) ) -  PaintBox1.Height;

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
procedure TFrmTextureDialog.SetDevice(const Value: TPHXDevice);
begin
  FDevice := Value;

  Label1.Enabled:= Assigned(FDevice);
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.SetTexture(const Value: TPHXTexture);
begin
  FTexture := Value;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureDialog.SetTextures(const Value: TPHXTextureList);
begin
  FTextures := Value;

  UpdateBuffers;
end;

procedure TFrmTextureDialog.cbTransparentClick(Sender: TObject);
begin

  UpdateBuffers;
end;


end.
