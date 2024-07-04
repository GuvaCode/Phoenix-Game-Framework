unit uTile.Select;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.ImgList,

  Vcl.Mask, JvExMask, JvSpin,

  phxTypes,
  phxMath,

  phxImage,
  phxImageEx,

  phxGraphics,
  phxGraphicsEx;

type

TPivotMode = (
  pmLeft,
  pmCenter,
  pmRight
);

//------------------------------------------------------------------------------
TFrmTileSelect = class(TForm)
    Panel1: TPanel;
    brnOkey: TButton;
    btnCancel: TButton;
    Panel2: TPanel;
    ImageList1: TImageList;
    Panel3: TPanel;
    ToolBar1: TToolBar;
    btnZoomIn: TToolButton;
    btnZoom100: TToolButton;
    btnZoomOut: TToolButton;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    GroupBox1: TGroupBox;
    edGridHeight: TJvSpinEdit;
    edGridWidth: TJvSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    cbGridDraw: TCheckBox;
    cbGridSnap: TCheckBox;
    GroupBox2: TGroupBox;
    edTileHeight: TJvSpinEdit;
    edTileWidth: TJvSpinEdit;
    edTileY: TJvSpinEdit;
    edTileX: TJvSpinEdit;
    edTileName: TEdit;
    Label5: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1DblClick(Sender: TObject);
    procedure edTileXChange(Sender: TObject);
    procedure edTileYChange(Sender: TObject);
    procedure edTileWidthChange(Sender: TObject);
    procedure edTileHeightChange(Sender: TObject);
    procedure edTileNameChange(Sender: TObject);
    procedure cbGridDrawClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoom100Click(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure edGridWidthChange(Sender: TObject);
    procedure edGridHeightChange(Sender: TObject);
  private
    FBuffer     : TBitmap;
    FBackground : TBitmap;
    FImage      : TPHXImage;
    FZoom       : Single;

    FTileName  : String;
    FTileX     : Integer;
    FTileY     : Integer;
    FTileHeight: Integer;
    FTileWidth : Integer;
    FTilePivotX: Integer;
    FTilePivotY: Integer;

    FHoverX: Integer;
    FHoverY: Integer;


    FGridHeight: Integer;
    FGridWidth : Integer;

    function ScreenToTile(const X,Y: Integer): TPoint;

    procedure DrawGrid;

    procedure PickTile(const X,Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(const Image: TPHXImage): Boolean;

    // The image containing the tiles
    property Image: TPHXImage read FImage;
    // Image zoom factor
    property Zoom: Single read FZoom write FZoom;

    property TileName: String read FTileName write FTileName;
    // Width of the tile
    property TileX: Integer read FTileX write FTileX;
    // Height of the tile
    property TileY: Integer read FTileY write FTileY;
    // Width of the tile
    property TileWidth: Integer read FTileWidth write FTileWidth;
    // Height of the tile
    property TileHeight: Integer read FTileHeight write FTileHeight;

    // Height of the tile
    property TilePivotX: Integer read FTilePivotX write FTilePivotX;
    // Height of the tile
    property TilePivotY: Integer read FTilePivotY write FTilePivotY;
  end;

var
  FrmTileSelect: TFrmTileSelect;

implementation

{$R *.dfm}

// TFrmTileSelect
//==============================================================================
constructor TFrmTileSelect.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= TBitmap.CreateBackground(4);

  ScrollBox1.DoubleBuffered:= True;
end;

//------------------------------------------------------------------------------
destructor TFrmTileSelect.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;

  inherited;
end;


//------------------------------------------------------------------------------
function TFrmTileSelect.Execute(const Image: TPHXImage): Boolean;
begin
  FImage:= Image;
  FZoom := 1.0;

  FImage.Draw(FBuffer, FBackground);

  PaintBox1.Width := 4 + Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= 4 + Trunc(Image.Height * Zoom);

  FGridWidth := FTileWidth;
  FGridHeight:= FTileHeight;

  edGridWidth .Value:= FGridWidth;
  edGridHeight.Value:= FGridHeight;

  edTileName  .Text := FTileName;
  edTileX     .Value:= FTileX;
  edTileY     .Value:= FTileY;
  edTileWidth .Value:= FTileWidth;
  edTileHeight.Value:= FTileHeight;
 //   FTilePivotX: Integer;
 //   FTilePivotY: Integer;

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.PaintBox1DblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

//------------------------------------------------------------------------------
function TFrmTileSelect.ScreenToTile(const X, Y: Integer): TPoint;
var W,H: Integer;
begin
  W:= Trunc(FGridWidth  * Zoom);
  H:= Trunc(FGridHeight * Zoom);

  if cbGridSnap.Checked then
  begin
    Result.X:= Trunc((X-2) / W) * W;
    Result.Y:= Trunc((Y-2) / H) * H;
  end else
  begin
    Result.X:= (X-2);
    Result.Y:= (Y-2);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.PickTile(const X, Y: Integer);
var W,H: Integer;
begin
  W:= Trunc(FGridWidth  * Zoom);
  H:= Trunc(FGridHeight * Zoom);

  if cbGridSnap.Checked then
  begin
    FTileX:= Trunc((X-2) / W) * W;
    FTileY:= Trunc((Y-2) / H) * H;
  end else
  begin
    FTileX:= (X-2);
    FTileY:= (Y-2);
  end;

  edTileX.Value:= FTileX;
  edTileY.Value:= FTileY;
end;



//------------------------------------------------------------------------------
procedure TFrmTileSelect.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    PickTile(X,Y);

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Tile: TPoint;
begin
  Tile:= ScreenToTile(X, Y);
  if ssLeft in Shift then
  begin
    PickTile(X,Y);

    PaintBox1.Invalidate;
  end;

  if (FHoverX <> Tile.X) or (FHoverY <> Tile.Y) then
  begin
    FHoverX:= Tile.X;
    FHoverY:= Tile.Y;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.DrawGrid;
var X,Y: Integer;
var W,H: Integer;
begin
  W:= Trunc(FGridWidth  * Zoom);
  H:= Trunc(FGridHeight * Zoom);

  with PaintBox1.Canvas do
  begin
    Pen.Color:= clBlack;
    Pen.Style:= psDot;

    X:= 2;
    while X + W < PaintBox1.Width  do
    begin
      MoveTo(X, 2);
      LineTo(X, PaintBox1.Height);

      X:= X + W;
    end;

    Y:= 2;
    while Y + H < PaintBox1.Height  do
    begin
      MoveTo(2              , Y);
      LineTo(PaintBox1.Width, Y);

      Y:= Y + H;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.PaintBox1Paint(Sender: TObject);
var Rect: TRect;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Style:= bsSolid;
    Brush.Color:= clWhite;

    FillRect(PaintBox1.ClientRect);

    Rect.Left  := 2;
    Rect.Top   := 2;
    Rect.Right := 2 + Trunc(Image.Width  * Zoom);
    Rect.Bottom:= 2 + Trunc(Image.Height * Zoom);

    StretchDraw(Rect, FBuffer);

    Brush.Style:= bsClear;
    if cbGridDraw.Checked then
    begin
      DrawGrid;
    end;

    // Draw hover
    if (FHoverX <> TileX) or (FHoverY <> FTileY) then
    begin
      Rect.Left  := FHoverX + 2;
      Rect.Top   := FHoverY + 2;
      Rect.Right := FHoverX + 2 + Trunc(TileWidth  * Zoom);
      Rect.Bottom:= FHoverY + 2 + Trunc(TileHeight * Zoom);

      Pen.Color:= clFuchsia;
      Pen.Style:= psDot;
      Rectangle(Rect);
    end;

    // Draw selection
    Rect.Left  := TileX + 2;
    Rect.Top   := TileY + 2;
    Rect.Right := TileX + 2 + Trunc(TileWidth  * Zoom);
    Rect.Bottom:= TileY + 2 + Trunc(TileHeight * Zoom);

    Pen.Color:= clFuchsia;
    Pen.Style:= psSolid;
    Rectangle(Rect);

  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.cbGridDrawClick(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.edGridWidthChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edGridWidth.Value);

  if Value <> FGridWidth then
  begin
    FGridWidth:= Value;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.edGridHeightChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edGridHeight.Value);

  if Value <> FGridHeight then
  begin
    FGridHeight:= Value;

    PaintBox1.Invalidate;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmTileSelect.edTileNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edTileName.Text;

  if Value <> FTileName then
  begin
    FTileName:= Value;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.edTileXChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edTileX.Value);

  if Value <> FTileX then
  begin
    FTileX:= Value;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.edTileYChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edTileY.Value);

  if Value <> FTileY then
  begin
    FTileY:= Value;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.edTileWidthChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edTileWidth.Value);

  if Value <> FTileWidth then
  begin
    FTileWidth:= Value;

    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.edTileHeightChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edTileHeight.Value);

  if Value <> FTileHeight then
  begin
    FTileHeight:= Value;

    PaintBox1.Invalidate;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmTileSelect.btnZoomInClick(Sender: TObject);
begin
  FZoom:= FZoom * 2;

  PaintBox1.Width := 4 + Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= 4 + Trunc(Image.Height * Zoom);
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.btnZoomOutClick(Sender: TObject);
begin
  FZoom:= FZoom * 0.5;

  PaintBox1.Width := 4 + Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= 4 + Trunc(Image.Height * Zoom);
end;

//------------------------------------------------------------------------------
procedure TFrmTileSelect.btnZoom100Click(Sender: TObject);
begin
  FZoom:= 1;

  PaintBox1.Width := 4 + Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= 4 + Trunc(Image.Height * Zoom);
end;




end.
