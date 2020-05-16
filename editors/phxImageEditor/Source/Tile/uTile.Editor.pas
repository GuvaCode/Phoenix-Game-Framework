unit uTile.Editor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Math,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ImgList, Vcl.Mask,

  JvExMask, JvSpin,

  Generics.Defaults,
  Generics.Collections,

  phxTypes,
  phxMath,

  phxImage,
  phxImageEx,

  phxGraphics,
  phxGraphicsEx,

  uTile.Select, Vcl.ActnList;

type

//------------------------------------------------------------------------------
TPHXTile = record
  // Name of the tile
  Name: String[128];
  // Index of the tile
  Index: Integer;
  // X location in the image
  X: Integer;
  // Y location in the image
  Y: Integer;
  // Width of the tile
  Width: Integer;
  // Height of the tile
  Height: Integer;
  // The pivot point
  Pivot: TVector2i;
  // Flip the tile
  Flip: Boolean;
  // Mirror the tile
  Mirror: Boolean;
end;

//------------------------------------------------------------------------------
TPHXTileComparer = class(TInterfacedObject, IComparer<TPHXTile>)
  public
    function Compare(const Left, Right: TPHXTile): Integer;
  end;


//------------------------------------------------------------------------------
TFrmTileEditor = class(TForm)
    Panel1: TPanel;
    brnOkey: TButton;
    btnCancel: TButton;
    Panel2: TPanel;
    ScrollBox1: TScrollBox;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    btnZoomIn: TToolButton;
    btnZoom100: TToolButton;
    btnZoomOut: TToolButton;
    Panel3: TPanel;
    PaintBox1: TPaintBox;
    ToolButton1: TToolButton;
    btnAdd: TToolButton;
    btnDelete: TToolButton;
    ActionList1: TActionList;
    actTileDelete: TAction;
    GroupBox1: TGroupBox;
    edTileColumns: TJvSpinEdit;
    edTileHeight: TJvSpinEdit;
    edTileWidth: TJvSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edTileName: TEdit;
    Label4: TLabel;
    actTileAdd: TAction;
    cbDrawIndex: TCheckBox;
    actTileUp: TAction;
    actTileDown: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Label5: TLabel;
    edTileMode: TComboBox;

    procedure PaintBox1Paint(Sender: TObject);
    procedure edTileWidthChange(Sender: TObject);
    procedure edTileHeightChange(Sender: TObject);
    procedure edTileColumnsChange(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;   Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoom100Click(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure actTileDeleteExecute(Sender: TObject);
    procedure actTileUpdate(Sender: TObject);
    procedure actTileAddExecute(Sender: TObject);
    procedure cbDrawIndexClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FBuffer     : TBitmap;
    FBackground : TBitmap;
    FImage      : TPHXImage;
    FZoom       : Single;
    FSelected   : Integer;

    FTiles      : TList<TPHXTile>;
    FTileHeight : Integer;
    FTileWidth  : Integer;
    FTileColumns: Integer;

    procedure DrawGrid;
    procedure DrawTiles;
    procedure DrawTile(Dest: TCanvas; const X,Y: Integer; const Tile: Integer; const Zoom: Single);

    procedure ResizePaintbox;

    // Copy the tiles from the image patterns
    procedure TilesFromPatterns;
    // Copy the image patterns from the tiles
    procedure TilesToPatterns;

    //
    function TileToScreen(const Tile: Integer): TPoint;
    function ScreenToTile(const X,Y: Integer): Integer;
    // Get the bounding rectangle for a tile
    function TileRect(const Tile: Integer): TRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(const Image: TPHXImage): Boolean;

    // The image containing the tiles
    property Image: TPHXImage read FImage;
    // Image zoom factor
    property Zoom: Single read FZoom write FZoom;

    // List of tiles
    property Tiles: TList<TPHXTile> read FTiles;
    // Width of the tiles
    property TileWidth: Integer read FTileWidth write FTileWidth;
    // Height of the tiles
    property TileHeight: Integer read FTileHeight write FTileHeight;
    // Number of columns to show
    property TileColumns: Integer read FTileColumns write FTileColumns;
  end;

var
  FrmTileEditor: TFrmTileEditor;

implementation

{$R *.dfm}

// TPHXTileComparer
//------------------------------------------------------------------------------
function TPHXTileComparer.Compare(const Left, Right: TPHXTile): Integer;
begin
  Result:= (Left.Index - Right.Index);
end;

// TFrmTileEditor
//==============================================================================
constructor TFrmTileEditor.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer    := TBitmap.Create;
  FBackground:= TBitmap.CreateBackground(4);

  FTiles:= TList<TPHXTile>.Create(TPHXTileComparer.Create);

  FTileWidth  := 32;
  FTileHeight := 32;
  FTileColumns:= 8;

  FZoom    := 1.0;
  FSelected:= -1;

  ScrollBox1.DoubleBuffered:= True;
end;

//------------------------------------------------------------------------------
destructor TFrmTileEditor.Destroy;
begin
  FTiles.Free;

  FBuffer.Free;
  FBackground.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TFrmTileEditor.TilesFromPatterns;
var Index  : Integer;
var Tile   : TPHXTile;
var Pattern: TPHXPattern;
begin
  FTiles.Clear;

  for Index:= 0 to Image.Patterns.Count - 1 do
  begin
    Pattern:= Image.Patterns.List^[Index];

    Tile.Index := Index + 1;
    Tile.Name  := Pattern.Name;
    Tile.X     := Pattern.X;
    Tile.Y     := Pattern.Y;
    Tile.Width := Pattern.Width;
    Tile.Height:= Pattern.Height;
    Tile.Pivot := Pattern.Pivot;
    Tile.Flip  := Pattern.Flip;
    Tile.Mirror:= Pattern.Mirror;

    FTiles.Add(Tile);
  end;

  FTiles.Sort;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.TilesToPatterns;
var Index  : Integer;
var Tile   : TPHXTile;
var Pattern: TPHXPattern;
begin
  Image.Patterns.Clear;

  for Index:= 0 to FTiles.Count - 1 do
  begin
    Tile:= FTiles[Index];

    Pattern.Name  := Tile.Name;
    Pattern.X     := Tile.X;
    Pattern.Y     := Tile.Y;
    Pattern.Width := Tile.Width;
    Pattern.Height:= Tile.Height;
    Pattern.Pivot := Tile.Pivot;
    Pattern.Flip  := Tile.Flip;
    Pattern.Mirror:= Tile.Mirror;

    Image.Patterns.Add(Pattern);
  end;
end;


//------------------------------------------------------------------------------
function TFrmTileEditor.Execute(const Image: TPHXImage): Boolean;
begin
  FImage:= Image;
  FZoom := 1.0;

  FImage.Draw(FBuffer, FBackground);

  // Copy the tile size from the first pattern
  if Image.Patterns.Count > 0 then
  begin
    FTileWidth := Image.Patterns[0].Width;
    FTileHeight:= Image.Patterns[0].Height;
  end;

  edTileName   .Text := Image.Name;
  edTileWidth  .Value:= FTileWidth;
  edTileHeight .Value:= FTileHeight;
  edTileColumns.Value:= FTileColumns;

  // Copy tiles from the image patterns
  TilesFromPatterns;

  ResizePaintbox;

  Result:= ShowModal = mrOk;

  if Result then
  begin
    TilesToPatterns;
  end;
end;



const TileSpacing = 4;
const TileOffset  = 2;

//------------------------------------------------------------------------------
function TFrmTileEditor.TileToScreen(const Tile: Integer): TPoint;
var W, H: Integer;
begin
  W:= Round(FTileWidth  * FZoom) + TileSpacing;
  H:= Round(FTileHeight * FZoom) + TileSpacing;

  Result.X:= TileOffset + (Tile mod TileColumns) * W;
  Result.Y:= TileOffset + (Tile div TileColumns) * H;
end;

//------------------------------------------------------------------------------
function TFrmTileEditor.ScreenToTile(const X, Y: Integer): Integer;
var W, H: Integer;
begin
  W:= Round(FTileWidth  * FZoom) + TileSpacing;
  H:= Round(FTileHeight * FZoom) + TileSpacing;

  Result:= (X - TileOffset) div W + (Y - TileOffset) div H * TileColumns;
end;

//------------------------------------------------------------------------------
function TFrmTileEditor.TileRect(const Tile: Integer): TRect;
var Point: TPoint;
begin
  Point:= TileToScreen(Tile);

  Result.Left  := Point.X;
  Result.Top   := Point.Y;
  Result.Right := Point.X + Round(TileWidth   * FZoom);
  Result.Bottom:= Point.Y + Round(FTileHeight * FZoom);
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.ResizePaintbox;
var W, H: Integer;
begin
  W:= Round(FTileWidth  * FZoom) + TileSpacing;
  H:= Round(FTileHeight * FZoom) + TileSpacing;

  PaintBox1.Width := (2 * TileOffset) + W * TileColumns;
  PaintBox1.Height:= (2 * TileOffset) + H * Ceil(Tiles.Count div TileColumns);

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.DrawTile(Dest: TCanvas; const X,Y: Integer; const Tile: Integer; const Zoom: Single);
var SrcRect: TRect;
var DstRect: TRect;
var Pivot  : TPoint;
begin
  SrcRect.Left  := FTiles[Tile].X;
  SrcRect.Top   := FTiles[Tile].Y;
  SrcRect.Right := FTiles[Tile].X + FTiles[Tile].Width;
  SrcRect.Bottom:= FTiles[Tile].Y + FTiles[Tile].Height;

  DstRect.Left  :=  X;
  DstRect.Top    := Y;
  DstRect.Right  := X + Round(FTiles[Tile].Width  * Zoom);
  DstRect.Bottom := Y + Round(FTiles[Tile].Height * Zoom);

  Dest.CopyRect(DstRect, FBuffer.Canvas, SrcRect);

  // Draw pivot
  Pivot.X:= X + Round(FTiles[Tile].Pivot.X * Zoom);
  Pivot.Y:= Y + Round(FTiles[Tile].Pivot.Y * Zoom);

  Dest.Pen.Style:= psSolid;
  Dest.Pen.Color:= clMaroon;

  Dest.MoveTo(Pivot.X, Y);
  Dest.LineTo(Pivot.X, Y + Round(FTiles[Tile].Height * Zoom));

  Dest.MoveTo(X                                    , Pivot.Y);
  Dest.LineTo(X + Round(FTiles[Tile].Width  * Zoom), Pivot.Y);
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.DrawTiles;
var Index: Integer;
var Rect : TRect;
var Point: TPoint;
begin
  with PaintBox1.Canvas do
  begin
    for Index := 0 to FTiles.Count - 1 do
    begin
      Rect:= TileRect(Index);

      // Draw the tile
      DrawTile(PaintBox1.Canvas, Rect.Left, Rect.Top, Index, FZoom);

      // Draw tile border rectangle
      Pen.Color:= clSilver;
      Pen.Style:= psSolid;

      InflateRect(Rect, 1, 1);

      Rectangle(Rect);

      // Draw selection rectangle
      if Index = FSelected then
      begin
        Pen  .Color:= clBlack;
        Pen  .Style:= psSolid;

        InflateRect(Rect, 1, 1);

        Rectangle(Rect);
      end;

      if cbDrawIndex.Checked then
      begin
        Point:= TileToScreen(Index);

        TextOut(Point.X + 2, Point.Y + 2, IntToStr( FTiles[Index].Index) );
      end;


    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.DrawGrid;
var X,Y: Integer;
var W,H: Integer;
begin
  W:= Trunc(FTileWidth  * Zoom);
  H:= Trunc(FTileHeight * Zoom);

  with PaintBox1.Canvas do
  begin
    Pen.Color:= clBlack;
    Pen.Style:= psDot;

    X:= TileOffset;
    while X + W < PaintBox1.Width  do
    begin
      MoveTo(X, TileOffset);
      LineTo(X, PaintBox1.Height);

      X:= X + W;
    end;

    Y:= TileOffset;
    while Y + H < PaintBox1.Height  do
    begin
      MoveTo(TileOffset     , Y);
      LineTo(PaintBox1.Width, Y);

      Y:= Y + H;
    end;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmTileEditor.PaintBox1Paint(Sender: TObject);
var W, H: Integer;
var Rect : TRect;
begin

  W:= Round(FTileWidth  * FZoom) + TileSpacing;
  H:= Round(FTileHeight * FZoom) + TileSpacing;

  if FTiles.Count = 0 then
  begin
    PaintBox1.Width := 4 + Trunc(Image.Width  * Zoom);
    PaintBox1.Height:= 4 + Trunc(Image.Height * Zoom);
  end else
  begin
    PaintBox1.Width := (2 * TileOffset) + W * TileColumns;
    PaintBox1.Height:= (2 * TileOffset) + H * Ceil(Tiles.Count / TileColumns);
  end;

  with PaintBox1.Canvas do
  begin
    Pen  .Color:= clBlack;
    Pen  .Style:= psSolid;
    Brush.Style:= bsClear;

    if FTiles.Count = 0 then
    begin
      Rect.Left  := TileOffset;
      Rect.Top   := TileOffset;
      Rect.Right := TileOffset + Trunc(Image.Width  * Zoom);
      Rect.Bottom:= TileOffset + Trunc(Image.Height * Zoom);

      StretchDraw(Rect, FBuffer);

      DrawGrid;
    end else
    begin
      DrawTiles;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Tile: Integer;
begin
  if Button = TMouseButton.mbLeft then
  begin
    Tile:= ScreenToTile(X,Y);

    if Tile <> FSelected then
    begin
      FSelected:= Tile;

      PaintBox1.Invalidate;
    end;
  end;

  Self.SetFocus;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Tile: Integer;
begin
  if ssLeft in Shift then
  begin
    Tile:= ScreenToTile(X,Y);

    if Tile <> FSelected then
    begin
      FSelected:= Tile;

      PaintBox1.Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = Winapi.Windows.VK_INSERT then
  begin
    actTileAdd.Execute;
  end;
  if Key = Winapi.Windows.VK_DELETE then
  begin
    actTileDelete.Execute;
  end;

end;


//------------------------------------------------------------------------------
procedure TFrmTileEditor.edTileWidthChange(Sender: TObject);
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
procedure TFrmTileEditor.actTileUpdate(Sender: TObject);
begin
  actTileDelete.Enabled:= (FSelected >= 0) and (FSelected < FTiles.Count);
end;

var TileX: Integer = 0;
var TileY: Integer = 0;

var DialogX: Integer = 0;
var DialogY: Integer = 0;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.actTileAddExecute(Sender: TObject);
var Dialog: TFrmTileSelect;
var Tile   : TPHXTile;
begin
  Dialog:= TFrmTileSelect.Create(Self);
  try
//    Dialog.Width     := Min(640, Self.Width  - 200);
//    Dialog.Height    := Min(480, Self.Height - 200);

    Dialog.TileName  := 'Tile' + IntToStr(Tiles.Count + 1);
    Dialog.TileX     := TileX;
    Dialog.TileY     := TileY;
    Dialog.TileWidth := TileWidth;
    Dialog.TileHeight:= TileHeight;

    if Dialog.Execute(Image) then
    begin
      Tile.Index  :=             Tiles.Count + 1;
      Tile.Name   := ShortString(Dialog.TileName);
      Tile.X      :=             Dialog.TileX;
      Tile.Y      :=             Dialog.TileY;
      Tile.Width  :=             Dialog.TileWidth;
      Tile.Height :=             Dialog.TileHeight;
      Tile.Pivot.X:=             0;
      Tile.Pivot.Y:=             0;
      Tile.Flip   :=             False;
      Tile.Mirror :=             False;

      case edTileMode.ItemIndex of
        // rectangular
        0:
        begin
          Tile.Pivot.X:= 0;
          Tile.Pivot.Y:= Tile.Height - FTileHeight;
        end;
        // Isometric
        1:
        begin
          Tile.Pivot.X:= FTileWidth div 2;
          Tile.Pivot.Y:= Tile.Height - FTileHeight;
        end;
      end;


      FTiles.Add(Tile);

      TileX:= Dialog.TileX;
      TileY:= Dialog.TileY;

      PaintBox1.Invalidate;
    end;
  finally
    Dialog.Free;
  end;

  FTiles.Sort;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.actTileDeleteExecute(Sender: TObject);
begin
  FTiles.Delete(FSelected);

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.edTileHeightChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edTileHeight.Value);

  if Value <> FTileHeight then
  begin
    FTileHeight:= Value;

    PaintBox1.Invalidate;
  end
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.edTileColumnsChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edTileColumns.Value);

  if Value <> FTileColumns then
  begin
    FTileColumns:= Value;

    PaintBox1.Invalidate;
  end
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.btnZoomInClick(Sender: TObject);
begin
  FZoom:= FZoom * 2;

  ResizePaintbox;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.btnZoomOutClick(Sender: TObject);
begin
  FZoom:= FZoom * 0.5;

  ResizePaintbox;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.btnZoom100Click(Sender: TObject);
begin
  FZoom:= 1;

  ResizePaintbox;
end;

//------------------------------------------------------------------------------
procedure TFrmTileEditor.cbDrawIndexClick(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;




end.
