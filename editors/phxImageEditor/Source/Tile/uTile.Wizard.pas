unit uTile.Wizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, ComCtrls, ToolWin, Mask,

  JvExMask, JvSpin,

  Generics.Collections,

  phxTypes,
  phxClasses,
  phxEditor,

  phxGraphicsEx,

  phxImage,
  phxImageEx;




type

//------------------------------------------------------------------------------
TfrmTileWizard = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Panel2: TPanel;
    brnOkey: TButton;
    Panel3: TPanel;
    Label11: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    cbAppend: TRadioButton;
    RadioButton2: TRadioButton;
    ImageList2: TImageList;
    Label6: TLabel;
    Label7: TLabel;
    edTileName: TEdit;
    cbDrawIndex: TCheckBox;
    Label8: TLabel;
    edOffsetX: TJvSpinEdit;
    edOffsetY: TJvSpinEdit;
    Label9: TLabel;
    Label10: TLabel;
    ToolBar1: TToolBar;
    btnZoomIn: TToolButton;
    btnZoom100: TToolButton;
    btnZoomOut: TToolButton;
    edWidth: TJvSpinEdit;
    edHeight: TJvSpinEdit;
    edHorzSpace: TJvSpinEdit;
    edVertSpace: TJvSpinEdit;
    edSkip: TJvSpinEdit;
    edMaxTiles: TJvSpinEdit;
    edMaxCols: TJvSpinEdit;
    edMaxRows: TJvSpinEdit;
    btnCancel: TButton;
    edCenterPivots: TCheckBox;
    procedure PaintBox1Paint(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoom100Click(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure cbDrawIndexClick(Sender: TObject);
  private
    FImage: TPHXImage;
    FBuffer: TBitmap;
    FBackground : TBitmap;

    FTiles: TList<TRecti>;

    procedure TilesToPatterns;

    procedure SetImage(const Value: TPHXImage);
  public
    Zoom     : Single;
    TileWidth : Integer;
    TileHeight: Integer;
    TileHorzSpace: Integer;
    TileVertSpace: Integer;

    OffsetHorz : Integer;
    OffsetVert: Integer;

    MaxTiles: Integer;
    MaxColumns: Integer;
    MaxRows   : Integer;
    NumSkip   : Integer;

    procedure GenerateTiles;

    function Execute(const Image: TPHXImage): Boolean;

    property Image: TPHXImage read FImage write SetImage;

    property Tiles: TList<TRecti> read FTiles;
  end;

var
  frmTileWizard: TfrmTileWizard;

implementation

{$R *.dfm}

// TfrmTileWizard
//==============================================================================
procedure TfrmTileWizard.FormCreate(Sender: TObject);
begin
  FBuffer:= TBitmap.Create;

  FBackground:= CreateTransparentImage(4);

  FTiles:= TList<TRecti>.Create;

  ScrollBox1.DoubleBuffered:= True;
end;

//------------------------------------------------------------------------------
procedure TfrmTileWizard.FormDestroy(Sender: TObject);
begin
  FBackground.Free;
  FBuffer.Free;
  FTiles.Free;
end;


//------------------------------------------------------------------------------
function TfrmTileWizard.Execute(const Image: TPHXImage): Boolean;
begin
  Zoom:= 1;

  SetImage(Image);

  Result:= ShowModal = mrOk;

  if Result then
  begin
    TilesToPatterns;
  end;
end;


//------------------------------------------------------------------------------
procedure TfrmTileWizard.TilesToPatterns;
var Index: Integer;
var Tile : TRecti;
var Pattern : TPHXPattern;
begin
  if not cbAppend.Checked then
  begin
    Image.Patterns.Clear;
  end;

  for Index := 0 to Tiles.Count - 1 do
  begin
    Tile:= Tiles[Index];

    Pattern.Name   := AnsiString(Format(edTileName.Text, [Index+1]));
    Pattern.X      := Tile.Left;
    Pattern.Y      := Tile.Top;
    Pattern.Width  := Tile.Right  - Tile.Left;
    Pattern.Height := Tile.Bottom - Tile.Top;

    if edCenterPivots.Checked then
    begin
      Pattern.Pivot.X:= Pattern.Width  div 2;
      Pattern.Pivot.Y:= Pattern.Height div 2;
    end else
    begin
      Pattern.Pivot.X:= 0;
      Pattern.Pivot.Y:= 0;
    end;

    Pattern.Flip  := False;
    Pattern.Mirror:= False;

    Image.Patterns.Add(Pattern);
  end;
end;


//------------------------------------------------------------------------------
procedure TfrmTileWizard.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  Image.Draw(FBuffer, FBackground);

  EditChange(nil);


  PaintBox1.Width := Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= Trunc(Image.Height * Zoom);
  PaintBox1.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TfrmTileWizard.EditChange(Sender: TObject);
begin
  TileWidth    := StrToIntDef( edWidth .Text, 16);
  TileHeight   := StrToIntDef( edHeight.Text, 16);
  TileHorzSpace:= StrToIntDef( edHorzSpace .Text, 0);
  TileVertSpace:= StrToIntDef( edVertSpace .Text, 0);

  NumSkip   :=  StrToIntDef( edSkip     .Text, 0);
  MaxTiles  :=  StrToIntDef( edMaxTiles .Text, -1);
  MaxColumns:=  StrToIntDef( edMaxCols  .Text, -1);
  MaxRows   :=  StrToIntDef( edMaxRows  .Text, -1);

  OffsetHorz := Trunc(edOffsetX.Value);
  OffsetVert := Trunc(edOffsetY.Value);

  GenerateTiles;
  
  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TfrmTileWizard.PaintBox1Paint(Sender: TObject);
var Index: Integer;
var Tile : TRecti;
var Rect: TRect;
begin
  with PaintBox1.Canvas do
  begin
    Draw(0,0, FBuffer);

    Rect.Left  := 0;
    Rect.Top   := 0;
    Rect.Right := Trunc(Image.Width  * Zoom);
    Rect.Bottom:= Trunc(Image.Height * Zoom);

    StretchDraw(Rect, FBuffer);

    Pen  .Color:= clBlack;
    Pen  .Style:= psSolid;
    Brush.Style:= bsClear;

    for Index := 0 to Tiles.Count - 1 do
    begin
      Tile:= Tiles[Index];
      Tile.Left  := Trunc(Tile.Left    * Zoom);
      Tile.Top   := Trunc(Tile.Top     * Zoom);
      Tile.Right := Trunc(Tile.Right   * Zoom);
      Tile.Bottom:= Trunc(Tile.Bottom  * Zoom);

   //   Rectangle(Tile.Left, Tile.Top, Tile.Right, Tile.Bottom);

    //  Tile.Left  := Tile.Left    +1;
     // Tile.Top   := Tile.Top     +1;
     // Tile.Right := Tile.Right   -1;
     // Tile.Bottom:= Tile.Bottom  -1;
    //
   //   Rectangle(Tile.Left, Tile.Top, Tile.Right, Tile.Bottom);

      Pen  .Color:= clBlack;
      MoveTo(Tile.Left, Tile.Top); LineTo(Tile.Right, Tile.Top);
      MoveTo(Tile.Left, Tile.Top); LineTo(Tile.Left, Tile.Bottom);

      Pen  .Color:= $00202020;
      MoveTo(Tile.Left , Tile.Bottom); LineTo(Tile.Right, Tile.Bottom);
      MoveTo(Tile.Right, Tile.Top   ); LineTo(Tile.Right, Tile.Bottom);

      if cbDrawIndex.Checked then
      begin
        TextOut(Tile.Left + 2, Tile.Top + 2, IntToStr(Index+1));
      end;
    end;
  end;
end;



//------------------------------------------------------------------------------
procedure TfrmTileWizard.GenerateTiles;
var Index: Integer;
var IX, IY: Integer;
var DX, DY: Integer;
var Tile: TRecti;
begin

  // Avoid infinite loops
  if(TileWidth  < 1) then
  begin
    DX := 1;
  end else
  begin
    DX := TileWidth + TileHorzSpace;
  end;

  if(TileHeight < 1) then
  begin
    DY := 1;
  end else
  begin
    DY := TileHeight + TileVertSpace;
  end;

  // Remove all current patterns
  Tiles.Count:= 0;

  Index:= 0;

  IY:=OffsetVert;
  while IY + TileHeight <=Image.Height do
  begin
    IX:=OffsetHorz;

    while IX + TileWidth <= Image.Width do
    begin
      Tile.Left    := IX;
      Tile.Top     := IY;
      Tile.Right   := IX + TileWidth;
      Tile.Bottom  := IY + TileHeight;

      if(Index >= NumSkip) then Tiles.Add(Tile);

      if (MaxTiles > 0) and (Tiles.Count >= MaxTiles) then Exit;

      Inc(IX, DX);

      if (MaxColumns > 0) and (IX >= MaxColumns * DX) then Break;

      Inc(Index);
    end;
    Inc(IY, DY);

    if (MaxRows > 0) and (IY >= MaxRows * DY) then Break;
  end;
end;


//------------------------------------------------------------------------------
procedure TfrmTileWizard.btnZoomInClick(Sender: TObject);
begin
  Zoom:= Zoom * 2;

  PaintBox1.Width := Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= Trunc(Image.Height * Zoom);
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TfrmTileWizard.btnZoom100Click(Sender: TObject);
begin
  Zoom:= 1;

  PaintBox1.Width := Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= Trunc(Image.Height * Zoom);
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TfrmTileWizard.btnZoomOutClick(Sender: TObject);
begin
  Zoom:= Zoom * 0.5;

  PaintBox1.Width := Trunc(Image.Width  * Zoom);
  PaintBox1.Height:= Trunc(Image.Height * Zoom);
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TfrmTileWizard.cbDrawIndexClick(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;


end.
