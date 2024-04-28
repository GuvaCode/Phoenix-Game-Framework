unit uGraph1f;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ComCtrls, ToolWin, ExtCtrls, ImgList, JvExMask,
  JvSpin,

  phxTypes,
  phxClasses,

  phxParticle,
  phxParticleGraphs;

type

//------------------------------------------------------------------------------
TPnlGraph1f = class(TCustomControl)
  private
    FGraph : TPHXGraph1f;
    FValues: TPHXSingleList;
    FBorder: TRecti;
    FZoomMin: Single;
    FZoomMax: Single;
    FSelected: Integer;

    procedure SetGraph(const Value: TPHXGraph1f);

    procedure PaintAxes;
    procedure PaintGrid;
    procedure PaintGraph;
    procedure SetZoomMax(const Value: Single);
    procedure SetZoomMin(const Value: Single);
    procedure SetSelected(const Value: Integer);
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;

    // Convert a screen coordinate to a graph key
    function ScreenToGraph(const Screen: TVector2f): TPHXGraphKey1f; overload;
    function ScreenToGraph(const X, Y  : Single   ): TPHXGraphKey1f; overload;
    // Convert a graph key to screen coordinates
    function GraphToScreen(const Key: TPHXGraphKey1f): TVector2f; overload;
    function GraphToScreen(const Time, Value: Single): TVector2f; overload;

    function ValueAt(const Time: Single): Single;

    // Get the key at a given location
    function KeyAt(const Screen: TVector2f): Integer; overload;
    function KeyAt(const X, Y  : Single   ): Integer; overload;

    procedure ZoomFit;

    property Graph: TPHXGraph1f read FGraph write SetGraph;
    // Selected key index
    property Selected: Integer read FSelected write SetSelected;

    property Border: TRecti read FBorder write FBorder;

    // Maximum value to show
    property ZoomMax: Single read FZoomMax write SetZoomMax;
    // Minimum value to show
    property ZoomMin: Single read FZoomMin write SetZoomMin;
  end;

//------------------------------------------------------------------------------
TFrmGraph1f = class(TFrame)
    ImageList1: TImageList;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    ToolBar1: TToolBar;
    btnPointer: TToolButton;
    btnMove: TToolButton;
    ToolButton9: TToolButton;
    btnAdd: TToolButton;
    ToolButton2: TToolButton;
    btnInterpolateCubic: TToolButton;
    btnInterpolateLinear: TToolButton;
    btnInterpolateStep: TToolButton;
    ToolButton6: TToolButton;
    btnDelete: TToolButton;
    ToolButton1: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    edTime: TJvSpinEdit;
    edValue: TJvSpinEdit;
    ToolButton3: TToolButton;
    btnSave: TToolButton;
    procedure GraphPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GraphPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GraphPanelMouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);

    procedure edValueChange(Sender: TObject);

    procedure btnPointerClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnInterpolateCubicClick(Sender: TObject);
    procedure btnInterpolateLinearClick(Sender: TObject);
    procedure btnInterpolateStepClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FGraph   : TPHXGraph1f;
    FValues  : TPHXSingleList;
    FPanel    : TPnlGraph1f;
    FSelected: Integer;
    FOnChanged: TNotifyEvent;

    procedure Changed;



    procedure SetGraph(const Value: TPHXGraph1f);
    procedure SetSelected(const Value: Integer);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Panel: TPnlGraph1f read FPanel write FPanel;

    property Graph: TPHXGraph1f read FGraph write SetGraph;
    // Selected key index
    property Selected: Integer read FSelected write SetSelected;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

{ TFrmGraph1f }


const DEFAULT_BORDER_LEFT    = 30;
const DEFAULT_BORDER_TOP     = 4;
const DEFAULT_BORDER_RIGHT   = 4;
const DEFAULT_BORDER_BOTTOM  = 17;

//------------------------------------------------------------------------------
constructor TFrmGraph1f.Create(AOwner: TComponent);
begin
  inherited;

  Panel:= TPnlGraph1f.Create(Self);
  Panel.Parent:= Self;
  Panel.Align:= alClient;
  Panel.OnMouseDown:= GraphPanelMouseDown;
  Panel.OnMouseMove:= GraphPanelMouseMove;
  Panel.OnMouseUp  := GraphPanelMouseUp;

  SetGraph(nil);
end;

//------------------------------------------------------------------------------
destructor TFrmGraph1f.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.Changed;
begin
  FGraph.Expand;

  if Assigned(FOnChanged) then FOnChanged(Self);
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.GraphPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Key: TPHXGraphKey1f;
begin
  if Graph = nil then Exit;

  Key:= Panel.ScreenToGraph( X, Y);

  if btnMove.Down and (ssLeft in Shift) then
  begin
    SetSelected( Panel.KeyAt( X, Y) );
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmGraph1f.GraphPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var GraphKey: TPHXGraphKey1f;
begin
  if Graph = nil then Exit;


  // Trunc the time
 // if GraphKey.Time < 0.0 then GraphKey.Time:= 0.0;
 // if GraphKey.Time > 1.0 then GraphKey.Time:= 1.0;

  // Trunc the value
//  if GraphKey.Value < Graph.MinValue then GraphKey.Value:= Graph.MinValue;
//  if GraphKey.Value > Graph.MaxValue then GraphKey.Value:= Graph.MaxValue;

 // StatusBar1.Panels[0].Text:= Format('X: %.2f Y: %.2f', [GraphKey.Time, GraphKey.Value]);

 // StatusBar1.Panels[1].Text:= Format('Value: %.2f', [ Panel.ValueAt(GraphKey.Time)]);

  // Change the cursor
  if btnPointer.Down then
  begin
    Panel.Cursor:= crDefault;
  end else
  if btnMove.Down and (Panel.KeyAt( X, Y) >= 0) then
  begin
    Panel.Cursor:= crSizeAll;
  end else
  if btnAdd.Down then
  begin
    Panel.Cursor:= crCross;
  end else
  begin
    Panel.Cursor:= crDefault;
  end;

  if ssLeft in Shift then
  begin

    if btnMove.Down and (Selected >=0) and (Selected < Graph.Keys.Count) then
    begin
      GraphKey:= Panel.ScreenToGraph( X, Y);

      if GraphKey.Time < 0.0 then GraphKey.Time:= 0.0;
      if GraphKey.Time > 1.0 then GraphKey.Time:= 1.0;

      if GraphKey.Value < Graph.MinValue then GraphKey.Value:= Graph.MinValue;
      if GraphKey.Value > Graph.MaxValue then GraphKey.Value:= Graph.MaxValue;

      Graph.Keys[Selected]:= GraphKey;

      SetSelected(Selected);

      Changed;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.GraphPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var GraphKey        : TPHXGraphKey1f;
var GraphKeyPosition: TVector2f;
begin
  if Graph = nil then Exit;

  GraphKey:= Panel.ScreenToGraph( X, Y);

  // Trunc the time
  if GraphKey.Time < 0.0 then GraphKey.Time:= 0.0;
  if GraphKey.Time > 1.0 then GraphKey.Time:= 1.0;

  // Trunc the value
  if GraphKey.Value < Graph.MinValue then GraphKey.Value:= Graph.MinValue;
  if GraphKey.Value > Graph.MaxValue then GraphKey.Value:= Graph.MaxValue;


  // Select keys
  if btnPointer.Down then
  begin
    SetSelected( Panel.KeyAt( X, Y) );
  end else
  // Add keys
  if btnAdd.Down then
  begin
    GraphKeyPosition:= Panel.GraphToScreen(GraphKey);

    Graph.Keys.Add(GraphKey);
    Graph.Keys.SortByTime;

    SetSelected( Panel.KeyAt( GraphKeyPosition));

    Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmGraph1f.KeyUp(var Key: Word; Shift: TShiftState);
var GraphKey: TPHXGraphKey1f;
begin
  inherited;

  if Key = Ord('P') then btnPointerClick(Self);
  if Key = Ord('M') then btnMoveClick   (Self);
  if Key = Ord('A') then btnAddClick    (Self);

  if (Selected >=0) and (Selected < Graph.Keys.Count) then
  begin
    GraphKey:= Graph.Keys[Selected];

    if Key = Windows.VK_LEFT  then GraphKey.Time:= GraphKey.Time - 0.1;
    if Key = Windows.VK_RIGHT then GraphKey.Time:= GraphKey.Time + 0.1;

    if Key = Windows.VK_UP    then GraphKey.Value:= GraphKey.Value + 0.1;
    if Key = Windows.VK_DOWN  then GraphKey.Value:= GraphKey.Value - 0.1;

    // Trunc the time
    if GraphKey.Time < 0.0 then GraphKey.Time:= 0.0;
    if GraphKey.Time > 1.0 then GraphKey.Time:= 1.0;

    // Trunc the value
    if GraphKey.Value < Graph.MinValue then GraphKey.Value:= Graph.MinValue;
    if GraphKey.Value > Graph.MaxValue then GraphKey.Value:= Graph.MaxValue;

    Graph.Keys[Selected]:= GraphKey;

    Panel.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.SetGraph(const Value: TPHXGraph1f);
begin
  if FGraph <> Value then
  begin
    FGraph := Value;

    SetSelected(-1);
  end;

  if Assigned(FGraph) then
  begin
    btnPointer.Enabled:= True;
    btnMove   .Enabled:= True;
    btnAdd    .Enabled:= True;
    btnDelete .Enabled:= True;
    btnSave   .Enabled:= True;

    btnInterpolateCubic .Enabled:= True;
    btnInterpolateLinear.Enabled:= True;
    btnInterpolateStep  .Enabled:= True;

    btnZoomIn .Enabled:= Panel.ZoomMax < Graph.MaxValue;
    btnZoomOut.Enabled:= Panel.ZoomMax > 1.0;

    btnPointer.Down:= True;
    btnMove   .Down:= False;
    btnAdd    .Down:= False;

    btnInterpolateCubic .Down:= Graph.Interpolation = ipCubic;
    btnInterpolateLinear.Down:= Graph.Interpolation = ipLinear;
    btnInterpolateStep  .Down:= Graph.Interpolation = ipNone;
  end else
  begin
    btnPointer.Enabled:= False;
    btnMove   .Enabled:= False;
    btnAdd    .Enabled:= False;
    btnDelete .Enabled:= False;
    btnSave   .Enabled:= False;

    edTime .Enabled:= False;
    edValue.Enabled:= False;

    edTime .Color:= clBtnFace;
    edValue.Color:= clBtnFace;

    btnZoomIn .Enabled:= False;
    btnZoomOut.Enabled:= False;

    btnInterpolateCubic .Enabled:= False;
    btnInterpolateLinear.Enabled:= False;
    btnInterpolateStep  .Enabled:= False;
  end;
  Panel.Graph:= Graph;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.SetSelected(const Value: Integer);
var Key: TPHXGraphKey1f;
begin
  FSelected := Value;

  edTime .OnChange:= nil;
  edValue.OnChange:= nil;
  if Assigned(Graph) and (Selected >=0) and (Selected < Graph.Keys.Count) then
  begin
    btnDelete.Enabled:= True;

    edTime .Enabled:= True;
    edValue.Enabled:= True;

    edTime .Color := clWindow;
    edValue.Color := clWindow;

    Key:= Graph.Keys[Selected];

    edTime .Value:= Key.Time  * 100;
    edValue.Value:= Key.Value;

    edTime .OnChange:= edValueChange;
    edValue.OnChange:= edValueChange;
  end else
  begin
    btnDelete .Enabled:= False;

    edTime .Enabled:= False;
    edValue.Enabled:= False;

    edTime .Color := clBtnFace;
    edValue.Color := clBtnFace;
  end;
  Panel.Selected:= Selected;
end;


//------------------------------------------------------------------------------
procedure TFrmGraph1f.edValueChange(Sender: TObject);
var Key: TPHXGraphKey1f;
begin
  if Assigned(Graph) and (Selected >=0) and (Selected < Graph.Keys.Count) then
  begin
    Key.Time := edTime .Value / 100;
    Key.Value:= edValue.Value;

    Graph.Keys[Selected]:= Key;

    Changed;

    Panel.Invalidate;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnPointerClick(Sender: TObject);
begin
  btnPointer.Down:= not btnPointer.Down;
  btnAdd    .Down:= False;
  btnMove   .Down:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnAddClick(Sender: TObject);
begin
  btnAdd    .Down:= not btnAdd.Down;
  btnPointer.Down:= False;
  btnMove   .Down:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnMoveClick(Sender: TObject);
begin
  btnMove   .Down:= not btnMove.Down;
  btnPointer.Down:= False;
  btnAdd    .Down:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnDeleteClick(Sender: TObject);
begin
  if Assigned(Graph) and (Selected >=0) and (Selected < Graph.Keys.Count) then
  begin
    Graph.Keys.Delete(Selected);

    SetSelected(-1);

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnInterpolateCubicClick(Sender: TObject);
begin
  Graph.Interpolation:= ipCubic;

  SetGraph(Graph);

  Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnInterpolateLinearClick(Sender: TObject);
begin
  Graph.Interpolation:= ipLinear;

  SetGraph(Graph);

  Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnInterpolateStepClick(Sender: TObject);
begin
  Graph.Interpolation:= ipNone;

  SetGraph(Graph);

  Changed;
end;


//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnZoomInClick(Sender: TObject);
begin
  Panel.ZoomMax:= Panel.ZoomMax * 2.0;

  btnZoomIn .Enabled:= Panel.ZoomMax < Graph.MaxValue;
  btnZoomOut.Enabled:= Panel.ZoomMax > 1.0;
end;


//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnZoomOutClick(Sender: TObject);
begin
  Panel.ZoomMax:= Panel.ZoomMax * 0.5;

  btnZoomIn .Enabled:= Panel.ZoomMax < Graph.MaxValue;
  btnZoomOut.Enabled:= Panel.ZoomMax > 1.0;
end;

//------------------------------------------------------------------------------
procedure TFrmGraph1f.btnSaveClick(Sender: TObject);
begin
  Graph.Expand(Graph.Count);
end;


// TPnlGraph1f
//------------------------------------------------------------------------------
constructor TPnlGraph1f.Create(AOwner: TComponent);
begin
  inherited;
  FValues:= TPHXSingleList.Create;

  FBorder.Left  := DEFAULT_BORDER_LEFT;
  FBorder.Top   := DEFAULT_BORDER_TOP;
  FBorder.Right := DEFAULT_BORDER_RIGHT;
  FBorder.Bottom:= DEFAULT_BORDER_BOTTOM;

  FZoomMin:= 0;
  FZoomMax:= 1.0;

  DoubleBuffered:= True;
end;

//------------------------------------------------------------------------------
destructor TPnlGraph1f.Destroy;
begin
  FValues.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPnlGraph1f.Click;
begin
  inherited;
  SetFocus;
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.Invalidate;
begin
  inherited;

  if Assigned(Graph) then
  begin
    FValues.Count:= Width - Border.Left - Border.Right;

    Graph.Expand(FValues.Count, FValues.List);
  end else
  begin
    FValues.Count:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.Paint;
var Key: TPHXGraphKey1f;
var Position : TVector2f;
begin
  with Canvas do
  begin
    Brush.Color:= clWhite;

    FillRect(ClipRect);
  end;

  PaintAxes;
  PaintGrid;

  if Assigned(Graph) then
  begin
    PaintGraph;

    if (Selected >=0) and (Selected < Graph.Keys.Count) then
    begin
      Key:= Graph.Keys[Selected];

      Position:= GraphToScreen(Key);

      Canvas.Pen.Color:= clBlack;
      Canvas.Pen.Style:= psSolid;

      Canvas.Brush.Color:= clGray;
      Canvas.Brush.Style:= bsSolid;

      Canvas.Rectangle( Round(Position.X - 3), Round(Position.Y - 3), Round(Position.X + 4), Round(Position.Y + 4));
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.PaintAxes;
var Index   : Integer;
var Position: TVector2f;
var X,Y     : Integer;
var Value   : Single;
var Text    : String;
begin
  Canvas.Pen.Color:= clBlack;
  Canvas.Pen.Style:= psSolid;

  Canvas.MoveTo(Border.Left, Border.Top);
  Canvas.LineTo(Border.Left, Height - Border.Bottom);

  Canvas.MoveTo(Border.Left, Height - Border.Bottom);
  Canvas.LineTo(Width - Border.Right, Height - Border.Bottom);



  Canvas.Brush.Style:= bsClear;
  for Index := 0 to 10 do
  begin
    if Assigned(Graph) then
    begin
      Position:= GraphToScreen(Index / 10, 0);
    end else
    begin
      Position.X:= Border.Left + Width * (Index / 10);
    end;

    Text:= IntToStr(Index * 10);

    X:= Round(Position.X);
    Y:= Height - Border.Bottom;

    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X, Y + 3);

    X:= X + 1 - Canvas.TextHeight(Text) div 2;
    Y:= Y + 2;

    if X + Canvas.TextWidth(Text) > Width then X:= Width - Canvas.TextWidth(Text);

    Canvas.TextOut(X, Y, Text);
  end;
  for Index := 0 to 2 do
  begin
    if Assigned(Graph) then
    begin
      Value:= ZoomMin + (Index / 2) * (ZoomMax - ZoomMin);
    end else
    begin
      Value:= Index / 2;
    end;
    Position:= GraphToScreen(0, Value );

    Text:= Format('%.1f', [Value]);

    X:= Border.Left;
    Y:= Round(Position.Y);

    Canvas.MoveTo(X-2, Y);
    Canvas.LineTo(X  , Y);

    X:= X - 4 - Canvas.TextWidth (Text);
    Y:= Y - 1 - Canvas.TextHeight(Text) div 2;

    if Y < 0 then Y:= 0;


    Canvas.TextOut(X, Y, Text);
  end;

////  Position:= GraphToScreen(0, Graph.MinValue);
 // Position:= GraphToScreen(0, Graph.MaxValue);
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.PaintGrid;
var Index   : Integer;
var Position: TVector2f;
begin
  Canvas.Pen.Color:= clSilver;
  Canvas.Pen.Style:= psSolid;
  for Index := 1 to 10 do
  begin
    if Assigned(Graph) then
    begin
      Position:= GraphToScreen(Index / 10, 0);
    end else
    begin
      Position.X:= Width * (Index / 10);
    end;

    Canvas.MoveTo( Round(Position.X), Border.Top);
    Canvas.LineTo( Round(Position.X), Height - Border.Bottom);
  end;

  for Index := 1 to 2 do
  begin
    if Assigned(Graph) then
    begin
      Position:= GraphToScreen(0, ZoomMin + (Index / 2) *( ZoomMax - ZoomMin) );
    end else
    begin
      Position:= GraphToScreen(0, Index / 2 );
    end;

    Canvas.MoveTo( Border.Left + 1   , Round(Position.Y));
    Canvas.LineTo( Width-Border.Right, Round(Position.Y));
  end;
end;


//------------------------------------------------------------------------------
procedure TPnlGraph1f.PaintGraph;
var Index   : Integer;
var Key      : TPHXGraphKey1f;
var Position : TVector2f;
begin
  Canvas.Pen.Color:= clMaroon;
  Canvas.Pen.Style:= psSolid;

  Canvas.Brush.Color:= clWhite;
  Canvas.Brush.Style:= bsSolid;


  for Index := 0 to FValues.Count - 1 do
  begin
    Key.Time := Index / FValues.Count;
    Key.Value:= FValues[Index];

    Position:= GraphToScreen(Key);

    if Index = 0 then
    begin
      Canvas.MoveTo( Round(Position.X), Round(Position.Y));
    end else
    begin
      Canvas.LineTo( Round(Position.X), Round(Position.Y));
    end;
  end;

  Canvas.Pen.Color:= clBlack;
  for Index := 0 to Graph.Keys.Count - 1 do
  begin
    Key:= Graph.Keys[Index];

    Position:= GraphToScreen(Key);

    Canvas.Rectangle( Round(Position.X - 3), Round(Position.Y - 3), Round(Position.X + 4), Round(Position.Y + 4));

   //   Graph.Keys[Index].Value;
  end;


end;


//------------------------------------------------------------------------------
function TPnlGraph1f.ValueAt(const Time: Single): Single;
var Index: Integer;
begin
  Index := Round(Time * FValues.Count);

  if Index <  0             then Index:= 0;
  if Index >= FValues.Count then Index:= FValues.Count - 1;

  Result:=FValues[Index];
end;


//------------------------------------------------------------------------------
function TPnlGraph1f.KeyAt(const Screen: TVector2f): Integer;
var Index   : Integer;
var Key      : TPHXGraphKey1f;
var Position : TVector2f;
begin
  for Index := 0 to Graph.Keys.Count - 1 do
  begin
    Key:= Graph.Keys[Index];

    Position:= GraphToScreen(Key);

    if (Screen.X > Position.X - 3) and
       (Screen.Y > Position.Y - 3) and
       (Screen.X < Position.X + 4) and
       (Screen.Y < Position.Y + 4) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPnlGraph1f.KeyAt(const X, Y: Single): Integer;
begin
  Result:= KeyAt( Vector2f(X, Y ) );
end;



//------------------------------------------------------------------------------
procedure TPnlGraph1f.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  TFrmGraph1f(Owner).KeyUp(Key, Shift);
end;

//------------------------------------------------------------------------------
function TPnlGraph1f.GraphToScreen(const Key: TPHXGraphKey1f): TVector2f;
begin
  Result.X:= Border.Left + Key.Time  * (Width  - (Border.Right  + Border.Left));
  Result.Y:= (Height - Border.Bottom) - ((Key.Value - ZoomMin) / (ZoomMax - ZoomMin) ) * (Height - (Border.Bottom + Border.Top));
end;

//------------------------------------------------------------------------------
function TPnlGraph1f.GraphToScreen(const Time, Value: Single): TVector2f;
var Key: TPHXGraphKey1f;
begin
  Key.Time := Time;
  Key.Value:= Value;

  Result:= GraphToScreen( Key );
end;

//------------------------------------------------------------------------------
function TPnlGraph1f.ScreenToGraph(const Screen: TVector2f): TPHXGraphKey1f;
begin
  Result.Time := (Screen.X - Border.Left) / (Width  - (Border.Right  + Border.Left));
  Result.Value:= ZoomMin + (ZoomMax - ZoomMin) * (Height - Screen.Y - Border.Bottom) / (Height  - (Border.Bottom  + Border.Top));
end;

//------------------------------------------------------------------------------
function TPnlGraph1f.ScreenToGraph(const X, Y: Single): TPHXGraphKey1f;
begin
  Result:= ScreenToGraph( Vector2f(X, Y ) );
end;


//------------------------------------------------------------------------------
procedure TPnlGraph1f.ZoomFit;
var Index: Integer;
var Value: Single;
begin
  if Graph = nil then Exit;

  FZoomMin:= Graph.MinValue;
  FZoomMax:= Graph.MinValue + 1.0;

  for Index := 0 to Graph.Keys.Count - 1 do
  begin
    Value:= Graph.Keys[Index].Value;

    if Value > FZoomMax then
    begin
      FZoomMax:= Value;
    end;

  end;

end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.SetGraph(const Value: TPHXGraph1f);
var w: Integer;
begin
  FGraph := Value;

  FBorder.Left:= DEFAULT_BORDER_LEFT;

  if Assigned(FGraph) then
  begin
   // w:= Canvas.TextWidth( Format('%.1f', [Graph.MaxValue]));

    //if w + 8 > Border.Left then FBorder.Left:= w + 8;

    //FZoomMax:= Graph.MaxValue;
   // FZoomMin:= Graph.MinValue;
    ZoomFit;
  end;



  Resize;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.SetSelected(const Value: Integer);
begin
  FSelected := Value;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.SetZoomMax(const Value: Single);
begin
  FZoomMax := Value;

  if Assigned(Graph) and (ZoomMax > Graph.MaxValue) then
  begin
    FZoomMax:= Graph.MaxValue;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPnlGraph1f.SetZoomMin(const Value: Single);
begin
  FZoomMin := Value;

  if Assigned(Graph) and (ZoomMin > Graph.MinValue) then
  begin
    FZoomMin:= Graph.MinValue;
  end;
  Invalidate;
end;



end.
