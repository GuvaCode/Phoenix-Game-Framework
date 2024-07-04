unit uGraphRGB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin, ExtCtrls, ComCtrls, ToolWin, ImgList,

  phxTypes,
  phxClasses,

  phxParticle,
  phxParticleGraphs;

type

//------------------------------------------------------------------------------
TPnlGraphRGB = class(TCustomControl)
  private
    FGraph : TPHXGraphCf;
    FValues: TPHXColorList4f;
    FBorder: TRecti;
    FSelected: Integer;

    procedure SetGraph(const Value: TPHXGraphCf);

    procedure PaintAxes;
    procedure PaintGrid;
    procedure PaintGraph;
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
    function ScreenToGraph(const Screen: TVector2f): Single; overload;
    function ScreenToGraph(const X,Y: Single): Single; overload;
    // Convert a graph key to screen coordinates
    function GraphToScreen(const Key: TPHXGraphKeyCf): Single; overload;
    function GraphToScreen(const Time: Single): Single; overload;

    // Get the key at a given location
    function KeyAt(const Screen: Single): Integer; overload;

    property Graph: TPHXGraphCf read FGraph write SetGraph;
    // Selected key index
    property Selected: Integer read FSelected write SetSelected;

    property Border: TRecti read FBorder write FBorder;
  end;

//------------------------------------------------------------------------------
TFrmGraphRGB = class(TFrame)
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
    edValue: TColorBox;
    edTime: TJvSpinEdit;
    procedure GraphPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GraphPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GraphPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure edValueChange(Sender: TObject);

    procedure btnPointerClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnInterpolateCubicClick(Sender: TObject);
    procedure btnInterpolateLinearClick(Sender: TObject);
    procedure btnInterpolateStepClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  private
    FGraph: TPHXGraphCf;
    FSelected: Integer;
    FPanel: TPnlGraphRGB;
    FOnChanged: TNotifyEvent;

    procedure Changed;


    procedure SetGraph(const Value: TPHXGraphCf);
    procedure SetSelected(const Value: Integer);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    property Graph  : TPHXGraphCf read FGraph   write SetGraph;

    property Panel: TPnlGraphRGB read FPanel write FPanel;

    property Selected: Integer read FSelected write SetSelected;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}


const DEFAULT_BORDER_LEFT    = 30;//8;
const DEFAULT_BORDER_TOP     = 4;
const DEFAULT_BORDER_RIGHT   = 4;
const DEFAULT_BORDER_BOTTOM  = 17;


{ TFrmGraphRGB }

constructor TFrmGraphRGB.Create(AOwner: TComponent);
begin
  inherited;
  Panel:= TPnlGraphRGB.Create(Self);
  Panel.Parent:= Self;
  Panel.Align:= alClient;
  Panel.OnMouseDown:= GraphPanelMouseDown;
  Panel.OnMouseMove:= GraphPanelMouseMove;
  Panel.OnMouseUp  := GraphPanelMouseUp;

  SetGraph(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.Changed;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);

  FGraph.Expand;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.GraphPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var Key: TPHXGraphKeyCf;
begin
  if Graph = nil then Exit;

 // Key:= Panel.ScreenToGraph( X, Y );

  if btnMove.Down and (ssLeft in Shift) then
  begin
    SetSelected( Panel.KeyAt( X ) );
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmGraphRGB.GraphPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var GraphKey: TPHXGraphKeyCf;
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
  if btnMove.Down and (Panel.KeyAt( X) >= 0) then
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
      GraphKey:= Graph.Keys[Selected];
      GraphKey.Time:= Panel.ScreenToGraph( X, Y);
      Graph.Keys[Selected]:= GraphKey;

      SetSelected(Selected);

      Changed;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.GraphPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Time  : Single;
var R,G,B: Single;
begin
  if Graph = nil then Exit;

  Time:= Panel.ScreenToGraph( X, Y);

  // Trunc the time
  if Time < 0.0 then Time:= 0.0;
  if Time > 1.0 then Time:= 1.0;


  // Select keys
  if btnPointer.Down then
  begin
    SetSelected( Panel.KeyAt( X) );
  end else
  // Add keys
  if btnAdd.Down then
  begin
    R:= GetRValue(edValue.Selected) / 255;
    G:= GetGValue(edValue.Selected) / 255;
    B:= GetBValue(edValue.Selected) / 255;

    Graph.Keys.Add(Time, R,G,B, 1.0);

    Graph.Keys.SortByTime;

    SetSelected( Panel.KeyAt( X));

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.KeyUp(var Key: Word; Shift: TShiftState);
var GraphKey: TPHXGraphKeyCf;
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

    // Trunc the time
    if GraphKey.Time < 0.0 then GraphKey.Time:= 0.0;
    if GraphKey.Time > 1.0 then GraphKey.Time:= 1.0;

    Graph.Keys[Selected]:= GraphKey;

    Panel.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.SetGraph(const Value: TPHXGraphCf);
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

    btnInterpolateCubic .Enabled:= True;
    btnInterpolateLinear.Enabled:= True;
    btnInterpolateStep  .Enabled:= True;

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

    edTime .Enabled:= False;
    edValue.Enabled:= False;

    edTime .Color:= clBtnFace;
    edValue.Color:= clBtnFace;

    btnInterpolateCubic .Enabled:= False;
    btnInterpolateLinear.Enabled:= False;
    btnInterpolateStep  .Enabled:= False;
  end;
  Panel.Graph:= Graph;
end;


//------------------------------------------------------------------------------
procedure TFrmGraphRGB.SetSelected(const Value: Integer);
var Key: TPHXGraphKeyCf;
var R,G,B: Byte;
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
    R:= Round(Key.Value.Red   * 255);
    G:= Round(Key.Value.Green * 255);
    B:= Round(Key.Value.Blue  * 255);

    edTime .Value   := Key.Time  * 100;
    edValue.Selected:= RGB(R, G, B);

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
procedure TFrmGraphRGB.edValueChange(Sender: TObject);
var Key: TPHXGraphKeyCf;
var R,G,B: Single;
begin
  if Assigned(Graph) and (Selected >=0) and (Selected < Graph.Keys.Count) then
  begin
    R:= GetRValue(edValue.Selected) / 255;
    G:= GetGValue(edValue.Selected) / 255;
    B:= GetBValue(edValue.Selected) / 255;

    Key.Time := edTime .Value / 100;
    Key.Value:= Color4f(R,G,B, 1.0);

    Graph.Keys[Selected]:= Key;

    Changed;

    Panel.Invalidate;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnPointerClick(Sender: TObject);
begin
  btnPointer.Down:= not btnPointer.Down;
  btnAdd    .Down:= False;
  btnMove   .Down:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnAddClick(Sender: TObject);
begin
  btnAdd    .Down:= not btnAdd.Down;
  btnPointer.Down:= False;
  btnMove   .Down:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnMoveClick(Sender: TObject);
begin
  btnMove   .Down:= not btnMove.Down;
  btnPointer.Down:= False;
  btnAdd    .Down:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnDeleteClick(Sender: TObject);
begin
  if Assigned(Graph) and (Selected >=0) and (Selected < Graph.Keys.Count) then
  begin
    Graph.Keys.Delete(Selected);

    SetSelected(-1);

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnInterpolateCubicClick(Sender: TObject);
begin
  Graph.Interpolation:= ipCubic;

  SetGraph(Graph);

  Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnInterpolateLinearClick(Sender: TObject);
begin
  Graph.Interpolation:= ipLinear;

  SetGraph(Graph);

  Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmGraphRGB.btnInterpolateStepClick(Sender: TObject);
begin
  Graph.Interpolation:= ipNone;

  SetGraph(Graph);

  Changed;
end;

{$REGION 'TPnlGraphRGB'}


//------------------------------------------------------------------------------
constructor TPnlGraphRGB.Create(AOwner: TComponent);
begin
  inherited;
  FValues:= TPHXColorList4f.Create;

  FBorder.Left  := DEFAULT_BORDER_LEFT;
  FBorder.Top   := DEFAULT_BORDER_TOP;
  FBorder.Right := DEFAULT_BORDER_RIGHT;
  FBorder.Bottom:= DEFAULT_BORDER_BOTTOM;

  DoubleBuffered:= True;
end;

//------------------------------------------------------------------------------
destructor TPnlGraphRGB.Destroy;
begin
  FValues.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPnlGraphRGB.Click;
begin
  inherited;
  SetFocus;
end;

//------------------------------------------------------------------------------
procedure TPnlGraphRGB.Invalidate;
begin
  inherited;

  if Assigned(Graph) then
  begin
    FValues.Count:=  Width - Border.Left - Border.Right-4;

    Graph.Expand(FValues.Count, FValues.List);
  end else
  begin
    FValues.Count:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPnlGraphRGB.Paint;
begin
  with Canvas do
  begin
    Brush.Color:= clWhite;

    FillRect(ClipRect);
  end;

  PaintAxes;

  if Assigned(Graph) then
  begin
    PaintGraph;
  end;
  PaintGrid;
end;

//------------------------------------------------------------------------------
procedure TPnlGraphRGB.PaintAxes;
var Index   : Integer;
var Position: Single;
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
      Position:= GraphToScreen(Index / 10);
    end else
    begin
      Position:= Border.Left + Width * (Index / 10);
    end;

    Text:= IntToStr(Index * 10);

    X:= Round(Position);
    Y:= Height - Border.Bottom;

    Canvas.MoveTo(X, Y);
    Canvas.LineTo(X, Y + 3);

    X:= X + 1 - Canvas.TextHeight(Text) div 2;
    Y:= Y + 2;

    if X + Canvas.TextWidth(Text) > Width then X:= Width - Canvas.TextWidth(Text);

    Canvas.TextOut(X, Y, Text);
  end;


        {
  for Index := 0 to 2 do
  begin
    Value:= Index / 2;

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
       }
////  Position:= GraphToScreen(0, Graph.MinValue);
 // Position:= GraphToScreen(0, Graph.MaxValue);
end;

//------------------------------------------------------------------------------
procedure TPnlGraphRGB.PaintGrid;
var Index   : Integer;
var Position: Single;
begin
  Canvas.Pen.Color:= clSilver;
  Canvas.Pen.Style:= psSolid;
  for Index := 1 to 10 do
  begin
    if Assigned(Graph) then
    begin
      Position:= GraphToScreen(Index / 10);
    end else
    begin
      Position:= Width * (Index / 10);
    end;

    Canvas.MoveTo( Round(Position), Border.Top);
    Canvas.LineTo( Round(Position), Height - Border.Bottom);
  end;
  Canvas.MoveTo(Border.Left         ,  Border.Top);
  Canvas.LineTo(Width - Border.Right,  Border.Top);
end;


//------------------------------------------------------------------------------
procedure TPnlGraphRGB.PaintGraph;
var Index   : Integer;
var Key      : TPHXGraphKeyCf;
var Position : Single;
var R,G,B,A: Byte;
begin
  Canvas.Pen.Color:= clMaroon;
  Canvas.Pen.Style:= psSolid;

  Canvas.Brush.Color:= clWhite;
  Canvas.Brush.Style:= bsSolid;


  for Index := 0 to FValues.Count - 1 do
  begin
    R:= Round(FValues.List^[Index].Red   * 255);
    G:= Round(FValues.List^[Index].Green * 255);
    B:= Round(FValues.List^[Index].Blue  * 255);
    A:= Round(FValues.List^[Index].Alpha * 255);

    Canvas.Pen.Color:= RGB(R, G, B);

    Canvas.MoveTo(Index + FBorder.Left + 2, FBorder.Top+4);
    Canvas.LineTo(Index + FBorder.Left + 2, Height - FBorder.Bottom - 1-4)
  end;

  Canvas.Pen.Color:= clBlack;
  for Index := 0 to Graph.Keys.Count - 1 do
  begin
    Key:= Graph.Keys[Index];

    Position:= GraphToScreen(Key);

    if Index = Selected then
    begin
      Canvas.Brush.Color:= clGray;
      Canvas.Brush.Style:= bsSolid;
    end else
    begin
      Canvas.Brush.Color:= clWhite;
      Canvas.Brush.Style:= bsClear; // bsSolid
    end;

    Canvas.Rectangle( Round(Position - 3), Border.Top+1, Round(Position + 4), Height - Border.Bottom - 1);
  end;
end;



//------------------------------------------------------------------------------
function TPnlGraphRGB.KeyAt(const Screen: Single): Integer;
var Index   : Integer;
var Key      : TPHXGraphKeyCf;
var Position : Single;
begin
  for Index := 0 to Graph.Keys.Count - 1 do
  begin
    Key:= Graph.Keys[Index];

    Position:= GraphToScreen(Key);

    if (Screen > Position - 3) and
       (Screen < Position + 4) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;




//------------------------------------------------------------------------------
procedure TPnlGraphRGB.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  TFrmGraphRGB(Owner).KeyUp(Key, Shift);
end;

//------------------------------------------------------------------------------
function TPnlGraphRGB.GraphToScreen(const Key: TPHXGraphKeyCf): Single;
begin
  Result:= Border.Left + Key.Time  * (Width  - (Border.Right  + Border.Left));
end;

//------------------------------------------------------------------------------
function TPnlGraphRGB.GraphToScreen(const Time: Single): Single;
var Key: TPHXGraphKeyCf;
begin
  Key.Time := Time;
// Key.Value:= Value;

  Result:= GraphToScreen( Key );
end;

//------------------------------------------------------------------------------
function TPnlGraphRGB.ScreenToGraph(const Screen: TVector2f): Single;
begin
  Result := (Screen.X - Border.Left) / (Width  - (Border.Right  + Border.Left));
//  Result.Value:= 0;//ZoomMin + (ZoomMax - ZoomMin) * (Height - Screen.Y - Border.Bottom) / (Height  - (Border.Bottom  + Border.Top));
end;

//------------------------------------------------------------------------------
function TPnlGraphRGB.ScreenToGraph(const X, Y: Single): Single;
begin
  Result:= ScreenToGraph( Vector2f(X, Y ) );
end;

//------------------------------------------------------------------------------
procedure TPnlGraphRGB.SetGraph(const Value: TPHXGraphCf);
var w: Integer;
begin
  FGraph := Value;

//  FBorder.Left:= DEFAULT_BORDER_LEFT;

  if Assigned(FGraph) then
  begin
  //  w:= Canvas.TextWidth( Format('%.1f', [Graph.MaxValue]));

 //   if w + 8 > Border.Left then FBorder.Left:= w + 8;

    //FZoomMax:= Graph.MaxValue;
   // FZoomMin:= Graph.MinValue;
  //  ZoomFit;
  end;



  Resize;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPnlGraphRGB.SetSelected(const Value: Integer);
begin
  FSelected := Value;

  Invalidate;
end;

{$ENDREGION}


end.
