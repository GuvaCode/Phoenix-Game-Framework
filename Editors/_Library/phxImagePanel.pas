unit phxImagePanel;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Graphics,

  Generics.Collections,

  phxTypes,
  phxMath,
  phxImage,
  phxImageEx,

  phxGraphicsEx;

type

//------------------------------------------------------------------------------
TPHXImageViewType = (
  vtImage,
  vtPatterns
  );

//------------------------------------------------------------------------------
TPHXImagePanel = class(TGraphicControl)
  private
    FImage   : TPHXImage;
    FBuffer  : TBitmap;
    FGridSize: TSizei;

    FScrollBar: TScrollBar;

    FPatternRects: TList<TRecti>;
    FPatternIndex: Integer;

    FViewtype: TPHXImageViewType;
    FBackground: TColor;

    procedure CalculateGridSize;

    procedure PaintDesign;
    procedure PaintPatterns;

    procedure SetImage       (const Value: TPHXImage);
    procedure SetViewtype    (const Value: TPHXImageViewType);
    procedure SetScrollBar   (const Value: TScrollBar);
    procedure SetPatternIndex(const Value: Integer);
    function GetColumnCount: Integer;
    function GetRowCount: Integer;
    procedure SetBackground(const Value: TColor);
    function GetPatternName: String;
    function GetHasSelection: Boolean;
    procedure UpdateScrollbars;
    procedure ScrollBarChange(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;

    Property PatternRects: TList<TRecti> read FPatternRects;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Select a pattern by name
    procedure SelectPattern(const Name: String);

    Procedure DrawPattern(const Dest: TCanvas; const Rect: TRecti; const PatternIndex: Integer);



    property PatternIndex: Integer read FPatternIndex write SetPatternIndex;
    property PatternName : String  read GetPatternName;

    property HasSelection: Boolean read GetHasSelection;

    property ColumnWidth: Integer read FGridSize.Width write FGridSize.Width;
    property ColumnCount: Integer read GetColumnCount;
    property RowHeight: Integer read FGridSize.Height write FGridSize.Height;
    property RowCount: Integer read GetRowCount;
  published
    property OnDblClick;
    property OnClick;

    // The image
    Property Image: TPHXImage read FImage write SetImage;
    // Vertical scrollbar
    Property ScrollBar: TScrollBar read FScrollBar write SetScrollBar;

    property Viewtype: TPHXImageViewType read FViewtype write SetViewtype;

    property Background: TColor read FBackground write SetBackground;

    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property Align;
  end;

implementation


{ TPHXImagePanel }

//------------------------------------------------------------------------------
constructor TPHXImagePanel.Create(AOwner: TComponent);
begin
  inherited;
  FBackground:= clBtnFace;
  FPatternRects:= TList<TRecti>.Create;
  FPatternIndex:= -1;

  FBuffer:= TBitmap.Create;

end;

//------------------------------------------------------------------------------
destructor TPHXImagePanel.Destroy;
begin
  FBuffer.Free;

  FPatternRects.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXImagePanel.Paint;
begin
  If (csDesigning in ComponentState) then
  begin
    PaintDesign
  end else
  begin
    Canvas.Font.Style:= [fsBold];
    Canvas.Brush.Style:= bsClear;

    if Assigned(Image) then
    begin
      PaintPatterns;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.PaintDesign;
var s: String;
var w, H: Integer;
begin
  Canvas.Brush.Style := Graphics.bsSolid;
  Canvas.Brush.Color := inherited Color;
  Canvas.Pen.Color   := clBlack;
  Canvas.Pen.Style   := Graphics.psDash;
  Canvas.Rectangle(0, 0, Width, Height);

  Canvas.Pen.Style := Graphics.psSolid;
  Canvas.Pen.Color := clGray;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width, Height);

  Canvas.MoveTo(0, Height);
  Canvas.LineTo(Width, 0);

  s := Format('(%s)', [ClassName]);

  w := Canvas.TextWidth(s);
  h := Canvas.TextHeight(s);

  Canvas.Brush.Style := Graphics.bsSolid;
  Canvas.Brush.Color := clBtnFace;
  Canvas.TextOut(Width div 2-w div 2, Height div 2-h div 2, s);
end;



const PATTERN_VSPACE = 4;
const PATTERN_HSPACE = 4;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.PaintPatterns;
var Index      : Integer;
var X,X2       : Integer;
var Y,Y2       : Integer;
var Pattern    : TPHXPattern;
var PatternRect: TRecti;
var Name       : String;
begin
  FPatternRects.Clear;

  Canvas.Font.Style:= [fsBold];
  Canvas.Brush.Style:= bsClear;

  X:= PATTERN_HSPACE div 2;
  Y:= PATTERN_VSPACE div 2;

  if Assigned(ScrollBar) then
  begin
     Y:= Y - ScrollBar.Position;
  end;

  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    Pattern:= Image.Patterns.List^[Index];

    // Move to the next line
    if (X + FGridSize.Width > Width) then
    begin
      X:= PATTERN_HSPACE div 2;

      Y:= Y + FGridSize.Height + PATTERN_VSPACE;
    end;

    PatternRect.Left  := X;
    PatternRect.Top   := Y;
    PatternRect.Bottom:= Y + FGridSize.Height;
    PatternRect.Right := X + FGridSize.Width;

    Name:= String(Pattern.Name);

    // Draw the pattern text
    X2:= X + (FGridSize.Width - Canvas.TextWidth(Name)) div 2;

    Canvas.TextOut(X2, Y, Name);

    // Draw the pattern image
    X2:= X + (FGridSize.Width  - Pattern.Width                           ) div 2;
    Y2:= Y + (FGridSize.Height - Pattern.Height - Canvas.TextHeight(Name)) div 2 + Canvas.TextHeight(Name);

    Image.DrawPattern(Canvas, FBuffer, X2, Y2, Index);

    FPatternRects.Add(PatternRect);

    if Index = FPatternIndex then
    begin
      Canvas.Pen.Color:= clRed;

      Canvas.Rectangle(PatternRect.Left - 1, PatternRect.Top - 1, PatternRect.Right + 1, PatternRect.Bottom + 1);
    end;

    X:=X + FGridSize.Width + PATTERN_HSPACE;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.Resize;
begin
  inherited;
  UpdateScrollbars;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.DrawPattern(const Dest: TCanvas; const Rect: TRecti; const PatternIndex: Integer);
var Pattern: TPHXPattern;
var X,Y: Integer;
begin
  if Assigned(Image) and ( PatternIndex >= 0) and (PatternIndex < Image.Patterns.Count) then
  begin
    Pattern:= Image.Patterns.List^[PatternIndex];

    X:= Rect.Left + ((Rect.Right  - Rect.Left) - Pattern.Width ) div 2;
    Y:= Rect.Top  + ((Rect.Bottom - Rect.Top ) - Pattern.Height) div 2;

    Image.DrawPattern(Dest, FBuffer, X,Y, PatternIndex);
  end;
end;


//------------------------------------------------------------------------------
function TPHXImagePanel.GetColumnCount: Integer;
begin
  Result:= (Width - PATTERN_HSPACE) div (ColumnWidth + PATTERN_HSPACE);
end;

//------------------------------------------------------------------------------
function TPHXImagePanel.GetRowCount: Integer;
begin
  if Assigned(Image) then
  begin
    Result:= (Image.Patterns.Count div GetColumnCount);
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
function TPHXImagePanel.GetHasSelection: Boolean;
begin
  Result:= Assigned(Image) and (PatternIndex >= 0) and (PatternIndex < Image.Patterns.Count);
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.CalculateGridSize;
var Index  : Integer;
var Pattern: TPHXPattern;
var W,H    : Integer;
begin
  FGridSize.Width := 0;
  FGridSize.Height:= 0;

  Canvas.Font.Style := [fsBold];
  Canvas.Brush.Style:= bsClear;
  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    Pattern:= Image.Patterns.List^[Index];

    // Calculate the width of the column
    if Canvas.TextWidth( String(Pattern.Name) ) > Pattern.Width then
    begin
      W:= Canvas.TextWidth( String(Pattern.Name) );
    end else
    begin
      W:= Pattern.Width;
    end;

    H:= Pattern.Height + Canvas.TextHeight( String(Pattern.Name) );

    if W > FGridSize.Width  then FGridSize.Width := W;
    if H > FGridSize.Height then FGridSize.Height:= H;
  end;

  UpdateScrollbars;
end;


//------------------------------------------------------------------------------
procedure TPHXImagePanel.SelectPattern(const Name: String);
begin
  if Assigned(Image) then
  begin
    SetPatternIndex( Image.Patterns.IndexOf(Name));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.UpdateScrollbars;
var Rows: Integer;
var ScrollMax: Integer;
begin
  if Assigned(ScrollBar) then
  begin
    Rows:= GetRowCount;

    ScrollMax:= Rows * FGridSize.Height;

    if (Height < ScrollMax) then
    begin
      ScrollBar.Enabled:= True;
      ScrollBar.Max    := ScrollMax - Height;
    end else
    begin
      ScrollBar.Enabled:= False;
    end;

  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImagePanel.SetBackground(const Value: TColor);
begin
  FBackground := Value;

  if Assigned(FImage) then
  begin
    Image.Draw(FBuffer,  Graphics.ColorToRGB(Background) );
  end;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  if Assigned(FImage) then
  begin
    Image.Draw(FBuffer,  Graphics.ColorToRGB(Background) );

    CalculateGridSize;
  end;


  Invalidate;
end;

//------------------------------------------------------------------------------
function TPHXImagePanel.GetPatternName: String;
begin
  if Assigned(FImage) and (PatternIndex >= 0) and (PatternIndex < Image.Patterns.Count) then
  begin
    Result:= String( Image.Patterns[PatternIndex].Name );
  end else
  begin
    Result:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.SetPatternIndex(const Value: Integer);
begin
  FPatternIndex := Value;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.SetScrollBar(const Value: TScrollBar);
begin
  FScrollBar := Value;

  if Assigned(FScrollBar) then
  begin
    UpdateScrollbars;

    FScrollBar.OnChange:= ScrollBarChange;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.ScrollBarChange(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXImagePanel.SetViewtype(const Value: TPHXImageViewType);
begin
  FViewtype := Value;

  Invalidate;
end;


//------------------------------------------------------------------------------
procedure TPHXImagePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Index: Integer;
begin
  inherited;

  if Button = TMouseButton.mbLeft then
  begin
    for Index := 0 to PatternRects.Count - 1 do
    begin
      if PointInRect(Vector2i(X,Y), PatternRects[Index]) then
      begin
        SetPatternIndex(Index);

        Exit;
      end;

    end;
    SetPatternIndex(-1);
  end;
end;


end.
