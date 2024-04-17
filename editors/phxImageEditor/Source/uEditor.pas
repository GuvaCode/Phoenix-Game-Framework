unit uEditor;

interface

uses Classes, Types, SysUtils, Graphics, Dialogs, Controls,

  phxTypes,
  phxMath,

  phxImage,
  phxImageEx,

  phxGraphics,
  phxGraphicsEx,

  phxClasses,
  phxShape,

  phxEditor;

type

TPHXImageEditor = class;

TDragArea =(
   daNone,
   daLeft,
   daRight,
   daTop,
   daBottom,
   daPivotX,
   daPivotY
   );

TPHXPatternEvent = procedure(Sender: TObject; PatternIndex: Integer) of object;

//------------------------------------------------------------------------------
TPHXImageEditor = class(TPHXEditor)
 private
    FImage      : TPHXImage;
    FBuffer     : TBitmap;
    FBackground : TBitmap;
    FMask       : TBitmap;

    FOnChange: TNotifyEvent;
    FOnTagChange: TNotifyEvent;
    //FOnPatternChange: TNotifyEvent;

    FOnSelectPattern: TPHXPatternEvent;
    procedure DoSelectPattern(PatternIndex: Integer);

    procedure SetImage(const Value: TPHXImage);
  protected
    procedure GetDocumentSize(out Width: Integer; out Height: Integer); override;
    // Paint the image
    procedure PaintDocument(const Offset: TVector2i; const Zoom: Single); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;   Y: Integer); override;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;


    function GetDragArea(const Pattern: TPHXPattern; X, Y, Tolerance: Integer): TDragArea;

    // Returns the pattern at a screen location
    function PatternAt(ScreenX, ScreenY: Integer): Integer;

    // Save the selected pattern as an image in a file
    procedure SavePattern(const FileName: String; PatternIndex: Integer);

    procedure MaskPattern(PatternIndex: Integer);
    // Draw pattern bounds
    procedure DrawPattern(PatternIndex: Integer; DrawPivot: Boolean = True);

    // The current image
    Property Image     : TPHXImage read FImage write SetImage;
    property Buffer: TBitmap read FBuffer;
    property Background: TBitmap read FBackground;
  published
    // Options for the image editor
//    property Options: TPHXSkinEditorOptions read FOptions write SetOptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    // Event thats called when the selected pattern is changed
   // Property OnElementChange: TNotifyEvent read FOnElementChange write FOnElementChange;
    // Event thats called when the selected pattern is changed
   // Property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;

   // Property OnPatternChange: TNotifyEvent read FOnPatternChange write FOnPatternChange;
    Property OnTagChange: TNotifyEvent read FOnTagChange write FOnTagChange;


    property OnSelectPattern: TPHXPatternEvent read FOnSelectPattern write FOnSelectPattern;
  end;


procedure Register;

implementation


procedure Register;
begin
//  RegisterComponents('Phoenix', [TPHXEditor]);

 // RegisterComponents('Phoenix', [TPHXImageEditor]);
  //RegisterComponents('Phoenix', [TPHXShapeEditor]);

end;



{$REGION 'TPHXImageEditor'}

// TPHXImageEditor
//==============================================================================
constructor TPHXImageEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered:= True;

  FBuffer    := TBitmap.Create;
  FMask      := TBitmap.Create;
  FBackground:= CreateTransparentImage(4);

//  FGridSize:= 16;

//  FBackground:= TBitmap.Create;

//  FPatternIndex:= NO_PATTERN;
end;

//------------------------------------------------------------------------------
destructor TPHXImageEditor.Destroy;
begin
  FBuffer.Free;
  FMask.Free;
  FBackground.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.DoSelectPattern(PatternIndex: Integer);
begin
  if Assigned(FOnSelectPattern) then
  begin
    FOnSelectPattern(Self, PatternIndex);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.GetDocumentSize(out Width, Height: Integer);
begin
  if Assigned(Image) then
  begin
    Width := Image.Width;
    Height:= Image.Height;
  end else
  begin
    Width := 0;
    Height:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.PaintDocument(const Offset: TVector2i; const Zoom: Single);
var Rect: TRect;
begin
  if Assigned(Image)and (Image.Width > 0) and (Image.Height > 0) then
  begin
    Rect.Left  := Offset.X;
    Rect.Top   := Offset.Y;
    Rect.Right := Offset.X + Trunc(Image.Width  * Viewport.Zoom);
    Rect.Bottom:= Offset.Y + Trunc(Image.Height * Viewport.Zoom);

    Canvas.StretchDraw(Rect, FBuffer);

    // if ieDrawGrid in Options then
    Grid.Draw(Canvas, Rect); //, FGridSize
  end else
  begin
    with Canvas do
    begin
      Brush.Color:= clBtnFace;
      Brush.Style:= bsSolid;
      Pen.Color  := clBtnFace;
      Pen.Style  := psSolid;

      FillRect(ClipRect);

      //W:= TextWidth ('Image is empty');
      //H:= TextHeight('Image is empty');

      //TextOut((Width - W) div 2, (Height - H) div 2, 'Image is empty' );
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.SetImage(const Value: TPHXImage);
begin
  if FImage <> Value then
  begin
    FImage := Value;

    Viewport.ZoomFit;
  end;

  if Assigned(FImage) then
  begin
    FImage.Draw(FBuffer, FBackground);
    FImage.Draw(FMask  , FBackground, 0.5);
  end
  else
  begin
    FBuffer.Width := 0;
    FBuffer.Height:= 0;

    FMask.Width := 0;
    FMask.Height:= 0;
  end;
  DocumentChanged;

  Invalidate;

  if Assigned(OnChange) then OnChange(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Pattern: Integer;
begin
  inherited;

  if Assigned(Image) and not Assigned(Tools.Active) then
  begin
    Pattern:= PatternAt(X, Y);

    if Pattern <> PHXPATTERN_NONE then
    begin
      DoSelectPattern(Pattern);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXImageEditor.GetDragArea(const Pattern: TPHXPattern; X, Y, Tolerance: Integer): TDragArea;
var L,T,R,B,PX,PY: Extended;
var MinX, MinY: Integer;
var MaxX, MaxY: Integer;
begin
  MinX:= X - Tolerance;
  MinY:= Y - Tolerance;
  MaxX:= X + Tolerance;
  MaxY:= Y + Tolerance;

  L:= Pattern.X;
  T:= Pattern.Y;
  R:= Pattern.X  + Pattern.Width;
  B:= Pattern.Y  + Pattern.Height;

  PX:= Pattern.X  + Pattern.Pivot.X;
  PY:= Pattern.Y  + Pattern.Pivot.Y;


  Result:= daNone;
  if (MinX < PX) and (MaxX > PX) then Result:=daPivotX;
  if (MinY < PY) and (MaxY > PY) then Result:=daPivotY;

  if (MinX < L) and (MaxX > L) then Result:=daLeft;
  if (MinX < R) and (MaxX > R) then Result:=daRight;

  if (MinY < T) and (MaxY > T) then Result:=daTop;
  if (MinY < B) and (MaxY > B) then Result:=daBottom;
end;


//------------------------------------------------------------------------------
function TPHXImageEditor.PatternAt(ScreenX, ScreenY: Integer): Integer;
var Index  : Integer;
var Pattern: TPHXPattern;
var Rect   : TRecti;
var Position: TVector2i;
begin
  Position:= ScreenToDocument( ScreenX, ScreenY );

  for Index:=0 to Image.Patterns.Count-1 do
  begin
    Pattern:=Image.Patterns[Index];

    Rect.Left  := Pattern.X;
    Rect.Top   := Pattern.Y;
    Rect.Right := Pattern.X + Pattern.Width;
    Rect.Bottom:= Pattern.Y + Pattern.Height;

    if PointInRect(Position, Rect) then
    begin
      Result:= Index;
      Exit;
    end;

  end;
  Result:= PHXPATTERN_NONE;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.SavePattern(const FileName: String; PatternIndex: Integer);
var Temp: TBitmap;
var Pattern: TPHXPattern;

var SrcRect: TRect;
var DstRect: TRect;
begin
  Pattern:= Image.Patterns.List^[PatternIndex];

  DstRect.Left   :=0;
  DstRect.Top   := 0;
  DstRect.Right := Pattern.Width;
  DstRect.Bottom:= Pattern.Height;

  SrcRect.Left  :=Pattern.X;
  SrcRect.Top   := Pattern.Y;
  SrcRect.Right := Pattern.X + Pattern.Width;
  SrcRect.Bottom:= Pattern.Y + Pattern.Height;

  Temp:= TBitmap.Create;
  try
    Temp.PixelFormat:= pf24Bit;
    Temp.Width      := Pattern.Width;
    Temp.Height     := Pattern.Height;

    Temp.Canvas.CopyRect(DstRect, FBuffer.Canvas, SrcRect);

    Temp.SaveToFile(FileName);
  finally
    Temp.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.DrawPattern(PatternIndex: Integer; DrawPivot: Boolean = True);
var Offset: TVector2i;
var Pattern: TPHXPattern ;
var rPattern: TRect;
var rPivot  : TPoint;
begin
  if (Image = nil) or (PatternIndex <  0) or (PatternIndex >= Image.Patterns.Count) then Exit;

  Offset.X:= Viewport.Offset.X;
  Offset.Y:= Viewport.Offset.Y;

  Pattern:=Image.Patterns[PatternIndex];

  rPattern.Left  := Round(Offset.X + (Pattern.X                 ) * Viewport.Zoom);
  rPattern.Top   := Round(Offset.Y + (Pattern.Y                 ) * Viewport.Zoom);
  rPattern.Right := Round(Offset.X + (Pattern.X + Pattern.Width ) * Viewport.Zoom);
  rPattern.Bottom:= Round(Offset.Y + (Pattern.Y + Pattern.Height) * Viewport.Zoom);

  rPivot.X:= Round(Offset.X + (Pattern.X + Pattern.Pivot.X ) * Viewport.Zoom);
  rPivot.Y:= Round(Offset.Y + (Pattern.Y + Pattern.Pivot.Y ) * Viewport.Zoom);

  with Canvas do
  begin
     Brush.Style:= bsClear;

//     Pen.Mode := pmXor;

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(rPattern);

     Pen.Color  := clRed;//clBlack;
     Pen.Width  := 1;
     Pen.Style  := psSolid;//psDot;
     Rectangle(rPattern);


     Pen.Color  := clRed;
     Pen.Style  := psDash;// psDot;

     if DrawPivot and (Pattern.Pivot.X  > 0) then
     begin
       MoveTo(rPivot.X        , rPattern.Top);
       LineTo(rPivot.X        , rPattern.Bottom);
     end;

     if DrawPivot and (Pattern.Pivot.Y  > 0) then
     begin
       MoveTo(rPattern.Left   , rPivot.Y);
       LineTo(rPattern.Right  , rPivot.Y);
     end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEditor.MaskPattern(PatternIndex: Integer);
var Pattern: TPHXPattern ;
var PatternRect: TRect;
var SourceRect: TRect;
begin
  if (Image = nil) or (PatternIndex <  0) or (PatternIndex >= Image.Patterns.Count) then Exit;

  Pattern:= Image.Patterns[PatternIndex];

  SourceRect.Left  := Pattern.X;
  SourceRect.Top   := Pattern.Y;
  SourceRect.Right := Pattern.X + Pattern.Width ;
  SourceRect.Bottom:= Pattern.Y + Pattern.Height ;


  PatternRect.Left  := Round(Viewport.Offset.X + (Pattern.X                 ) * Viewport.Zoom);
  PatternRect.Top   := Round(Viewport.Offset.Y + (Pattern.Y                 ) * Viewport.Zoom);
  PatternRect.Right := Round(Viewport.Offset.X + (Pattern.X + Pattern.Width ) * Viewport.Zoom);
  PatternRect.Bottom:= Round(Viewport.Offset.Y + (Pattern.Y + Pattern.Height) * Viewport.Zoom);

  Canvas.CopyRect(PatternRect, FMask.Canvas, SourceRect);

end;




{$ENDREGION}

end.
