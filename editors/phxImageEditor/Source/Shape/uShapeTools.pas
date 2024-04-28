unit uShapeTools;

interface

uses Classes, Types, SysUtils, Graphics, Dialogs, Math,



  phxEditor,
  phxTypes,
  phxTexture,
  phxImage,
  phxImageEx,
  phxGraphics,
  phxGraphicsEx,

  phxShape;

type

{$REGION 'TPHXShapeEditor'}

//------------------------------------------------------------------------------
TPHXShapeEditor = class(TPHXEditor)
  private
    FImage     : TPHXImage;
    FPattern   : Integer;
    APattern   : TPHXPattern;

    FBackground: TBitmap;
    FBuffer    : TBitmap;
    FMask      : TBitmap;

    FRelativeToPattern: Boolean;

    FShape: TPHXShape;

    procedure DrawShape(Shape: TPHXPoint  ; Selected: Boolean); overload;
    procedure DrawShape(Shape: TPHXBox    ; Selected: Boolean); overload;
    procedure DrawShape(Shape: TPHXPolygon; Selected: Boolean); overload;
    procedure DrawShape(Shape: TPHXLine   ; Selected: Boolean); overload;
    procedure DrawShape(Shape: TPHXCircle ; Selected: Boolean); overload;

    procedure SetShape(const Value: TPHXShape);
    procedure SetImage(const Value: TPHXImage);
    procedure SetPattern(const Value: Integer);
  protected
    procedure GetDocumentSize(out Width: Integer; out Height: Integer); override;
    // Paint the image
    procedure PaintDocument(const Offset: TVector2i; const Zoom: Single); override;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;

    function GetPattern: TPHXPattern;

    function PatternToScreen(const Point: TVector2f): TVector2f; overload;
    function PatternToScreen(const X,Y: Single): TVector2f; overload;
    function ScreenToPattern(const Point: TVector2f): TVector2f; overload;
    function ScreenToPattern(const X,Y: Single): TVector2f; overload;

    // The current image
    Property Image : TPHXImage read FImage write SetImage;
    // The current shape
    property Shape: TPHXShape read FShape write SetShape;
    // The pattern if RelativeToPattern is true
    property Pattern: Integer read FPattern write SetPattern;
    // If the selected shape is relative to the pattern or not
    property RelativeToPattern: Boolean read FRelativeToPattern;


    property Buffer: TBitmap read FBuffer;
    property Background: TBitmap read FBackground;
  published

  end;

{$ENDREGION}


  TPointArray = array of TPoint;
  TRealArray = array of Real;

procedure ExtractPoints(Texture: TPHXTexture; Rect: TRect; out APoints: TPointArray);

function FindConvexHull(var APoints: TPointArray): Boolean;

procedure UpdateShape(Editor: TPHXShapeEditor; Shape: TPHXBox; const Points: TPointArray); overload;
procedure UpdateShape(Editor: TPHXShapeEditor; Shape: TPHXPolygon; const Points: TPointArray); overload;
procedure UpdateShape(Editor: TPHXShapeEditor; Shape: TPHXCircle; const Points: TPointArray); overload;

implementation

//------------------------------------------------------------------------------
procedure UpdateShape(Editor: TPHXShapeEditor; Shape: TPHXBox; const Points: TPointArray);
var Min     : TVector2f;
var Max     : TVector2f;
var Index   : Integer;
var Position: TVector2f;
begin
  if Length(Points) = 0 then Exit;

  if Editor.RelativeToPattern then
  begin
    Min.X:= Points[0].X - Editor.GetPattern.X - Editor.GetPattern.Pivot.X;
    Min.Y:= Points[0].Y - Editor.GetPattern.Y - Editor.GetPattern.Pivot.Y;
  end else
  begin
    Min.X:= Points[0].X;
    Min.Y:= Points[0].Y;
  end;
  Max:= Min;

  for Index := 1 to Length(Points) - 1 do
  begin
    if Editor.RelativeToPattern then
    begin
      Position.X:= Points[Index].X - Editor.GetPattern.X - Editor.GetPattern.Pivot.X;
      Position.Y:= Points[Index].Y - Editor.GetPattern.Y - Editor.GetPattern.Pivot.Y;
    end else
    begin
      Position.X:= Points[Index].X;
      Position.Y:= Points[Index].Y;
    end;

    if Position.X < Min.X then Min.X:= Position.X;
    if Position.Y < Min.Y then Min.Y:= Position.Y;
    if Position.X > Max.X then Max.X:= Position.X;
    if Position.Y > Max.Y then Max.Y:= Position.Y;
  end;

  Shape.Center:= Vector2f( (Max.X + Min.X) / 2, (Max.Y + Min.Y) / 2);

  Shape.Width := (Max.X - Min.X);
  Shape.Height:= (Max.Y - Min.Y);
end;

//------------------------------------------------------------------------------
procedure UpdateShape(Editor: TPHXShapeEditor; Shape: TPHXPolygon; const Points: TPointArray);
var Index  : Integer;
var Position: TVector2f;
begin
  Shape.Size:= Length(Points);

  for Index := 0 to Length(Points) - 1 do
  begin
    if Editor.RelativeToPattern then
    begin
      Position.X:= Points[Index].X - Editor.GetPattern.X - Editor.GetPattern.Pivot.X;
      Position.Y:= Points[Index].Y - Editor.GetPattern.Y - Editor.GetPattern.Pivot.Y;
    end else
    begin
      Position.X:= Points[Index].X;
      Position.Y:= Points[Index].Y;
    end;

    Shape.List^[Index]:= Position;
  end;
end;

//------------------------------------------------------------------------------
procedure UpdateShape(Editor: TPHXShapeEditor; Shape: TPHXCircle; const Points: TPointArray);
var Index  : Integer;
var Position: TVector2f;
var Min     : TVector2f;
var Max     : TVector2f;
var Radius  : Single;
var Distance: Single;
begin
  if Length(Points) = 0 then Exit;

  if Editor.RelativeToPattern then
  begin
    Min.X:= Points[0].X - Editor.GetPattern.X - Editor.GetPattern.Pivot.X;
    Min.Y:= Points[0].Y - Editor.GetPattern.Y - Editor.GetPattern.Pivot.Y;
  end else
  begin
    Min.X:= Points[0].X;
    Min.Y:= Points[0].Y;
  end;
  Max:= Min;

  for Index := 1 to Length(Points) - 1 do
  begin
    if Editor.RelativeToPattern then
    begin
      Position.X:= Points[Index].X - Editor.GetPattern.X - Editor.GetPattern.Pivot.X;
      Position.Y:= Points[Index].Y - Editor.GetPattern.Y - Editor.GetPattern.Pivot.Y;
    end else
    begin
      Position.X:= Points[Index].X;
      Position.Y:= Points[Index].Y;
    end;

    if Position.X < Min.X then Min.X:= Position.X;
    if Position.Y < Min.Y then Min.Y:= Position.Y;
    if Position.X > Max.X then Max.X:= Position.X;
    if Position.Y > Max.Y then Max.Y:= Position.Y;
  end;
  Shape.Center:= Vector2f( (Max.X + Min.X) / 2, (Max.Y + Min.Y) / 2);

  Radius:= 0;
  for Index := 0 to Length(Points) - 1 do
  begin
    if Editor.RelativeToPattern then
    begin
      Position.X:= Points[Index].X - Editor.GetPattern.X - Editor.GetPattern.Pivot.X;
      Position.Y:= Points[Index].Y - Editor.GetPattern.Y - Editor.GetPattern.Pivot.Y;
    end else
    begin
      Position.X:= Points[Index].X;
      Position.Y:= Points[Index].Y;
    end;

    Distance:=Sqrt( Sqr(Shape.Center.X - Position.X) + Sqr(Shape.Center.Y - Position.Y) );

    if Distance > Radius then Radius:= Distance;

  end;

  Shape.Radius:= Radius;
end;



{$REGION 'FindConvexHull'}

procedure QuickSort(var Points: TPointArray; Angles: array of Real; L, R: Integer);
var
  I, J: Integer;
  P, T: Real;
  TempPoint: TPoint;
begin
  repeat
    I := L;
    J := R;
    P := Angles[(L + R) shr 1];
    repeat
      while (Angles[I] < P)  do
        Inc(I);
      while (Angles[J] > P) do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := Angles[I];
          Angles[I] := Angles[J];
          Angles[J] := T;

          // swap angles
          TempPoint := Points[I];
          Points[I]:= Points[J];
          Points[J]:= TempPoint;

        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Points, Angles, L, J);
    L := I;
  until I >= R;
end;
(*

procedure TBubbleSort.Sort(var A: array of Integer);
var
  I, J, T: Integer;
begin
  for I := High(A) downto Low(A) do
    for J := Low(A) to High(A) - 1 do
      if A[J] > A[J + 1] then
      begin
        VisualSwap(A[J], A[J + 1], J, J + 1);
        T := A[J];
        A[J] := A[J + 1];
        A[J + 1] := T;
        if Terminated then Exit;
      end;
end;
*)

procedure BubbleSort(var Points: TPointArray; Angles: TRealArray);
var
  i, j: Integer;

  TempPoint: TPoint;
  TempAngle: Real;
begin
  // bubble sort
  for I := High(Angles) downto Low(Angles) do
  begin
    for J := Low(Angles) to High(Angles) - 1 do
    begin

      if ( Angles[j] > Angles[j+1] ) then
      begin
        TempAngle   := Angles[j];
        Angles[j]   := Angles[j+1];
        Angles[j+1] := TempAngle;

        TempPoint   := Points[j];
        Points[j]   := Points[j+1];
        Points[j+1] := TempPoint;
      end;
    end;
  end;
end;



     {
// sort an array of points by angle
//------------------------------------------------------------------------------
procedure QuickSortAngle(var A: TPointArray; Angles: array of Real; iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid: Real;
  TempPoint: TPoint;
  TempAngle: Real;
begin
  Lo  := iLo;
  Hi  := iHi;
  Mid := Angles[(Lo + Hi) div 2];
  repeat
    while Angles[Lo] < Mid do Inc(Lo);
    while Angles[Hi] > Mid do Dec(Hi);
    if Lo <= Hi then
    begin
      // swap points
      TempPoint := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := TempPoint;
      // swap angles
      TempAngle := Angles[Lo];
      Angles[Lo] := Angles[Hi];
      Angles[Hi] := TempAngle;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  // perform quicksorts on subsections
  if Hi > iLo then QuickSortAngle(A, Angles, iLo, Hi);
  if Lo < iHi then QuickSortAngle(A, Angles, Lo, iHi);
end;
      }


type

  TPointFloat = record
    X: Real;
    Y: Real;
  end;

// return the boundary points of the convex hull of a set of points using Grahams scan
// over-writes the input array - so make a copy first
//
// Author: Peter Bone
// Homepage: http://www.geocities.com/peter_bone_uk
//------------------------------------------------------------------------------
function FindConvexHull(var APoints: TPointArray): Boolean;
var
  LAngles: TRealArray;
  Lindex, LMinY, LMaxX, LPivotIndex: Integer;
  LPivot: TPoint;
  LBehind, LInfront: TPoint;
  LRightTurn: Boolean;
  LVecPoint: TPointFloat;
begin
  Result := True;

  if Length(APoints) = 3 then Exit; // already a convex hull
  if Length(APoints) < 3 then
  begin // not enough points
    Result := False;
    Exit;
  end;

  // find pivot point, which is known to be on the hull
  // point with lowest y - if there are multiple, point with highest x
  LMinY := 1000;
  LMaxX := 1000;
  LPivotIndex := 0;
  for Lindex := 0 to High(APoints) do
  begin
    if APoints[Lindex].Y = LMinY then
    begin
      if APoints[Lindex].X > LMaxX then
      begin
        LMaxX := APoints[Lindex].X;
        LPivotIndex := Lindex;
      end;
    end
    else if APoints[Lindex].Y < LMinY then
    begin
      LMinY := APoints[Lindex].Y;
      LMaxX := APoints[Lindex].X;
      LPivotIndex := Lindex;
    end;
  end;
  // put pivot into seperate variable and remove from array
  LPivot := APoints[LPivotIndex];
  APoints[LPivotIndex] := APoints[High(APoints)];
  SetLength(APoints, High(APoints));

  // calculate angle to pivot for each point in the array
  // quicker to calculate dot product of point with a horizontal comparison vector
  SetLength(LAngles, Length(APoints));
  for Lindex := 0 to High(APoints) do
  begin
    LVecPoint.X := LPivot.X - APoints[Lindex].X; // point vector
    LVecPoint.Y := LPivot.Y - APoints[Lindex].Y;
    // reduce to a unit-vector - length 1
    LAngles[Lindex] := LVecPoint.X / Hypot(LVecPoint.X, LVecPoint.Y);
  end;

  // sort the points by angle
//  QuickSort(APoints, LAngles, 0, High(APoints));
  BubbleSort(APoints, LAngles);

  // step through array to remove points that are not part of the convex hull
  Lindex := 1;
  repeat
    // assign points behind and infront of current point
    if Lindex = 0 then LRightTurn := True
    else
    begin
      LBehind := APoints[Lindex - 1];
      if Lindex = High(APoints) then LInfront := LPivot
      else
        LInfront := APoints[Lindex + 1];

      // work out if we are making a right or left turn using vector product
      if ((LBehind.X - APoints[Lindex].X) * (LInfront.Y - APoints[Lindex].Y)) -
        ((LInfront.X - APoints[Lindex].X) * (LBehind.Y - APoints[Lindex].Y)) < 0 then
        LRightTurn := True
      else
        LRightTurn := False;
    end;

    if LRightTurn then
    begin // point is currently considered part of the hull
      Inc(Lindex); // go to next point
    end
    else
    begin // point is not part of the hull
      // remove point from convex hull
      if Lindex = High(APoints) then
      begin
        SetLength(APoints, High(APoints));
      end
      else
      begin
        Move(APoints[Lindex + 1], APoints[Lindex],
        (High(APoints) - Lindex) * SizeOf(TPoint) + 1);
        SetLength(APoints, High(APoints));
      end;

      Dec(Lindex); // backtrack to previous point
    end;

  until Lindex = High(APoints);

  // add pivot back into points array
  SetLength(APoints, Length(APoints) + 1);
  APoints[High(APoints)] := LPivot;
end;


//------------------------------------------------------------------------------
procedure ExtractPoints(Texture: TPHXTexture; Rect: TRect; out APoints: TPointArray);
var X,Y: Integer;
var GetPixel: TGetPixel;
var SrcColor: TPHXPixel;
var SrcPixel: PByte;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  for y := 0 to Texture.Height - 1 do
  begin
    SrcPixel:= Texture.ScanLine(Y);
    for x := 0 to Texture.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      if (SrcColor.Alpha > 0) and PtInRect(Rect, Point(X,Y)) then
      begin
        SetLength(APoints, Length(APoints)+1);

        APoints[ Length(APoints) - 1].X:= X;
        APoints[ Length(APoints) - 1].Y:= Y;
      end;
    end;
  end;

end;


{$ENDREGION}


{$REGION 'TPHXShapeEditor'}

//------------------------------------------------------------------------------
procedure DrawHandle(canvas: TCanvas; const Position: TVector2f; const Selected: Boolean);
const Size = 3;
begin
  if Selected then
  begin
    Canvas.Pen.Style:= psSolid;
    Canvas.Pen.Color:= clWhite;

    Canvas.Brush.Style:= bsSolid;
    Canvas.Brush.Color:= clYellow;
  end else
  begin
    Canvas.Pen.Style:= psSolid;
    Canvas.Pen.Color:= clYellow;

    Canvas.Brush.Style:= bsSolid;
    Canvas.Brush.Color:= clWhite;
  end;
  Canvas.Rectangle(
    Trunc(Position.X - Size),
    Trunc(Position.Y - Size),
    Trunc(Position.X + Size),
    Trunc(Position.Y + Size)
  );
end;

const ColorShapeSelected = clYellow;
const ColorShapeNormal   = clWindow;

// TPHXShapeEditor
//==============================================================================
constructor TPHXShapeEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered:= True;

  FBuffer    := TBitmap.Create;
  FMask      := TBitmap.Create;
  FBackground:= CreateTransparentImage(4);
  FImage     := nil;
  FPattern   := -1;
end;

//------------------------------------------------------------------------------
destructor TPHXShapeEditor.Destroy;
begin
  FBuffer.Free;
  FMask.Free;
  FBackground.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.GetDocumentSize(out Width, Height: Integer);
begin
  if Assigned(Image) then
  begin
    //if RelativeToPattern then
    //begin
    //  Width := Pattern.Width;
   //   Height:= Pattern.Height;
    //end else
    //begin
      Width := Image.Width;
      Height:= Image.Height;
    //end;
  end else
  begin
    Width := 0;
    Height:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.DrawShape(Shape: TPHXPoint; Selected: Boolean);
var P0: TVector2f;
begin
  with Canvas do
  begin

    Pen.Style:= psSolid;
    Pen.Color:= clYellow;

    Brush.Style:= bsClear;
    Brush.Color:= clBlack;

    if Selected then
    begin
      Pen.Color:= ColorShapeSelected;
    end else
    begin
      Pen.Color:= ColorShapeNormal;
    end;

    P0:= Shape.Position;
    P0:= PatternToScreen(P0);

    MoveTo( Trunc(P0.X)-2, Trunc(P0.Y)-2 );
    LineTo( Trunc(P0.X)+2, Trunc(P0.Y)-2 );
    LineTo( Trunc(P0.X)+2, Trunc(P0.Y)+2 );
    LineTo( Trunc(P0.X)-2, Trunc(P0.Y)+2 );
    LineTo( Trunc(P0.X)-2, Trunc(P0.Y)-2 );


     if Selected then
    begin
      Brush.Style:= bsSolid;
      Brush.Color:= clWhite;

      DrawHandle(Canvas, P0, False);
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.DrawShape(Shape: TPHXLine; Selected: Boolean);
var P0, P1: TVector2f;
begin
  with Canvas do
  begin

    Pen.Style:= psSolid;
    Pen.Color:= clYellow;

    Brush.Style:= bsClear;
    Brush.Color:= clBlack;

    if Selected then
    begin
      Pen.Color:= ColorShapeSelected;
    end else
    begin
      Pen.Color:= ColorShapeNormal;
    end;

    P0:= Shape.Min;
    P0:= PatternToScreen(P0);

    P1:= Shape.Max;
    P1:= PatternToScreen(P1);


    MoveTo( Trunc(P0.X), Trunc(P0.Y) );
    LineTo( Trunc(P1.X), Trunc(P1.Y) );

    if Selected then
    begin
      Brush.Style:= bsSolid;
      Brush.Color:= clWhite;

      DrawHandle(Canvas, P0, False);
      DrawHandle(Canvas, P1, False);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.DrawShape(Shape: TPHXBox; Selected: Boolean);
var P0, P1, P2, P3, PC: TVector2f;
begin
  with Canvas do
  begin
    Pen.Style:= psSolid;
    Pen.Color:= clYellow;

    Brush.Style:= bsClear;
    Brush.Color:= clBlack;

    if Selected then
    begin
      Pen.Color:= ColorShapeSelected;
    end else
    begin
      Pen.Color:= ColorShapeNormal;
    end;
    PC.X:= Shape.Center.X;
    PC.Y:= Shape.Center.Y;
    PC:= PatternToScreen(PC);

    P0.X:= Shape.Center.X - Shape.Width  / 2;
    P0.Y:= Shape.Center.Y - Shape.Height / 2;
    P0:= PatternToScreen(P0);

    P1.X:= Shape.Center.X + Shape.Width  / 2;
    P1.Y:= Shape.Center.Y - Shape.Height / 2;
    P1:= PatternToScreen(P1);

    P2.X:= Shape.Center.X + Shape.Width  / 2;
    P2.Y:= Shape.Center.Y + Shape.Height / 2;
    P2:= PatternToScreen(P2);

    P3.X:= Shape.Center.X - Shape.Width  / 2;
    P3.Y:= Shape.Center.Y + Shape.Height / 2;
    P3:= PatternToScreen(P3);

    MoveTo( Trunc(P3.X), Trunc(P3.Y) );
    LineTo( Trunc(P0.X), Trunc(P0.Y) );
    LineTo( Trunc(P1.X), Trunc(P1.Y) );
    LineTo( Trunc(P2.X), Trunc(P2.Y) );
    LineTo( Trunc(P3.X), Trunc(P3.Y) );

    if Selected then
    begin
      Brush.Style:= bsSolid;
      Brush.Color:= clWhite;

      DrawHandle(Canvas, PC, False);
      //DrawHandle(Canvas, P1, False);
     // DrawHandle(Canvas, P2, False);
    //  DrawHandle(Canvas, P3, False);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.DrawShape(Shape: TPHXPolygon; Selected: Boolean);
var J: Integer;
var P0, P1: TVector2f;
begin
  if Shape.Size = 0 then Exit;

  with Canvas do
  begin

    Pen.Style:= psSolid;
    Pen.Color:= clYellow;

    Brush.Style:= bsClear;
    Brush.Color:= clBlack;

    if Selected then
    begin
      Pen.Color:= ColorShapeSelected;
    end else
    begin
      Pen.Color:= ColorShapeNormal;
    end;

    // Draw lines
    P0:= Shape.Points[ Shape.Size - 1];
    P0:= PatternToScreen(P0);

    for J := 0 to Shape.Size - 1 do
    begin
      P1:= Shape.Points[J];
      P1:= PatternToScreen(P1);

      MoveTo( Trunc(P1.X), Trunc(P1.Y) );
      LineTo( Trunc(P0.X), Trunc(P0.Y) );

      P0:= P1;
    end;

    if Selected then
    begin
      // Draw rectangles
      for J := 0 to Shape.Size - 1 do
      begin
        P1:= Shape.Points[J];
        P1:= PatternToScreen(P1);

      //  if SelectedPoint = J then
       // begin
          Brush.Style:= bsSolid;
          Brush.Color:= clWhite;
       // end else
       // begin
       //   Brush.Color:= clGray;
       //   Brush.Style:= bsSolid;
      //  end;

        DrawHandle(Canvas, P1, False);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.DrawShape(Shape: TPHXCircle; Selected: Boolean);
var P0: TVector2f;
var R : Integer;
begin

  P0:= Shape.Center;
  P0:= PatternToScreen(P0);

  R:= Trunc(Shape.Radius * Viewport.Zoom);

  with Canvas do
  begin
    Brush.Style:= bsClear;

    if Selected then
    begin
      Pen.Color:= ColorShapeSelected;
    end else
    begin
      Pen.Color:= ColorShapeNormal;
    end;

    Ellipse(Trunc(P0.X)-R, Trunc(P0.Y)-R, Trunc(P0.X)+R, Trunc(P0.Y)+R);

    if Selected then
    begin
      Pen.Color:= ColorShapeNormal;

//      if SelectedPoint = 0 then
  //    begin
  //      Brush.Style:= bsSolid;
 //       Brush.Color:= clWhite;
  //    end else
   //   begin
        Brush.Color:= clGray;
        Brush.Style:= bsSolid;
    //  end;

      DrawHandle(Canvas, P0, False);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.PaintDocument(const Offset: TVector2i; const Zoom: Single);
var dstRect: TRect;
var srcRect: TRect;
begin
  if Assigned(Image)and (Image.Width > 0) and (Image.Height > 0) then
  begin
       {
    if RelativeToPattern then
    begin
      dstRect.Left  := Offset.X;
      dstRect.Top   := Offset.Y;
      dstRect.Right := Offset.X + Trunc(Pattern.Width  * Viewport.Zoom);
      dstRect.Bottom:= Offset.Y + Trunc(Pattern.Height * Viewport.Zoom);

      srcRect.Left  := Pattern.X;
      srcRect.Top   := Pattern.Y;
      srcRect.Right := Pattern.X + Pattern.Width;
      srcRect.Bottom:= Pattern.Y + Pattern.Height;

      Canvas.CopyRect(dstRect, FBuffer.Canvas, srcRect);
    end else
    begin
      dstRect.Left  := Offset.X;
      dstRect.Top   := Offset.Y;
      dstRect.Right := Offset.X + Trunc(Image.Width  * Viewport.Zoom);
      dstRect.Bottom:= Offset.Y + Trunc(Image.Height * Viewport.Zoom);

      Canvas.StretchDraw(dstRect, FBuffer);
    end;   }

    dstRect.Left  := Offset.X;
    dstRect.Top   := Offset.Y;
    dstRect.Right := Offset.X + Trunc(Image.Width  * Viewport.Zoom);
    dstRect.Bottom:= Offset.Y + Trunc(Image.Height * Viewport.Zoom);

    if RelativeToPattern then
    begin
      // Draw mask
      Canvas.StretchDraw(dstRect, FMask);

      dstRect.Left  := Offset.X + Trunc((APattern.X                  )* Viewport.Zoom);
      dstRect.Top   := Offset.Y + Trunc((APattern.Y                  )* Viewport.Zoom);
      dstRect.Right := Offset.X + Trunc((APattern.X + APattern.Width  )* Viewport.Zoom);
      dstRect.Bottom:= Offset.Y + Trunc((APattern.Y + APattern.Height )* Viewport.Zoom);

      srcRect.Left  := APattern.X;
      srcRect.Top   := APattern.Y;
      srcRect.Right := APattern.X + APattern.Width;
      srcRect.Bottom:= APattern.Y + APattern.Height;

      Canvas.CopyRect(dstRect, FBuffer.Canvas, srcRect);

    end else
    begin
      Canvas.StretchDraw(dstRect, FBuffer);
    end;


    Grid.Draw(Canvas, dstRect);
  end;

  if Assigned(Shape) then
  begin
    case Shape.Kind of
      PHX_SHAPE_POINT  : DrawShape(TPHXPoint  (Shape), True);
      PHX_SHAPE_BOX    : DrawShape(TPHXBox    (Shape), True);
      PHX_SHAPE_POLYGON: DrawShape(TPHXPolygon(Shape), True);
      PHX_SHAPE_CIRCLE : DrawShape(TPHXCircle (Shape), True);
      PHX_SHAPE_LINE   : DrawShape(TPHXLine   (Shape), True);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXShapeEditor.PatternToScreen(const Point: TVector2f): TVector2f;
var Vector:  TVector2f;
begin
  if RelativeToPattern then
  begin
    Vector.X:= Point.X + APattern.X + APattern.Pivot.X;
    Vector.Y:= Point.Y + APattern.Y + APattern.Pivot.Y;

    Result:= Viewport.DocumentToScreen(Vector);
  end else
  begin
    Result:= Viewport.DocumentToScreen(Point);
  end;
end;

//------------------------------------------------------------------------------
function TPHXShapeEditor.ScreenToPattern(const Point: TVector2f): TVector2f;
begin
  Result:= Viewport.ScreenToDocument( Point );

  if RelativeToPattern then
  begin
    Result.X:= Result.X - APattern.X - APattern.Pivot.X;
    Result.Y:= Result.Y - APattern.Y - APattern.Pivot.Y;
  end;
end;

//------------------------------------------------------------------------------
function TPHXShapeEditor.PatternToScreen(const X, Y: Single): TVector2f;
begin
  Result:= PatternToScreen(TVector2f.Create(x,Y));
end;

//------------------------------------------------------------------------------
function TPHXShapeEditor.ScreenToPattern(const X, Y: Single): TVector2f;
begin
  Result:= ScreenToPattern(TVector2f.Create(x,Y));
end;


//------------------------------------------------------------------------------
procedure TPHXShapeEditor.SetShape(const Value: TPHXShape);
begin
  FShape := Value;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.SetImage(const Value: TPHXImage);
begin
  if FImage <> Value then
  begin
    FImage := Value;

    Viewport.ZoomFit;
  end;

  if Assigned(FImage) then
  begin
    FImage.Draw(FBuffer, FBackground);
    FImage.Draw(FMask  , FBackground, 0.50);
  end else
  begin
    FBuffer.Width := 0;
    FBuffer.Height:= 0;

    FMask.Width := 0;
    FMask.Height:= 0;
  end;
  DocumentChanged;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXShapeEditor.SetPattern(const Value: Integer);
begin
  FPattern := Value;

  if Assigned(Image) and (Pattern >= 0) and (Pattern < Image.Patterns.Count) then
  begin
    APattern:= Image.Patterns[FPattern];

    Viewport.CenterX:= APattern.X + APattern.Pivot.X;
    Viewport.CenterY:= APattern.Y + APattern.Pivot.Y;

    FRelativeToPattern:= true;
  end else
  begin
    FRelativeToPattern:= False;
  end;

  Invalidate;
end;


function TPHXShapeEditor.GetPattern: TPHXPattern;
begin
  if Assigned(Image) and (Pattern >= 0) and (Pattern < Image.Patterns.Count) then
  begin
    Result:= Image.Patterns[FPattern];
  end;

end;


{$ENDREGION}




end.
