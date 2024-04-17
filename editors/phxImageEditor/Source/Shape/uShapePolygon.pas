unit uShapePolygon;

interface

uses
   Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls,  Spin, Grids,

  phxShape,
  phxTypes,
  phxImage,

  uShapeProperties,
  uShapeTools;

type

//------------------------------------------------------------------------------
TFrmShapePolygon = class(TFrame, IShapeEditor)
    sgPoints: TStringGrid;
    edSize: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    btnCreateFromImage: TButton;

    procedure FrameResize(Sender: TObject);

    procedure edSizeChange(Sender: TObject);
    procedure sgPointsExit(Sender: TObject);
    procedure sgPointsSelectCell(Sender: TObject; ACol, ARow: Integer;  var CanSelect: Boolean);
    procedure btnCreateFromImageClick(Sender: TObject);
  private
    FShape: TPHXPolygon;

    procedure Changed;

    procedure sgPointsUpdate;
    procedure EnableEvents(Enabled: Boolean);
    procedure EnableEditors(Enabled: Boolean);

    procedure SetShape(Value: TPHXPolygon); overload;
    procedure SetShape(Value: TPHXShape); overload;
  public
    constructor Create(AOwner: TComponent); override;

    property Shape: TPHXPolygon read FShape write SetShape;
  end;

implementation

{$R *.dfm}

Uses Math, uActions, uShape;



//------------------------------------------------------------------------------
constructor TFrmShapePolygon.Create(AOwner: TComponent);
begin
  inherited;

  sgPoints.Cells[0, 0]:= '#';
  sgPoints.Cells[1, 0]:= 'X';
  sgPoints.Cells[2, 0]:= 'Y';

  sgPoints.ColWidths[0]:= 32;
  sgPoints.ColWidths[1]:= 64;
  sgPoints.ColWidths[2]:= 64;

 // fimxme SetShape(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.FrameResize(Sender: TObject);
begin
  sgPoints.ColWidths[0]:= 32;
  sgPoints.ColWidths[1]:= (sgPoints.ClientWidth - 32-20) div 2 ;
  sgPoints.ColWidths[2]:= (sgPoints.ClientWidth - 32-20) div 2 ;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.Changed;
begin
  FrmShapeEditor.Changed;
end;


//------------------------------------------------------------------------------
procedure TFrmShapePolygon.EnableEditors(Enabled: Boolean);
const EnabledColors : array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edSize.Enabled := Enabled;
  edSize.Color   := EnabledColors[Enabled];

  sgPoints.Enabled:= Enabled;
  sgPoints.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edSize.OnChange:= edSizeChange;
  end else
  begin
    edSize.OnChange:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.edSizeChange(Sender: TObject);
var Value: Integer;
var Point: TVector2f;
begin
  Value:= Round(edSize.Value);

  if Shape.Size <> Value then
  begin
    if Value < Shape.Size then
    begin
      Shape.Size:= Value;
    end else
    begin

      if Shape.Size <> 0 then
      begin
        Point:= Shape.Points[Shape.Size-1];
      end else
      begin
        Point.X:= 0;
        Point.Y:= 0;
      end;

      while(Shape.Size < Value) do
      begin
        Shape.Add(Point);
      end;
    end;

    sgPointsUpdate;

    Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmShapePolygon.sgPointsUpdate;
var Index: Integer;
var Point: TVector2f;
begin
  if (Shape = nil) or (Shape.Size = 0) then
  begin
    sgPoints.Options:= sgPoints.Options - [goEditing];
    sgPoints.RowCount:= 2;

    sgPoints.Cells[0, 1]:= '';
    sgPoints.Cells[1, 1]:= '';
    sgPoints.Cells[2, 1]:= '';
  end else
  begin
    sgPoints.Options:= sgPoints.Options + [goEditing];
    sgPoints.RowCount:= 1 + Shape.Size;

    for Index := 0 to Shape.Size-1 do
    begin
      Point:= Shape.Points[Index];

      sgPoints.Cells[0, Index+1]:= IntToStr(Index+1);
      sgPoints.Cells[1, Index+1]:= FloatToStr(Point.X);
      sgPoints.Cells[2, Index+1]:= FloatToStr(Point.Y);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.sgPointsExit(Sender: TObject);
begin
  sgPointsUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.sgPointsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var Index: Integer;
var Point: TVector2f;
var Value: TVector2f;
begin
  if Assigned(Shape) then
  begin
    for Index := 0 to Shape.Size-1 do
    begin
      Point:= Shape.Points[Index];

      Value.X:= StrToFloatDef(sgPoints.Cells[1, Index+1], Point.X);
      Value.Y:= StrToFloatDef(sgPoints.Cells[2, Index+1], Point.Y);

      if (Point.X <> Value.X) or (Point.Y <> Value.Y) then
      begin
        Shape.Points[Index]:= Value;

        Changed;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePolygon.btnCreateFromImageClick(Sender: TObject);
var Pattern: TPHXPattern;
var Rect   : TRect;
var Points : TPointArray;
begin
  Screen.Cursor:= crHourGlass;
  try

    if FrmShapeEditor.Editor.RelativeToPattern then
    begin
      Pattern:= FrmShapeEditor.Editor.GetPattern;

      Rect.Left  := Pattern.X;
      Rect.Top   := Pattern.Y;
      Rect.Right := Pattern.X + Pattern.Width;
      Rect.Bottom:= Pattern.Y + Pattern.Height;
    end else
    begin
      Rect.Left  := 0;
      Rect.Top   := 0;
      Rect.Right := FrmShapeEditor.Editor.Image.Width;
      Rect.Bottom:= FrmShapeEditor.Editor.Image.Height;
    end;

    ExtractPoints(FrmShapeEditor.Editor.Image.Texture, Rect, Points);

    if FindConvexHull(Points) then
    begin
      UpdateShape(FrmShapeEditor.Editor, Shape, Points);

      SetShape(FShape);

      Changed;
    end;

  finally
    Screen.Cursor:= crDefault;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmShapePolygon.SetShape(Value: TPHXPolygon);
begin
  FShape:= Value;

  EnableEvents(False);

  if Assigned(Shape) then
  begin
    EnableEditors(True);

    edSize.Value:= Shape.Size;

    sgPointsUpdate;

    EnableEvents(True);
  end else
  begin
    EnableEditors(False);
  end;

end;
//------------------------------------------------------------------------------
procedure TFrmShapePolygon.SetShape(Value: TPHXShape);
begin
  if Value is TPHXPolygon then
  begin
    SetShape(TPHXPolygon(Value));
  end else
  begin
    SetShape(TPHXPolygon(nil));
  end;
end;


initialization
  RegisterShapeEditor(TPHXPolygon, TFrmShapePolygon);
end.
