unit uShapePoint;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs,StdCtrls,

  Spin,

  phxShape,
  phxTypes,

  uShapeProperties;

type

//------------------------------------------------------------------------------
TFrmShapePoint = class(TFrame, IShapeEditor)
    Label1: TLabel;
    edPositionX: TSpinEdit;
    edPositionY: TSpinEdit;
    Label4: TLabel;
    procedure edPositionXChange(Sender: TObject);
    procedure edPositionYChange(Sender: TObject);
  private
    FShape: TPHXPoint;

    procedure Changed;

    procedure EnableEvents(Enabled: Boolean);
    procedure EnableEditors(Enabled: Boolean);

    procedure SetShape(Value: TPHXPoint); overload;
    procedure SetShape(Value: TPHXShape); overload;
  public
    constructor Create(AOwner: TComponent); override;

    property Shape: TPHXPoint read FShape write SetShape;
  end;

implementation

{$R *.dfm}

uses uShape;

// TFrmShapePoint
//==============================================================================
constructor TFrmShapePoint.Create(AOwner: TComponent);
begin
  inherited;

  EnableEditors(False);
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.Changed;
begin
  FrmShapeEditor.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.EnableEditors(Enabled: Boolean);
const EnabledColors : array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edPositionX.Enabled:= Enabled;
  edPositionX.Color  := EnabledColors[Enabled];

  edPositionY.Enabled:= Enabled;
  edPositionY.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edPositionX.OnChange:= edPositionXChange;
    edPositionY.OnChange:= edPositionYChange;
  end else
  begin
    edPositionX.OnChange:= nil;
    edPositionY.OnChange:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.edPositionXChange(Sender: TObject);
var Value: Single;
begin
  Value:= edPositionX.Value;

  if Shape.PositionX <> Value then
  begin
    Shape.PositionX:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.edPositionYChange(Sender: TObject);
var Value: Single;
begin
  Value:= edPositionY.Value;

  if Shape.PositionY <> Value then
  begin
    Shape.PositionY:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.SetShape(Value: TPHXPoint);
begin
  FShape:= Value;

  EnableEvents(False);

  if Assigned(Shape) then
  begin
    EnableEditors(True);

    edPositionX.Value:= Shape.PositionX;
    edPositionY.Value:= Shape.PositionY;

    EnableEvents(True);
  end else
  begin
    EnableEditors(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapePoint.SetShape(Value: TPHXShape);
begin
  if Value is TPHXPoint then
  begin
    SetShape(TPHXPoint(Value));
  end else
  begin
    SetShape(TPHXPoint(nil));
  end;
end;


initialization
  RegisterShapeEditor(TPHXPoint, TFrmShapePoint);

end.
