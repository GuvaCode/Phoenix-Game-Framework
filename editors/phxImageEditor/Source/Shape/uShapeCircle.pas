unit uShapeCircle;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin,

  phxTypes,
  phxShape,

  uShapeProperties;

type

//------------------------------------------------------------------------------
TFrmShapeCircle = class(TFrame, IShapeEditor)
    Label4: TLabel;
    edCenterX: TSpinEdit;
    edCenterY: TSpinEdit;
    edRadius: TSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    procedure edCenterXChange(Sender: TObject);
    procedure edCenterYChange(Sender: TObject);
    procedure edRadiusChange(Sender: TObject);
  private
    FShape: TPHXCircle;

    procedure Changed;

    procedure EnableEvents(Enabled: Boolean);
    procedure EnableEditors(Enabled: Boolean);

    procedure SetShape(Value: TPHXCircle); overload;
    procedure SetShape(Value: TPHXShape); overload;
  public
    constructor Create(AOwner: TComponent); override;

    property Shape: TPHXCircle read FShape write SetShape;
  end;

implementation

{$R *.dfm}

uses uShape;


//------------------------------------------------------------------------------
constructor TFrmShapeCircle.Create(AOwner: TComponent);
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.Changed;
begin
  FrmShapeEditor.Changed;
end;


//------------------------------------------------------------------------------
procedure TFrmShapeCircle.EnableEditors(Enabled: Boolean);
const EnabledColors : array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edCenterX.Enabled:= Enabled;
  edCenterX.Color  := EnabledColors[Enabled];

  edCenterY.Enabled:= Enabled;
  edCenterY.Color  := EnabledColors[Enabled];

  edRadius .Enabled:= Enabled;
  edRadius .Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edCenterX.OnChange:= edCenterXChange;
    edCenterY.OnChange:= edCenterYChange;
    edRadius .OnChange:= edRadiusChange;
  end else
  begin
    edCenterX.OnChange:= nil;
    edCenterY.OnChange:= nil;
    edRadius .OnChange:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.edCenterXChange(Sender: TObject);
var Value: Single;
begin
  Value:= edCenterX.Value;

  if Shape.CenterX <> Value then
  begin
    Shape.CenterX:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.edCenterYChange(Sender: TObject);
var Value: Single;
begin
  Value:= edCenterY.Value;

  if Shape.CenterY <> Value then
  begin
    Shape.CenterX:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.edRadiusChange(Sender: TObject);
var Value: Single;
begin
  Value:= edRadius.Value;

  if Shape.Radius <> Value then
  begin
    Shape.Radius:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.SetShape(Value: TPHXCircle);
begin
  FShape := Value;

  EnableEvents(False);

  if Assigned(Shape) then
  begin
    EnableEditors(True);

    edCenterX.Value:= Shape.Center.X;
    edCenterY.Value:= Shape.Center.Y;
    edRadius .Value:= Shape.Radius;

    EnableEvents(True);
  end else
  begin
    EnableEditors(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeCircle.SetShape(Value: TPHXShape);
begin
  if Value is TPHXCircle then
  begin
    SetShape(TPHXCircle(Value));
  end else
  begin
    SetShape(TPHXCircle(nil));
  end;
end;


initialization
  RegisterShapeEditor(TPHXCircle, TFrmShapeCircle);
end.
