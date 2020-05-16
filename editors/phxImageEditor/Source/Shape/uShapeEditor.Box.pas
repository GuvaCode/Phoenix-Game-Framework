unit uShapeEditor.Box;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,

  JvExMask, JvSpin,

  phxTypes,
  phxShape;

type

//------------------------------------------------------------------------------
TFrmShapeBox = class(TFrame)
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edCenterX: TJvSpinEdit;
    edCenterY: TJvSpinEdit;
    edWidth: TJvSpinEdit;
    edHeight: TJvSpinEdit;
    procedure edCenterXChange(Sender: TObject);
    procedure edCenterYChange(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
    procedure edHeightChange(Sender: TObject);
  private
    FShape: TPHXBox;

    procedure Changed;

    procedure EnableEvents(Enabled: Boolean);
    procedure EnableEditors(Enabled: Boolean);

    procedure SetShape(const Value: TPHXBox);
  public
    constructor Create(AOwner: TComponent); override;

    property Shape: TPHXBox read FShape write SetShape;
  end;

implementation

{$R *.dfm}

uses uActions;

// TFrmShapeBox
//==============================================================================
constructor TFrmShapeBox.Create(AOwner: TComponent);
begin
  inherited;

  EnableEditors(False);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeBox.SetShape(const Value: TPHXBox);
begin
  FShape := Value;

  EnableEvents(False);

  if Assigned(Shape) then
  begin
    EnableEditors(True);

    edCenterX.Value:= Shape.Center.X;
    edCenterY.Value:= Shape.Center.Y;
    edWidth  .Value:= Shape.Width;
    edHeight .Value:= Shape.Height;

    EnableEvents(True);
  end else
  begin
    EnableEditors(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeBox.Changed;
begin
  ModActions.Document.State:= ModActions.Document.State + [dsChanged];
  ModActions.Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeBox.EnableEditors(Enabled: Boolean);
const EnabledColors : array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edCenterX.Enabled:= Enabled;
  edCenterX.Color  := EnabledColors[Enabled];

  edCenterY.Enabled:= Enabled;
  edCenterY.Color  := EnabledColors[Enabled];

  edWidth .Enabled:= Enabled;
  edWidth .Color  := EnabledColors[Enabled];

  edHeight .Enabled:= Enabled;
  edHeight .Color  := EnabledColors[Enabled];

end;

//------------------------------------------------------------------------------
procedure TFrmShapeBox.EnableEvents(Enabled: Boolean);
begin

  if Enabled then
  begin
    edCenterX.OnChange:= edCenterXChange;
    edCenterY.OnChange:= edCenterYChange;
    edWidth  .OnChange:= edWidthChange;
    edHeight .OnChange:= edHeightChange;
  end else
  begin
    edCenterX.OnChange:= nil;
    edCenterY.OnChange:= nil;
    edWidth  .OnChange:= nil;
    edHeight .OnChange:= nil;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmShapeBox.edCenterXChange(Sender: TObject);
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
procedure TFrmShapeBox.edCenterYChange(Sender: TObject);
var Value: Single;
begin
  Value:= edCenterY.Value;

  if Shape.CenterY <> Value then
  begin
    Shape.CenterY:= Value;

    Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmShapeBox.edWidthChange(Sender: TObject);
var Value: Single;
begin
  Value:= edWidth.Value;

  if Shape.Width <> Value then
  begin
    Shape.Width:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeBox.edHeightChange(Sender: TObject);
var Value: Single;
begin
  Value:= edHeight.Value;

  if Shape.Height <> Value then
  begin
    Shape.Height:= Value;

    Changed;
  end;
end;



end.
