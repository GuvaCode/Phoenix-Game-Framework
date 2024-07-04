unit uEffect.Physics;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,
  System.Math, Vcl.ExtCtrls ,

  JvExStdCtrls, JvEdit, JvExMask, JvSpin,

  phxTypes,
  phxMath,
  phxParticle;

type

//------------------------------------------------------------------------------
TFrmEffectPhysics = class(TFrame)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label10: TLabel;
    edVelocityMin: TJvSpinEdit;
    edVelocityMax: TJvSpinEdit;
    edSpinMax: TJvSpinEdit;
    edSpinMin: TJvSpinEdit;
    GroupBox1: TGroupBox;
    Label7: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    edDirectionX: TJvSpinEdit;
    edDirectionY: TJvSpinEdit;
    edDirectionZ: TJvSpinEdit;
    edSpread: TJvSpinEdit;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    Label4: TLabel;
    Label12: TLabel;
    edWorldAccelerationX: TJvSpinEdit;
    edWorldAccelerationY: TJvSpinEdit;
    edWorldAccelerationZ: TJvSpinEdit;
    GroupBox4: TGroupBox;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    RadioXY: TRadioButton;
    RadioButton2: TRadioButton;
    procedure edDirectionXChange(Sender: TObject);
    procedure edDirectionYChange(Sender: TObject);
    procedure edDirectionZChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure RadioXYClick(Sender: TObject);
    procedure edSpreadChange(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
  private
    FEffect: TPHXParticleEffect;

    procedure Changed;

    procedure EnableControls(Enabled: Boolean);
    procedure EnableEvents(Enabled: Boolean);

    procedure SetEffect(const Value: TPHXParticleEffect);
  public
    constructor Create(AOwner: TComponent); override;

    property Effect: TPHXParticleEffect read FEffect write SetEffect;
  end;


implementation

{$R *.dfm}

uses uActions;

//------------------------------------------------------------------------------
procedure GetSpreadVectors(Effect: TPHXParticleEffect; out Min: TVector3f; out Max: TVector3f);
begin
  Min:= Effect.Direction;
  Max:= Effect.Direction;

  case Effect.Shape of
    psAlignX, psAlignXRotated:
    begin
      Min:= Matrix_Rotate(Matrix_CreateRotationX(-Effect.Spread ), Effect.Direction);
      Max:= Matrix_Rotate(Matrix_CreateRotationX( Effect.Spread ), Effect.Direction);
    end;
    psAlignY, psAlignYRotated:
    begin
      Min:= Matrix_Rotate(Matrix_CreateRotationY(-Effect.Spread ), Effect.Direction);
      Max:= Matrix_Rotate(Matrix_CreateRotationY( Effect.Spread ), Effect.Direction);
    end;
    psAlignZ, psAlignZRotated:
    begin
      Min:= Matrix_Rotate(Matrix_CreateRotationZ(-Effect.Spread ), Effect.Direction);
      Max:= Matrix_Rotate(Matrix_CreateRotationZ( Effect.Spread ), Effect.Direction);
    end;
    psPoint, psBillboard, psBillboardRotated:
    begin
      // TODO: Should accelerate along the view plane, need the view matrix for that
     // SpreadMatrix:= Matrix_Rotation( RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread), RandomInterval(-Spread, Spread) );

    end;
  end;
end;

// TFrmEffectPhysics
//==============================================================================
constructor TFrmEffectPhysics.Create(AOwner: TComponent);
begin
  inherited;

  EnableControls(False);
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.SetEffect(const Value: TPHXParticleEffect);
begin
  EnableEvents(False);

  FEffect := Value;

  if Assigned(Effect) then
  begin
    EnableControls(True);

    edVelocityMin.Value:= Effect.VelocityMin;
    edVelocityMax.Value:= Effect.VelocityMax;

    edSpread.Value:= Effect.Spread;

    edSpinMin.Value:= Effect.Spin;
    edSpinMax.Value:= Effect.SpinVariance;

    edWorldAccelerationX.Value:= Effect.AccelerationX;
    edWorldAccelerationY.Value:= Effect.AccelerationY;
    edWorldAccelerationZ.Value:= Effect.AccelerationZ;

    edDirectionX.Value:= Effect.DirectionX;
    edDirectionY.Value:= Effect.DirectionY;
    edDirectionZ.Value:= Effect.DirectionZ;

  //  edInitalUpdateCount    .Value:= Effect.InitalUpdateCount;
  //  edInitalUpdateInterval .Value:= Effect.InitalUpdateInterval;


    EnableEvents(true);
  end else
  begin
    EnableControls(False);
  end;

  PaintBox1.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWhite);
begin
  edDirectionX.Enabled:= Enabled;
  edDirectionX.Color  := EnabledColors[Enabled];

  edDirectionY.Enabled:= Enabled;
  edDirectionY.Color  := EnabledColors[Enabled];

  edDirectionZ.Enabled:= Enabled;
  edDirectionZ.Color  := EnabledColors[Enabled];

  edSpread.Enabled:= Enabled;
  edSpread.Color  := EnabledColors[Enabled];

  edVelocityMin.Enabled:= Enabled;
  edVelocityMin.Color  := EnabledColors[Enabled];

  edVelocityMax.Enabled:= Enabled;
  edVelocityMax.Color  := EnabledColors[Enabled];

  edSpinMin.Enabled:= Enabled;
  edSpinMin.Color  := EnabledColors[Enabled];

  edSpinMax.Enabled:= Enabled;
  edSpinMax.Color  := EnabledColors[Enabled];


  edWorldAccelerationX.Enabled:= Enabled;
  edWorldAccelerationX.Color  := EnabledColors[Enabled];

  edWorldAccelerationY.Enabled:= Enabled;
  edWorldAccelerationY.Color  := EnabledColors[Enabled];

  edWorldAccelerationZ.Enabled:= Enabled;
  edWorldAccelerationZ.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edDirectionX           .OnChange:= edDirectionXChange;
    edDirectionY           .OnChange:= edDirectionYChange;
    edDirectionZ           .OnChange:= edDirectionZChange;

    edSpread.OnChange:= edSpreadChange;
  end else
  begin
    edDirectionX           .OnChange:= nil;
    edDirectionY           .OnChange:= nil;
    edDirectionZ           .OnChange:= nil;

    edSpread.OnChange:= nil;
  end;
       {
  edName           .OnChange:= EnabledEvents[Enabled];
  edDelay          .OnChange:= EnabledEvents[Enabled];
  edEmissionMode   .OnChange:= EnabledEvents[Enabled];
  edEmissionDelay  .OnChange:= EnabledEvents[Enabled];
  edEmissionCount  .OnChange:= EnabledEvents[Enabled];
  edEmissionRepeat .OnChange:= EnabledEvents[Enabled];

  edQuota         .OnChange:= EnabledEvents[Enabled];
  edLifeMin       .OnChange:= EnabledEvents[Enabled];
  edLifeMax       .OnChange:= EnabledEvents[Enabled];

  edInitalUpdateCount       .OnChange:= EnabledEvents[Enabled];
  edInitalUpdateInterval    .OnChange:= EnabledEvents[Enabled];


  edLink  .OnClick:= EnabledEvents[Enabled];
  edIdleWhileParentActive  .OnClick:= EnabledEvents[Enabled];    }
end;

procedure TFrmEffectPhysics.GroupBox1Click(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.Changed;
begin
  ModActions.Document.State:=  ModActions.Document.State + [dsChanged];
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.PaintBox1Paint(Sender: TObject);
var Center: TPoint;
var Radius: Integer;
var Rect: TRect;

var P1, P2: TPoint;

var Direction: TVector3f;
var SpreadMin: TVector3f;
var SpreadMax: TVector3f;
begin
  with TPaintBox(Sender).Canvas do
  begin
    Brush.Color:= clBtnFace;

    FillRect(ClientRect);

    // Cacluclate the center of the circle
    Center.X:= PaintBox1.Width  div 2;
    Center.Y:= PaintBox1.Height div 2;
    // Calculate the radius
    Radius:= (Min(PaintBox1.Width, PaintBox1.Height) - 8) div 2;
    // Bounding box of the circle
    Rect.Left  := Center.X - Radius;
    Rect.Right := Center.X + Radius;
    Rect.Top   := Center.Y - Radius;
    Rect.Bottom:= Center.Y + Radius;

    //Pen.Color:= clBlack;
    //Pen.Style:= psSolid;
    //Ellipse(Rect);

    //Pen.Color:= clBlack;
    //Pen.Style:= psDot;
    //MoveTo(0, Center.Y); LineTo(PaintBox1.Width, Center.Y);
    //MoveTo(Center.X, 0); LineTo(Center.X       , PaintBox1.Height);

    if Effect = nil then Exit;

    Direction:= VectorNormalize(Effect.Direction);

    GetSpreadVectors(Effect, SpreadMin, SpreadMax);

    Pen.Color:= clBlack;
    Pen.Style:= psSolid;
    // Draw direction
    if RadioXY.Checked then
    begin
      P1.X:= Center.X + Round(Direction.X * Radius);
      P1.Y:= Center.Y + Round(Direction.Y * Radius);

      MoveTo(Center.X, Center.Y); LineTo(P1.X, P1.Y);
    end;

    Pen.Color:= clGreen;
    Pen.Style:= psSolid;
    // Draw minimum spread
    if RadioXY.Checked then
    begin
      P2.X:= Center.X + Round(SpreadMin.X * Radius);
      P2.Y:= Center.Y + Round(SpreadMin.Y * Radius);

      MoveTo(Center.X, Center.Y);  LineTo(P2.X, P2.Y);

      Pen.Style:= psDot;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, P1.X, P1.Y, P2.X, P2.Y);
    end;
    Pen.Color:= clRed;
    Pen.Style:= psSolid;
    // Draw minimum spread
    if RadioXY.Checked then
    begin
      P2.X:= Center.X + Round(SpreadMax.X * Radius);
      P2.Y:= Center.Y + Round(SpreadMax.Y * Radius);

      MoveTo(Center.X, Center.Y);  LineTo(P2.X, P2.Y);

      Pen.Style:= psDot;
      Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, P2.X, P2.Y, P1.X, P1.Y);
    end;

  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.RadioXYClick(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;



//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.edDirectionXChange(Sender: TObject);
var Value: Single;
begin
  Value:= edDirectionX.Value;

  if Effect.DirectionX <> Value then
  begin
    Effect.DirectionX:= Value;

    Changed;
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.edDirectionYChange(Sender: TObject);
var Value: Single;
begin
  Value:= edDirectionY.Value;

  if Effect.DirectionY <> Value then
  begin
    Effect.DirectionY:= Value;

    Changed;
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.edDirectionZChange(Sender: TObject);
var Value: Single;
begin
  Value:= edDirectionZ.Value;

  if Effect.DirectionZ <> Value then
  begin
    Effect.DirectionZ:= Value;

    Changed;
  end;

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPhysics.edSpreadChange(Sender: TObject);
var Value: Single;
begin
  Value:= edSpread.Value;

  if Effect.Spread <> Value then
  begin
    Effect.Spread:= Value;

    Changed;
  end;
  PaintBox1.Invalidate;
end;




end.
