unit uEffect.Emittor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,
  Vcl.ExtCtrls, System.Math,

  JvExStdCtrls, JvEdit, JvExMask, JvSpin,

  phxTypes,
  phxParticle, JvToolEdit;

type

//------------------------------------------------------------------------------
TFrmEffectEmittor = class(TFrame)
    PanelPoints: TPanel;
    PaintBox1: TPaintBox;
    edPointsInOrder: TCheckBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    edRepeats: TJvSpinEdit;
    edCount: TJvSpinEdit;
    edDelay: TJvSpinEdit;
    Label13: TLabel;
    edMode: TComboBox;
    Label2: TLabel;
    edShape: TJvComboEdit;
    Label5: TLabel;
    Label3: TLabel;
    procedure edPointsInOrderClick(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure edShapeChange(Sender: TObject);
    procedure edDelayChange(Sender: TObject);
    procedure edCountChange(Sender: TObject);
    procedure edRepeatsChange(Sender: TObject);
    procedure edModeChange(Sender: TObject);
    procedure edShapeButtonClick(Sender: TObject);
  private
    FEffect: TPHXParticleEffect;

  //  FrmEmittorTemplateLine  : TFrmEmittorTemplateLine;
  //  FrmEmittorTemplateCircle: TFrmEmittorTemplateCircle;
  //  FrmEmittorTemplateBox   : TFrmEmittorTemplateBox;
  //  FrmEmittorTemplateBitmap: TFrmEmittorTemplateBitmap;

    procedure Changed;

  //  procedure EnableControls(Enabled: Boolean);
  //  procedure EnableEvents(Enabled: Boolean);

    procedure SetEffect(const Value: TPHXParticleEffect);
  public
    constructor Create(AOwner: TComponent); override;

    property Effect: TPHXParticleEffect read FEffect write SetEffect;
  end;


implementation

{$R *.dfm}

uses uActions, uEmissionPoints;

(*
    // Shape of the emittor, only used by the editor
    property Shape: TPHXParticleEmittorShape read FShape write FShape;
    // The emission mode, emit over time or distance traveled
    property Mode: TPHXEmissionMode read FMode write FMode;
    // Delay between each emission in seconds or units
    property Delay: Single read FDelay write FDelay;
    // Number of particles to emit
    property Count: Integer read FCount write FCount;
    // Repeat count of the effect, zero is infinite
    property Repeats: Word read FRepeats write FRepeats;
    // The points to emit from
    property Points: TPHXVectorList3f read FPoints;
    // Use the poins in order instead of randomly
    property PointsInOrder: Boolean read FPointsInOrder write FPointsInOrder;

*)

//------------------------------------------------------------------------------
function ModeToStr(const Mode: TPHXEmissionMode): String;
begin
  Result:= '';
  case Mode of
    emTime    : Result:= 'Time';
    emDistance: Result:= 'Distance';
  end;
end;

//------------------------------------------------------------------------------
function ShapeToStr(const Shape: TPHXParticleEmittorShape): String;
begin
  Result:= '';
  case Shape of
    esCustom   : Result:= 'Custom';
    esLine     : Result:= 'Line';
    esRectangle: Result:= 'Rectangle';
    esCircle   : Result:= 'Circle';
  end;
end;

// TFrmEffectEmittor
//==============================================================================
constructor TFrmEffectEmittor.Create(AOwner: TComponent);
begin
  inherited;

 // FrmEmittorTemplateLine  := TFrmEmittorTemplateLine.Create(Self);
//  FrmEmittorTemplateCircle:= TFrmEmittorTemplateCircle.Create(Self);
 // FrmEmittorTemplateBox   := TFrmEmittorTemplateBox.Create(Self);
 // FrmEmittorTemplateBitmap:= TFrmEmittorTemplateBitmap.Create(Self);

 // pnEmittorTemplate.Color:= clBtnFace;

  SetEffect(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.Changed;
begin
  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edModeChange(Sender: TObject);
var Index: Integer;
var Value: TPHXEmissionMode;
begin
  Index:= edMode.Items.IndexOf(edMode.Text);

  if Index = -1 then Exit;

  Value:= TPHXEmissionMode(Index);

  if Effect.Emittor.Mode <> Value then
  begin
    Effect.Emittor.Mode:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edDelayChange(Sender: TObject);
var Value: Single;
begin
  Value:= edDelay.Value;

  if Effect.Emittor.Delay <> Value then
  begin
    Effect.Emittor.Delay:= Value;

    Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edCountChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edCount.Value);

  if Effect.Emittor.Count <> Value then
  begin
    Effect.Emittor.Count:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edRepeatsChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edRepeats.Value);

  if Effect.Emittor.Repeats <> Value then
  begin
    Effect.Emittor.Repeats:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edPointsInOrderClick(Sender: TObject);
var Value: Boolean;
begin
  Value:= edPointsInOrder.Checked;

  if Effect.Emittor.PointsInOrder <> Value then
  begin
    Effect.Emittor.PointsInOrder:= Value;

    Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edShapeButtonClick(Sender: TObject);
begin
  FrmEmissionPoints.Execute(FEffect);

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.edShapeChange(Sender: TObject);
begin
//
end;


//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.SetEffect(const Value: TPHXParticleEffect);
begin
  FEffect := Value;

  if Assigned(Effect) then
  begin
    edMode   .Enabled:= True;
    edShape  .Enabled:= True;
    edDelay  .Enabled:= True;
    edCount  .Enabled:= True;
    edRepeats.Enabled:= True;

    edMode   .Text := ModeToStr(Effect.Emittor.Mode);
    edDelay  .Value:= Effect.Emittor.Delay;
    edCount  .Value:= Effect.Emittor.Count;
    edRepeats.Value:= Effect.Emittor.Repeats;
    edShape.Text   := ShapeToStr(Effect.Emittor.Shape);

    edPointsInOrder.Checked:= Effect.Emittor.PointsInOrder;
  end else
  begin
    edMode   .Enabled:= False;
    edShape  .Enabled:= False;
    edDelay  .Enabled:= False;
    edCount  .Enabled:= False;
    edRepeats.Enabled:= False;

    edMode .Text:= '';
    edShape.Text:= '';
  end;

  PaintBox1.Invalidate;
end;

    {
//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWhite);
begin
  cbPointsInOrder.Enabled:= Enabled;
  cbPointsInOrder.Color  := EnabledColors[Enabled];

  edCount.Enabled:= Enabled;
  edCount.Color  := EnabledColors[Enabled];

  cbTemplateType.Enabled:= Enabled;
  cbTemplateType.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    cbPointsInOrder           .OnClick:= cbPointsInOrderClick;

  end else
  begin
    cbPointsInOrder           .OnClick:= nil;
  end;
end;
}






//------------------------------------------------------------------------------
procedure TFrmEffectEmittor.PaintBox1Paint(Sender: TObject);
var Index : Integer;
var Bounds: TRectf;
var Offset: TVector2i;
var ScaleW : Single;
var ScaleH : Single;
var Scale : Single;
var W,H: Single;
var X,Y: Integer;
var Point: TVector3f;
begin
  with PaintBox1.Canvas do
  begin
    Brush.Color:= clWhite;

    FillRect(ClipRect);
  end;

  if (Effect = nil) or (Effect.Emittor.Points.Count = 0) then Exit;

  Bounds.Left  := Effect.Emittor.Points[0].X;
  Bounds.Right := Effect.Emittor.Points[0].X;
  Bounds.Top   := Effect.Emittor.Points[0].Y;
  Bounds.Bottom:= Effect.Emittor.Points[0].Y;

  for Index := 1 to Effect.Emittor.Points.Count - 1 do
  begin
    Point:= Effect.Emittor.Points[Index];
    Bounds.Left  := Min(Bounds.Left  , Point.X);
    Bounds.Right := Max(Bounds.Right , Point.X);
    Bounds.Top   := Min(Bounds.Top   , Point.Y);
    Bounds.Bottom:= Max(Bounds.Bottom, Point.Y);
  end;

  // Avoid division by zero
//  if (Bounds.Right = Bounds.Left) or (Bounds.Bottom = Bounds.Top) then Exit;

  W:= (Bounds.Right   - Bounds.Left);
  H:= (Bounds.Bottom  - Bounds.Top);

  if W <> 0 then
  begin
    ScaleW:= (PaintBox1.Width  - 16) / (Bounds.Right  - Bounds.Left) ;
  end else
  begin
    ScaleW:= 0;
  end;

  if H <> 0 then
  begin
    ScaleH:= (PaintBox1.Height - 16) / (Bounds.Bottom  - Bounds.Top) ;
  end else
  begin
    ScaleH:= 0;
  end;

  if ScaleW > ScaleH then
  begin
    if ScaleH <> 0 then
    begin
      Scale:= ScaleH;
    end else
    begin
      Scale:= ScaleW;
    end;
  end else
  begin
    if ScaleW <> 0 then
    begin
      Scale:= ScaleW;
    end else
    begin
      Scale:= ScaleH;
    end;
  end;

  Offset.X:= PaintBox1.Width  div 2 - Round( Scale * ((Bounds.Left + Bounds.Right ) / 2));
  Offset.Y:= PaintBox1.Height div 2 - Round( Scale * ((Bounds.Top  + Bounds.Bottom) / 2)) ;

  with PaintBox1.Canvas do
  begin
    Pen.Color:= clSilver;
    Pen.Style := psDot;

    // Draw x-axis
    MoveTo(0                , Offset.Y);
    LineTo(PaintBox1.Width-0, Offset.Y);

    // Draw y-axis
    MoveTo(Offset.X, 0);
    LineTo(Offset.X, PaintBox1.Height-0);

    Pen.Color:= clMaroon;
    Pen.Style:= psSolid;

    Brush.Color:= clBlack;
    for Index := 0 to Effect.Emittor.Points.Count - 1 do
    begin
      X:= Offset.X + Round(Scale * Effect.Emittor.Points[Index].X);
      Y:= Offset.Y + Round(Scale * Effect.Emittor.Points[Index].Y);

      MoveTo(X  , Y);
      LineTo(X+1, Y);
    end;
  end;
end;








end.
