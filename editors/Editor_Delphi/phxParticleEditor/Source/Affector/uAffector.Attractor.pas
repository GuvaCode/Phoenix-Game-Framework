unit uAffector.Attractor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  uEffect.Affector,

  phxParticle,
  phxParticleAffectors;

type

//------------------------------------------------------------------------------
TFrmAffectorAttractor = class(TFrame, IAffectorEditor)
    LabelForce: TLabel;
    EditForce: TEdit;
    EditPositionX: TEdit;
    Label1: TLabel;
    EditPositionY: TEdit;
    Label2: TLabel;
    EditPositionZ: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditRelative: TCheckBox;
    procedure EditForceChange(Sender: TObject);
    procedure EditPositionXChange(Sender: TObject);
    procedure EditPositionYChange(Sender: TObject);
    procedure EditPositionZChange(Sender: TObject);
    procedure EditRelativeClick(Sender: TObject);
  private
    FAffector: TPHXParticleAttractor;

    function GetTitle: String;

    procedure SetAffector(Value: TPHXParticleAffector); overload;
    procedure SetAffector(Value: TPHXParticleAttractor); overload;
  public
    constructor Create(AOwner: TComponent); override;

    property Affector: TPHXParticleAttractor read FAffector write SetAffector;
  end;

implementation

{$R *.dfm}

uses uActions;

// TFrmAffectorAttractor
//==============================================================================
constructor TFrmAffectorAttractor.Create(AOwner: TComponent);
begin
  inherited;
  //  FPosition: TVector3f;
 //   FForce   : Single;
 //   FRelativeToSystem: Boolean;

end;

//------------------------------------------------------------------------------
function TFrmAffectorAttractor.GetTitle: String;
begin
  Result:= 'Attractor';
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.EditForceChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(EditForce.Text, FAffector.Force);

  if FAffector.Force <> Value then
  begin
    FAffector.Force:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.EditPositionXChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(EditPositionX.Text, FAffector.PositionX);

  if FAffector.PositionX <> Value then
  begin
    FAffector.PositionX:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.EditPositionYChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(EditPositionY.Text, FAffector.PositionY);

  if FAffector.PositionY <> Value then
  begin
    FAffector.PositionY:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.EditPositionZChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(EditPositionZ.Text, FAffector.PositionZ);

  if FAffector.PositionZ <> Value then
  begin
    FAffector.PositionZ:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.EditRelativeClick(Sender: TObject);
var Value: Boolean;
begin
  Value:= EditRelative.Checked;


  if FAffector.RelativeToSystem <> Value then
  begin
    FAffector.RelativeToSystem:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.SetAffector(Value: TPHXParticleAttractor);
begin
  FAffector:= Value;

  if Assigned(FAffector) then
  begin
    EditForce.Text:= FloatToStr(FAffector.Force);

    EditPositionX.Text:=FloatToStr(FAffector.PositionX);
    EditPositionY.Text:=FloatToStr(FAffector.PositionY);
    EditPositionZ.Text:=FloatToStr(FAffector.PositionZ);

    EditRelative.Checked:= FAffector.RelativeToSystem;
  end else
  begin
    EditForce.Text:= '';

    EditPositionX.Text:= '';
    EditPositionY.Text:= '';
    EditPositionZ.Text:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAffectorAttractor.SetAffector(Value: TPHXParticleAffector);
begin
  if Value is TPHXParticleAttractor then
  begin
    SetAffector(TPHXParticleAttractor(Value));
  end;
end;



initialization
  RegisterAffectorEditor(TPHXParticleAttractor, TFrmAffectorAttractor);
end.
