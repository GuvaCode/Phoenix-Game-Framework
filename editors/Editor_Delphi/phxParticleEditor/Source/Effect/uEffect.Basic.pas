unit uEffect.Basic;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,

  JvExStdCtrls, JvEdit, JvExMask, JvSpin,

  phxParticle, JvMemo ;

type

//------------------------------------------------------------------------------
TFrmEffectBasic = class(TFrame)
    edLink: TCheckBox;
    edIdleWhileParentActive: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    edName: TEdit;
    edAuthor: TEdit;
    edComment: TMemo;
    edVersion: TEdit;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edDelay: TJvSpinEdit;
    edQuota: TJvSpinEdit;
    edLifeValue: TJvSpinEdit;
    edLifeVariance: TJvSpinEdit;
    Label7: TLabel;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    edInitalUpdateInterval: TJvSpinEdit;
    edInitalUpdateCount: TJvSpinEdit;
    procedure edNameChange(Sender: TObject);
    procedure edLifeValueChange(Sender: TObject);
    procedure edLifeVarianceChange(Sender: TObject);
    procedure edQuotaChange(Sender: TObject);
    procedure edDelayChange(Sender: TObject);
    procedure edAuthorChange(Sender: TObject);
    procedure edVersionChange(Sender: TObject);
    procedure edCommentChange(Sender: TObject);
    procedure edInitalUpdateIntervalChange(Sender: TObject);
    procedure edInitalUpdateCountChange(Sender: TObject);
  private
    FEffect: TPHXParticleEffect;

    procedure Changed;

    procedure EnableControls(Enabled: Boolean);

    procedure SetEffect(const Value: TPHXParticleEffect);
  public
    constructor Create(AOwner: TComponent); override;

    property Effect: TPHXParticleEffect read FEffect write SetEffect;
  end;

implementation

{$R *.dfm}

uses uActions;

// TFrmEffectBasic
//==============================================================================
constructor TFrmEffectBasic.Create(AOwner: TComponent);
begin
  inherited;

  SetEffect(nil);
end;


//------------------------------------------------------------------------------
procedure TFrmEffectBasic.Changed;
begin
  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edName.Text;

  if Effect.Name <> Value then
  begin
    Effect.Name:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edAuthorChange(Sender: TObject);
var Value: String;
begin
  Value:= edAuthor.Text;

  if Effect.Author <> Value then
  begin
    Effect.Author:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edVersionChange(Sender: TObject);
var Value: String;
begin
  Value:= edVersion.Text;

  if Effect.Version <> Value then
  begin
    Effect.Version:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edCommentChange(Sender: TObject);
var Value: String;
begin
  Value:= edComment.Text;

  if Effect.Comment <> Value then
  begin
    Effect.Comment:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edDelayChange(Sender: TObject);
var Value: Single;
begin
  Value:= edDelay.Value;

  if Effect.Delay <> Value then
  begin
    Effect.Delay:= Value;

    Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edQuotaChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Trunc(edQuota.Value);

  if Effect.Quota <> Value then
  begin
    Effect.Quota:= Value;

    Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edLifeValueChange(Sender: TObject);
var Value: Single;
begin
  Value:= edLifeValue.Value;

  if Effect.LifeValue <> Value then
  begin
    Effect.LifeValue:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edLifeVarianceChange(Sender: TObject);
var Value: Single;
begin
  Value:= edLifeVariance.Value;

  if Effect.LifeVariance <> Value then
  begin
    Effect.LifeVariance:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edInitalUpdateCountChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edInitalUpdateCount.Value);

  if Effect.InitalUpdateCount <> Value then
  begin
    Effect.InitalUpdateCount:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectBasic.edInitalUpdateIntervalChange(Sender: TObject);
var Value: Single;
begin
  Value:= edInitalUpdateInterval.Value;

  if Effect.InitalUpdateInterval <> Value then
  begin
    Effect.InitalUpdateInterval:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmEffectBasic.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWhite);
begin
  edName.Enabled:= Enabled;
  edName.Color:= EnabledColors[Enabled];

  edAuthor.Enabled:= Enabled;
  edAuthor.Color:= EnabledColors[Enabled];

  edVersion.Enabled:= Enabled;
  edVersion.Color:= EnabledColors[Enabled];

  edComment.Enabled:= Enabled;
  edComment.Color:= EnabledColors[Enabled];

  edDelay.Enabled:= Enabled;
  edDelay.Color:= EnabledColors[Enabled];


  edQuota.Enabled:= Enabled;
  edQuota.Color:= EnabledColors[Enabled];

  edLifeValue.Enabled:= Enabled;
  edLifeValue.Color:= EnabledColors[Enabled];

  edLifeVariance.Enabled:= Enabled;
  edLifeVariance.Color:= EnabledColors[Enabled];

  edInitalUpdateCount.Enabled:= Enabled;
  edInitalUpdateCount.Color:= EnabledColors[Enabled];

  edInitalUpdateInterval.Enabled:= Enabled;
  edInitalUpdateInterval.Color:= EnabledColors[Enabled];


  edLink                 .Enabled:= Enabled;
  edIdleWhileParentActive.Enabled:= Enabled;
end;
//------------------------------------------------------------------------------
procedure TFrmEffectBasic.SetEffect(const Value: TPHXParticleEffect);
begin
  FEffect := Value;

  if Assigned(Effect) then
  begin
    EnableControls(True);

    edName    .Text    := Effect.Name;
    edAuthor  .Text    := Effect.Author;
    edVersion .Text    := Effect.Version;
    edComment .Text    := Effect.Comment;

    edDelay       .Value:= Effect.Delay;
    edQuota       .Value:= Effect.Quota;
    edLifeValue   .Value:= Effect.LifeValue;
    edLifeVariance.Value:= Effect.LifeVariance;

    edInitalUpdateCount    .Value:= Effect.InitalUpdateCount;
    edInitalUpdateInterval .Value:= Effect.InitalUpdateInterval;

    edLink.Checked:= Effect.LinkToSystem;
    edIdleWhileParentActive.Checked:= Effect.IdleWhileParentActive;
  end else
  begin
    EnableControls(False);
  end;
end;

end.
