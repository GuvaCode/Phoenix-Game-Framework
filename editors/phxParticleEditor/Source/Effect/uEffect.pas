unit uEffect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  phxParticle,

  uEffect.Basic,
  uEffect.Apperance,
  uEffect.Physics,
  uEffect.Emittor,
  uEffect.Affector;

type

//------------------------------------------------------------------------------
TFrmEffect = class(TFrame)
    PageControl1: TPageControl;
    TabBasic: TTabSheet;
    TabApperance: TTabSheet;
    TabPhysics: TTabSheet;
    TabEmittor: TTabSheet;
    TabAffector: TTabSheet;
  private
    FEffect: TPHXParticleEffect;

    FrmEffectBasic    : TFrmEffectBasic;
    FrmEffectApperance: TFrmEffectApperance;
    FrmEffectPhysics  : TFrmEffectPhysics;
    FrmEffectEmittor  : TFrmEffectEmittor;
    FrmEffectAffector : TFrmEffectAffector;


    procedure SetEffect(const Value: TPHXParticleEffect);
  public
    constructor Create(AOwner: TComponent); override;

    property Effect: TPHXParticleEffect read FEffect write SetEffect;
  end;

implementation

{$R *.dfm}

// TFrmEffect
//==============================================================================
constructor TFrmEffect.Create(AOwner: TComponent);
begin
  inherited;

  FrmEffectBasic:= TFrmEffectBasic.Create(Self);
  FrmEffectBasic.Parent:= TabBasic;
  FrmEffectBasic.Align := alClient;

  FrmEffectApperance:= TFrmEffectApperance.Create(Self);
  FrmEffectApperance.Align := alClient;
  FrmEffectApperance.Parent:= TabApperance;

  FrmEffectPhysics:= TFrmEffectPhysics.Create(Self);
  FrmEffectPhysics.Parent:= TabPhysics;
  FrmEffectPhysics.Align := alClient;

  FrmEffectEmittor:= TFrmEffectEmittor.Create(Self);
  FrmEffectEmittor.Parent:= TabEmittor;
  FrmEffectEmittor.Align := alClient;

  FrmEffectAffector:= TFrmEffectAffector.Create(Self);
  FrmEffectAffector.Parent:= TabAffector;
  FrmEffectAffector.Align := alClient;

  PageControl1.ActivePageIndex:= 0;
end;

//------------------------------------------------------------------------------
procedure TFrmEffect.SetEffect(const Value: TPHXParticleEffect);
begin
  FEffect := Value;

  FrmEffectBasic    .Effect:= FEffect;
  FrmEffectApperance.Effect:= FEffect;
  FrmEffectPhysics  .Effect:= FEffect;
  FrmEffectEmittor  .Effect:= FEffect;
  FrmEffectAffector .Effect:= FEffect;
end;



end.
