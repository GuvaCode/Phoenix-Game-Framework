unit uAffector.Default;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  uEffect.Affector,

  phxParticle,
  phxParticleAffectors, JvExControls, JvInspector;


type

//------------------------------------------------------------------------------
 TFrmAffectorDefault = class(TFrame, IAffectorEditor)
    JvInspector1: TJvInspector;
  private
    FAffector: TPHXParticleAffector;

    function GetTitle: String;

    procedure SetAffector(Value: TPHXParticleAffector); overload;
  public
    constructor Create(AOwner: TComponent); override;

    property Affector: TPHXParticleAffector read FAffector write SetAffector;
  end;

implementation

{$R *.dfm}

// TFrmAffectorDefault
//==============================================================================
constructor TFrmAffectorDefault.Create(AOwner: TComponent);
begin
  inherited;

end;

//------------------------------------------------------------------------------
function TFrmAffectorDefault.GetTitle: String;
begin
  if Assigned(FAffector) then
  begin
    Result:= FAffector.Name;
  end else
  begin
    Result:= '';
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmAffectorDefault.SetAffector(Value: TPHXParticleAffector);
begin
  FAffector:= Value;

  JvInspector1.InspectObject:= Value;
end;

initialization
  RegisterAffectorEditor(TPHXParticleAffector, TFrmAffectorDefault);
end.
