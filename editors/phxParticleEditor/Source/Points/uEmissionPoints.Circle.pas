unit uEmissionPoints.Circle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin,

  phxParticle;

type
  TFrmEmittorTemplateCircle = class(TFrame)
    Label1: TLabel;
    edRadius: TJvSpinEdit;
    cbConstrainToSurface: TCheckBox;
  private
    function GetRadius: Single;
    function GetConstrainToSurface: Boolean;
  public
    procedure Apply(Effect: TPHXParticleEffect; Count: Integer);

    Property Radius: Single read GetRadius;
    Property ConstrainToSurface: Boolean read GetConstrainToSurface;
  end;

implementation

{$R *.dfm}

{ TFrmEmittorTemplateCircle }

procedure TFrmEmittorTemplateCircle.Apply(Effect: TPHXParticleEffect; Count: Integer);
begin
  Effect.Emittor.CreateCircle(Count, ConstrainToSurface, Radius);
end;

function TFrmEmittorTemplateCircle.GetConstrainToSurface: Boolean;
begin
  Result:= cbConstrainToSurface.Checked;
end;

function TFrmEmittorTemplateCircle.GetRadius: Single;
begin
  result:= edRadius.Value;
end;

end.
