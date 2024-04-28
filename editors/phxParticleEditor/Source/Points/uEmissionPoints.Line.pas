unit uEmissionPoints.Line;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin,

  phxTypes,
  phxParticle;

type
  TFrmEmittorTemplateLine = class(TFrame)
    Label1: TLabel;
    edMinX: TJvSpinEdit;
    edMinY: TJvSpinEdit;
    edMaxX: TJvSpinEdit;
    edMaxY: TJvSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    function GetMaxX: Single;
    function GetMinX: Single;
    function GetMinY: Single;
    function GetMaxY: Single;
  public

    procedure Apply(Effect: TPHXParticleEffect; Count: Integer);

    property MinX: Single read GetMinX;
    property MinY: Single read GetMinY;
    property MaxX: Single read GetMaxX;
    property MaxY: Single read GetMaxY;
  end;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TFrmEmittorTemplateLine.Apply(Effect: TPHXParticleEffect; Count: Integer);
begin
  Effect.Emittor.CreateLine(Count, Vector3f(MinX,MinY,0), Vector3f(MaxX, MinY, 0));
end;

//------------------------------------------------------------------------------
function TFrmEmittorTemplateLine.GetMaxX: Single;
begin
  Result:= edMaxX.Value;
end;

//------------------------------------------------------------------------------
function TFrmEmittorTemplateLine.GetMaxY: Single;
begin
  Result:= edMaxY.Value;
end;

//------------------------------------------------------------------------------
function TFrmEmittorTemplateLine.GetMinX: Single;
begin
  Result:= edMinX.Value;
end;

//------------------------------------------------------------------------------
function TFrmEmittorTemplateLine.GetMinY: Single;
begin
  Result:= edMinY.Value;
end;

end.
