unit uEmissionPoints.Box;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin,

  phxTypes,
  phxParticle;

type
  TFrmEmittorTemplateBox = class(TFrame)
    cbConstrainToSurface: TCheckBox;
    Label1: TLabel;
    edMinX: TJvSpinEdit;
    edMinY: TJvSpinEdit;
    edMaxX: TJvSpinEdit;
    edMaxY: TJvSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    function GetConstrainToSurface: Boolean;
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

   Property ConstrainToSurface: Boolean read GetConstrainToSurface;
  end;

implementation

{$R *.dfm}

{ TFrmEmittorTemplateBox }

procedure TFrmEmittorTemplateBox.Apply(Effect: TPHXParticleEffect; Count: Integer);
begin
  Effect.Emittor.CreateRectangle(Count, ConstrainToSurface, Rectf(MinX, MinY, MaxX, MaxY));
end;

function TFrmEmittorTemplateBox.GetConstrainToSurface: Boolean;
begin
  Result:= cbConstrainToSurface.Checked;

end;


function TFrmEmittorTemplateBox.GetMaxX: Single;
begin
  Result:= edMaxX.Value;
end;

function TFrmEmittorTemplateBox.GetMaxY: Single;
begin
  Result:= edMaxY.Value;
end;

function TFrmEmittorTemplateBox.GetMinX: Single;
begin
  Result:= edMinX.Value;
end;

function TFrmEmittorTemplateBox.GetMinY: Single;
begin
  Result:= edMinY.Value;
end;


end.
