unit uEmittorTemplateCircle;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin;

type
  TFrmEmittorTemplateCircle = class(TFrame)
    Label1: TLabel;
    edRadius: TJvSpinEdit;
    btnCreate: TButton;
    cbConstrainToSurface: TCheckBox;
  private
    function GetRadius: Single;
    function GetConstrainToSurface: Boolean;
    { Private declarations }
  public
    { Public declarations }
    Property Radius: Single read GetRadius;
    Property ConstrainToSurface: Boolean read GetConstrainToSurface;
  end;

implementation

{$R *.dfm}

{ TFrmEmittorTemplateCircle }

function TFrmEmittorTemplateCircle.GetConstrainToSurface: Boolean;
begin
  Result:= cbConstrainToSurface.Checked;
end;

function TFrmEmittorTemplateCircle.GetRadius: Single;
begin
  result:= edRadius.Value;
end;

end.
