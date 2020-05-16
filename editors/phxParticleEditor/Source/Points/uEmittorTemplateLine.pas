unit uEmittorTemplateLine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin;

type
  TFrmEmittorTemplateLine = class(TFrame)
    btnCreate: TButton;
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
    { Private declarations }
  public
    { Public declarations }
  published
    property MinX: Single read GetMinX;
    property MinY: Single read GetMinY;
    property MaxX: Single read GetMaxX;
    property MaxY: Single read GetMaxY;
  end;

implementation

{$R *.dfm}

{ TFrmEmittorTemplateLine }


function TFrmEmittorTemplateLine.GetMaxX: Single;
begin
  Result:= edMaxX.Value;
end;

function TFrmEmittorTemplateLine.GetMaxY: Single;
begin
  Result:= edMaxY.Value;
end;

function TFrmEmittorTemplateLine.GetMinX: Single;
begin
  Result:= edMinX.Value;
end;

function TFrmEmittorTemplateLine.GetMinY: Single;
begin
  Result:= edMinY.Value;
end;

end.
