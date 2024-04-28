unit uEmittorTemplateBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin;

type
  TFrmEmittorTemplateBox = class(TFrame)
    btnCreate: TButton;
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
    { Private declarations }
  public
    { Public declarations }
  published
    property MinX: Single read GetMinX;
    property MinY: Single read GetMinY;
    property MaxX: Single read GetMaxX;
    property MaxY: Single read GetMaxY;

     Property ConstrainToSurface: Boolean read GetConstrainToSurface;
  end;

implementation

{$R *.dfm}

{ TFrmEmittorTemplateBox }

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
