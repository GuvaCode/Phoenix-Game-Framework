unit uDialogCenter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin,

  phxTypes;

type
  TFrmDialogCenter = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    GroupBox1: TGroupBox;
    edCenterZ: TCheckBox;
    edCenterY: TCheckBox;
    edCenterX: TCheckBox;
  private
    function GetCenterX: Boolean;
    function GetCenterY: Boolean;
    function GetCenterZ: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    Property CenterX: Boolean read GetCenterX;
    Property CenterY: Boolean read GetCenterY;
    Property CenterZ: Boolean read GetCenterZ;
  end;

var
  FrmDialogCenter: TFrmDialogCenter;

implementation

{$R *.dfm}

// TFrmDialogScale
//==============================================================================
constructor TFrmDialogCenter.Create(AOwner: TComponent);
begin
  inherited;
end;


function TFrmDialogCenter.GetCenterX: Boolean;
begin
  Result:= edCenterX.Checked;
end;

function TFrmDialogCenter.GetCenterY: Boolean;
begin
  Result:= edCenterY.Checked;
end;

function TFrmDialogCenter.GetCenterZ: Boolean;
begin
  Result:= edCenterZ.Checked;

end;

end.
