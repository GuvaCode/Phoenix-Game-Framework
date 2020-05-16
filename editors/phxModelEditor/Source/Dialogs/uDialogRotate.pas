unit uDialogRotate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin,

  phxTypes;

type
  TFrmDialogRotate = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edRotationX: TJvSpinEdit;
    Label2: TLabel;
    edRotationY: TJvSpinEdit;
    Label3: TLabel;
    edRotationZ: TJvSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
  private
    function GetRotation: TVector3f;
    procedure SetRotation(const Value: TVector3f);
  public
    constructor Create(AOwner: TComponent); override;

    Property Rotation: TVector3f read GetRotation write SetRotation;
  end;

var
  FrmDialogRotate: TFrmDialogRotate;

implementation

{$R *.dfm}

// TFrmDialogScale
//==============================================================================
constructor TFrmDialogRotate.Create(AOwner: TComponent);
begin
  inherited;

  edRotationX.Value:= 0;
  edRotationY.Value:= 0;
  edRotationZ.Value:= 0;
end;

//------------------------------------------------------------------------------
function TFrmDialogRotate.GetRotation: TVector3f;
begin
  Result.X:= edRotationX.Value ;
  Result.Y:= edRotationY.Value ;
  Result.Z:= edRotationZ.Value ;
end;

//------------------------------------------------------------------------------
procedure TFrmDialogRotate.SetRotation(const Value: TVector3f);
begin
  edRotationX.Value:= Value.X ;
  edRotationY.Value:= Value.Y ;
  edRotationZ.Value:= Value.Z ;
end;

end.
