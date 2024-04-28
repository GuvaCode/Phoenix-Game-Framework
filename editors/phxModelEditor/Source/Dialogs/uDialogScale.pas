unit uDialogScale;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin,

  phxTypes;

type
  TFrmDialogScale = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edScaleX: TSpinEdit;
    Label2: TLabel;
    edScaleY: TSpinEdit;
    Label3: TLabel;
    edScaleZ: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
  private
    function GetScale: TVector3f;
    procedure SetScale(const Value: TVector3f);
  public
    constructor Create(AOwner: TComponent); override;

    Property Scale: TVector3f read GetScale write SetScale;
  end;

var
  FrmDialogScale: TFrmDialogScale;

implementation

{$R *.dfm}

// TFrmDialogScale
//==============================================================================
constructor TFrmDialogScale.Create(AOwner: TComponent);
begin
  inherited;

  edScaleX.Value:= 100;
  edScaleY.Value:= 100;
  edScaleZ.Value:= 100;
end;

//------------------------------------------------------------------------------
function TFrmDialogScale.GetScale: TVector3f;
begin
  Result.X:= edScaleX.Value / 100;
  Result.Y:= edScaleY.Value / 100;
  Result.Z:= edScaleZ.Value / 100;
end;

//------------------------------------------------------------------------------
procedure TFrmDialogScale.SetScale(const Value: TVector3f);
begin
  edScaleX.Value:= Value.X * 100;
  edScaleY.Value:= Value.Y * 100;
  edScaleZ.Value:= Value.Z * 100;
end;

end.
