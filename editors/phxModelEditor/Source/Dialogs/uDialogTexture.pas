unit uDialogTexture;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,

  phxModel,
  phxGraphics;

type
  TFrmDialogTexture = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    edTextureKind: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    edTextureName: TEdit;
    OpenDialog1: TOpenDialog;
    procedure edTextureNameButtonClick(Sender: TObject);
  private
    function GetTextureKind: TPHXMeshTextureKind;
    function GetTextureName: String;
    procedure SetTextureKind(const Value: TPHXMeshTextureKind);
    procedure SetTextureName(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;

    function Execute: Boolean;

    property TextureName: String read GetTextureName write SetTextureName;
    property TextureKind: TPHXMeshTextureKind read GetTextureKind write SetTextureKind;
  end;

var
  FrmDialogTexture: TFrmDialogTexture;

implementation

{$R *.dfm}

uses uActions;

// TFrmDialogTexture
//==============================================================================
constructor TFrmDialogTexture.Create(AOwner: TComponent);
begin
  inherited;

end;

//------------------------------------------------------------------------------
function TFrmDialogTexture.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmDialogTexture.edTextureNameButtonClick(Sender: TObject);
var Path: String;
begin
  Path:= ExtractFilePath(ModActions.Document.FileName);
  OpenDialog1.Filter:= GraphicFormats.Filter;
  OpenDialog1.InitialDir:= Path;
  OpenDialog1.FileName:= edTextureName.Text;

  if OpenDialog1.Execute then
  begin
    edTextureName.Text:= ExtractRelativePath(Path, OpenDialog1.FileName);
  end;
end;


//------------------------------------------------------------------------------
function TFrmDialogTexture.GetTextureName: String;
begin
  Result:= edTextureName.Text;
end;

//------------------------------------------------------------------------------
function TFrmDialogTexture.GetTextureKind: TPHXMeshTextureKind;
begin
  Result:= TPHXMeshTextureKind(edTextureKind.ItemIndex);
end;

//------------------------------------------------------------------------------
procedure TFrmDialogTexture.SetTextureName(const Value: String);
begin
   edTextureName.Text:= Value;
end;

//------------------------------------------------------------------------------
procedure TFrmDialogTexture.SetTextureKind(const Value: TPHXMeshTextureKind);
begin
  edTextureKind.ItemIndex:= Ord(Value);
end;


end.
