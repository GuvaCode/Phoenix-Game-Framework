unit uModel.Material;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,

  uActions,

  phxTypes,
  phxModel, ImgList, ActnList, ToolWin;

type

//------------------------------------------------------------------------------
TFrmModelMaterial = class(TFrame)
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label9: TLabel;
    edName: TEdit;
    GroupBox1: TGroupBox;
    edDiffuse: TColorBox;
    lwTextures: TListView;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    actTextureAdd: TAction;
    actTextureDelete: TAction;
    actTextureEdit: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure edNameChange(Sender: TObject);
    procedure edDiffuseChange(Sender: TObject);
    procedure actTextureUpdate(Sender: TObject);
    procedure actTextureAddExecute(Sender: TObject);
    procedure actTextureDeleteExecute(Sender: TObject);
    procedure actTextureEditExecute(Sender: TObject);
    procedure lwTexturesDblClick(Sender: TObject);
  private
    FMesh: TPHXMesh;

    procedure lwTexturesUpdate(Textures: TPHXMeshTextures);

    procedure EnableControls(Enabled: Boolean);

    procedure SetMesh(const Value: TPHXMesh);
  public
    constructor Create(AOwner: TComponent); override;

    Property Mesh: TPHXMesh read FMesh write SetMesh;
  end;

implementation

{$R *.dfm}

uses uDialogTexture;

//------------------------------------------------------------------------------
function KindToStr(const Kind: TPHXMeshTextureKind): String;
begin
  Result:= '';
  case Kind of
    tkCustom      : Result:= 'Custom';
    tkDiffuse     : Result:= 'Diffuse';
    tkNormal      : Result:= 'Normal';
    tkTeam        : Result:= 'Team';
    tkAlphamap    : Result:= 'Alphamap';
    tkDisplacement: Result:= 'Displacement';
  end;
end;


// TFrmModelMaterial
//==============================================================================
constructor TFrmModelMaterial.Create(AOwner: TComponent);
begin
  inherited;

  SetMesh(nil);
end;

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.EnableControls(Enabled: Boolean);
const EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName.Enabled:= Enabled;
  edName.Color  := EnabledColors[Enabled];

  edDiffuse.Enabled:= Enabled;
  edDiffuse.Color  := EnabledColors[Enabled];

  lwTextures.Enabled:= Enabled;
  lwTextures.Color  := EnabledColors[Enabled];

end;


//------------------------------------------------------------------------------
procedure TFrmModelMaterial.lwTexturesUpdate(Textures: TPHXMeshTextures);
var Index  : Integer;
var Texture: TPHXMeshTexture;
var Item   : TListItem;
begin
  lwTextures.Items.BeginUpdate;
  lwTextures.Items.Clear;

  for Index := 0 to Textures.Count-1 do
  begin
    Texture:= Textures[Index];

    Item:= lwTextures.Items.Add;
    Item.Caption:= String(Texture.Name);
    Item.SubItems.Add( KindToStr(Texture.Kind) );
  end;

  lwTextures.Items.EndUpdate;

end;

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.lwTexturesDblClick(Sender: TObject);
begin
  actTextureEdit.Execute;
end;



{$REGION 'Edit Events'}


//------------------------------------------------------------------------------
procedure TFrmModelMaterial.edNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edName.Text;

  if Mesh.Material.Name <> Value then
  begin
    Mesh.Material.Name:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.edDiffuseChange(Sender: TObject);
var Value: TColor4f;
begin
  Value:= TColor4f.Create(edDiffuse.Selected, 1.0);

  if Mesh.Material.Diffuse <> Value then
  begin
    Mesh.Material.Diffuse:=  Value;

    ModActions.Document.Changed;
  end;
end;

{$ENDREGION}

{$REGION 'Texture Actions'}


//------------------------------------------------------------------------------
procedure TFrmModelMaterial.actTextureUpdate(Sender: TObject);
begin
  actTextureAdd   .Enabled:= Assigned(Mesh);
  actTextureDelete.Enabled:= Assigned(Mesh) and (lwTextures.ItemIndex <> -1);
  actTextureEdit  .Enabled:= Assigned(Mesh) and (lwTextures.ItemIndex <> -1);
end;

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.actTextureAddExecute(Sender: TObject);
begin
  FrmDialogTexture.TextureName:= '';
  FrmDialogTexture.TextureKind:= tkCustom;

  if FrmDialogTexture.Execute then
  begin
    Mesh.Material.Textures.Add(FrmDialogTexture.TextureName, FrmDialogTexture.TextureKind);

    lwTexturesUpdate(Mesh.Material.Textures);

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.actTextureDeleteExecute(Sender: TObject);
var Index: Integer;
begin
  Index:= lwTextures.ItemIndex;

  if MessageDlg('Delete texture?', mtConfirmation, mbYesNo, 0) = mrOk then
  begin
    Mesh.Material.Textures.Delete(Index);

    lwTexturesUpdate(Mesh.Material.Textures);

    ModActions.LoadModelTextures;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.actTextureEditExecute(Sender: TObject);
var Index: Integer;
begin
  Index:= lwTextures.ItemIndex;

  FrmDialogTexture.TextureName:= String(Mesh.Material.Textures.List^[Index].Name);
  FrmDialogTexture.TextureKind:=        Mesh.Material.Textures.List^[Index].Kind;

  if FrmDialogTexture.Execute then
  begin
    Mesh.Material.Textures.List^[Index].Name:= ShortString(FrmDialogTexture.TextureName);
    Mesh.Material.Textures.List^[Index].Kind:=             FrmDialogTexture.TextureKind;

    lwTexturesUpdate(Mesh.Material.Textures);

    ModActions.LoadModelTextures;

    ModActions.Document.Changed;
  end;
end;


{$ENDREGION}

//------------------------------------------------------------------------------
procedure TFrmModelMaterial.SetMesh(const Value: TPHXMesh);
begin
  FMesh := Value;

  if Assigned(FMesh) then
  begin
    EnableControls(True);

    edName .Text    :=              Mesh.Material.Name;
    edDiffuse.Selected:= ColorToRGB(Mesh.Material.Diffuse);

    lwTexturesUpdate(Mesh.Material.Textures);
  end else
  begin
    lwTextures.Items.Clear;

    EnableControls(False);
  end;
end;


end.
