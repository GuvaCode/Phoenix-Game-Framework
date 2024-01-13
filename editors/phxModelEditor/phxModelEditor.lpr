program phxModelEditor;

{$MODE Delphi}
uses
  Forms, Interfaces,
  uMain in 'Source\uMain.pas' {FrmMain},
  uActions in 'Source\uActions.pas' {ModActions: TDataModule},
  uModel.Properties in 'Source\Model\uModel.Properties.pas' {FrmModelProperties: TFrame},
  uModel.Groups in 'Source\Model\uModel.Groups.pas' {FrmModelGroups: TFrame},
  uModel.Joints in 'Source\Model\uModel.Joints.pas' {FrmModelJoints: TFrame},
  uModel.Tags in 'Source\Model\uModel.Tags.pas' {FrmModelTags: TFrame},
  uTextures in 'Source\uTextures.pas' {FrmTextures: TFrame},
  uImport in 'Source\Formats\uImport.pas',
  uImport3DS in 'Source\Formats\uImport3DS.pas',
  uImportMESH in 'Source\Formats\uImportMESH.pas',
  uImportMS3D in 'Source\Formats\uImportMS3D.pas',
  uImportOBJ in 'Source\Formats\uImportOBJ.pas',
  uImportX in 'Source\Formats\uImportX.pas',
  uModel.Material in 'Source\Model\uModel.Material.pas' {FrmModelMaterial: TFrame},
  uTextureMapper in 'Source\Tools\uTextureMapper.pas' {FrmTextureMapper},
  uViewer in 'Source\uViewer.pas' {FrmViewer: TFrame},
  uModel in 'Source\uModel.pas',
  uDialogCenter in 'Source\Dialogs\uDialogCenter.pas' {FrmDialogCenter},
  uDialogRotate in 'Source\Dialogs\uDialogRotate.pas' {FrmDialogRotate},
  uDialogScale in 'Source\Dialogs\uDialogScale.pas' {FrmDialogScale},
  uDialogTexture in 'Source\Dialogs\uDialogTexture.pas' {FrmDialogTexture};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'phxModelEditor';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmTextures, FrmTextures);
  Application.CreateForm(TFrmDialogTexture, FrmDialogTexture);
  Application.CreateForm(TFrmDialogCenter, FrmDialogCenter);
  Application.CreateForm(TFrmDialogRotate, FrmDialogRotate);
  Application.CreateForm(TFrmDialogScale, FrmDialogScale);
  Application.CreateForm(TFrmDialogTexture, FrmDialogTexture);
  Application.Run;
end.
