program phxSkinEditor;

uses
  Forms,
  phxSkinEx in 'Source\phxSkinEx.pas',
  uActions in 'Source\uActions.pas',
  uMain in 'Source\uMain.pas' {FrmMain},
  uSettings in 'Source\uSettings.pas',
  uTools in 'Source\uTools.pas' {ModTools},
  Partitions in 'Source\Tools\Partitions.pas',
  uPacker in 'Source\Tools\uPacker.pas',
  uTexture in 'Source\Frames\uTexture.pas',
  uElementName in 'Source\Element\uElementName.pas' {FrmElementName},
  uElementPreview in 'Source\Element\uElementPreview.pas',
  uElement in 'Source\Element\uElement.pas',
  uNewDialog in 'Source\Dialogs\uNewDialog.pas',
  phxGraphicsEx in '..\_Library\phxGraphicsEx.pas',
  phxEditor in '..\_Library\phxEditor.pas',
  uSkin.Properties in 'Source\Skin\uSkin.Properties.pas' {FrmSkinProperties: TFrame},
  uSkin.Elements in 'Source\Skin\uSkin.Elements.pas' {FrmSkinElements: TFrame},
  uDialog.New in 'Source\Dialogs\uDialog.New.pas' {NewDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Phoenix Skin Editor';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
