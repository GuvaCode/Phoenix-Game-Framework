program phxFontEditor;

uses
  Forms,
  uMain in 'Source\uMain.pas' {FrmMain},
  uActions in 'Source\uActions.pas' {ModActions: TDataModule},
  uCharacter.List in 'Source\Character\uCharacter.List.pas' {FrmFontCharacters: TFrame},
  uCharacter.Detail in 'Source\Character\uCharacter.Detail.pas' {FrmCharacter: TFrame},
  uFont.Properties in 'Source\Font\uFont.Properties.pas' {FrmFontProperties: TFrame},
  uGenerator.Dialog in 'Source\Generator\uGenerator.Dialog.pas' {FrmGenerator},
  uGenerator.Characters in 'Source\Generator\uGenerator.Characters.pas' {FrmGeneratorRange},
  uFont.Kernings in 'Source\Font\uFont.Kernings.pas' {FrmFontKernings: TFrame},
  uFont.Preview in 'Source\Font\uFont.Preview.pas' {FrmFontPreview: TFrame},
  uFont.Texture in 'Source\Font\uFont.Texture.pas' {FrmFontTexture: TFrame},
  Partitions in 'Source\Partitions.pas',
  GaussianBlur in 'Source\GaussianBlur.pas',
  phxFontEx in '..\_Library\phxFontEx.pas',
  phxGraphicsEx in '..\_Library\phxGraphicsEx.pas',
  uFont.New in 'Source\Font\uFont.New.pas' {FrmFontNew},
  uGenerator in 'Source\Generator\uGenerator.pas',
  uGenerator.Debug in 'Source\Generator\uGenerator.Debug.pas' {FrmGeneratorDebug},
  uCharacter.Search in 'Source\Character\uCharacter.Search.pas' {FrmCharacterSearch},
  uFont.WrapChars in 'Source\Font\uFont.WrapChars.pas' {FrmWrapCharacters};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'phxFontEditor';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmGenerator, FrmGenerator);
  Application.CreateForm(TFrmGeneratorRange, FrmGeneratorRange);
  Application.CreateForm(TFrmGeneratorDebug, FrmGeneratorDebug);
  Application.CreateForm(TFrmCharacterSearch, FrmCharacterSearch);
  Application.CreateForm(TFrmWrapCharacters, FrmWrapCharacters);
  Application.Run;
end.
