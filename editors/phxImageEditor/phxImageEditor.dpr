program phxImageEditor;

uses
  Forms,
  phxGraphicsEx in '..\_Library\phxGraphicsEx.pas',
  uMain in 'Source\uMain.pas' {FrmMain},
  uActions in 'Source\uActions.pas' {ModActions: TDataModule},
  uImage.Properties in 'Source\Image\uImage.Properties.pas' {FrmImageProperties: TFrame},
  uPattern.List in 'Source\Pattern\uPattern.List.pas' {FrmPatternList: TFrame},
  uImage.New.Empty in 'Source\Image\uImage.New.Empty.pas' {FrmImageEmpty},
  phxEditor in '..\_Library\phxEditor.pas',
  uEditor in 'Source\uEditor.pas',
  uImage.New in 'Source\Image\uImage.New.pas' {NewDialog},
  uImage.New.Packer in 'Source\Image\uImage.New.Packer.pas',
  Partitions in 'Source\Tools\Partitions.pas',
  uTag.List in 'Source\Tag\uTag.List.pas' {FrmTagList: TFrame},
  uTag.Actions in 'Source\Tag\uTag.Actions.pas' {ModTags: TDataModule},
  uTools in 'Source\uTools.pas' {ModTools: TDataModule},
  phxImagePanel in '..\_Library\phxImagePanel.pas',
  phxImageEx in '..\_Library\phxImageEx.pas',
  uTile.Wizard in 'Source\Tile\uTile.Wizard.pas',
  uSettings in 'Source\uSettings.pas',
  uPattern.Properties in 'Source\Pattern\uPattern.Properties.pas' {FrmPatternProperties: TFrame},
  uImage.Resize in 'Source\Image\uImage.Resize.pas' {FrmImageResize},
  uTile.Editor in 'Source\Tile\uTile.Editor.pas' {FrmTileEditor},
  uTile.Select in 'Source\Tile\uTile.Select.pas' {FrmTileSelect},
  uShape in 'Source\Shape\uShape.pas' {FrmShapeEditor},
  uShapeCircle in 'Source\Shape\uShapeCircle.pas' {FrmShapeCircle: TFrame},
  uShapeLine in 'Source\Shape\uShapeLine.pas' {FrmShapeLine: TFrame},
  uShapePoint in 'Source\Shape\uShapePoint.pas' {FrmShapePoint: TFrame},
  uShapePolygon in 'Source\Shape\uShapePolygon.pas' {FrmShapePolygon: TFrame},
  uShapeBox in 'Source\Shape\uShapeBox.pas' {FrmShapeBox: TFrame},
  uShapeProperties in 'Source\Shape\uShapeProperties.pas' {FrmShape: TFrame},
  uShapeTools in 'Source\Shape\uShapeTools.pas',
  uPattern.Search in 'Source\Pattern\uPattern.Search.pas' {FrmPatternSearch},
  uPattern.Sort in 'Source\Pattern\uPattern.Sort.pas' {FrmPatternSort},
  uTag.Properties in 'Source\Tag\uTag.Properties.pas' {FrmTagProperties: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'phxImageEditor';
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmTileEditor, FrmTileEditor);
  Application.CreateForm(TFrmTileSelect, FrmTileSelect);
  Application.CreateForm(TFrmShapeEditor, FrmShapeEditor);
  Application.CreateForm(TFrmPatternSearch, FrmPatternSearch);
  Application.CreateForm(TFrmPatternSort, FrmPatternSort);
  Application.Run;
end.
