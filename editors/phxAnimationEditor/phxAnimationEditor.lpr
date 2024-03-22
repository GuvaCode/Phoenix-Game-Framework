program phxAnimationEditor;

{$MODE Delphi}

uses
  Forms, Interfaces, PGF, 
  uMain in 'Source\uMain.pas' {FrmMain},
  uActions in 'Source\uActions.pas' {ModActions: TDataModule},
  phxGraphicsEx in '..\_Library\phxGraphicsEx.pas',
  phxImageEx in '..\_Library\phxImageEx.pas',
  uAnimation.Frame in 'Source\Animation\uAnimation.Frame.pas' {FrmAnimationFrame: TFrame},
  uImage.Patterns in 'Source\Image\uImage.Patterns.pas' {FrmPatterns: TFrame},
  uAnimation.Frames in 'Source\Animation\uAnimation.Frames.pas' {FrmAnimationFrames: TFrame},
  uAnimation.Properties in 'Source\Animation\uAnimation.Properties.pas' {FrmAnimationProperties: TFrame},
  uAnimation.Preview in 'Source\Animation\uAnimation.Preview.pas' {FrmAnimationPreview: TFrame},
  uImage.List in 'Source\Image\uImage.List.pas' {FrmImageList: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
