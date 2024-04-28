program phxGuiEditor;

uses
  Vcl.Forms,
  uMain in 'Source\uMain.pas' {FrmMain},
  uProject in 'Source\Project\uProject.pas',
  uPalette in 'Source\Frames\uPalette.pas' {FrmPalette: TFrame},
  uActions in 'Source\uActions.pas' {ModActions: TDataModule},
  uDesigner in 'Source\Frames\uDesigner.pas' {FrmDesigner: TFrame},
  uInspector in 'Source\Frames\uInspector.pas' {FrmInspector: TFrame},
  uOutput in 'Source\uOutput.pas' {FrmOutput},
  uControls in 'Source\Project\uControls.pas',
  uStructure in 'Source\Frames\uStructure.pas' {FrmStructure: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmOutput, FrmOutput);
  Application.Run;
end.
