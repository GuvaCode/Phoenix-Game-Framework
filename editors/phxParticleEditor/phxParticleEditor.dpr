program phxParticleEditor;

uses
  Vcl.Forms,
  uMain in 'Source\uMain.pas' {FrmMain},
  uActions in 'Source\uActions.pas' {ModActions: TDataModule},
  uEditor in 'Source\uEditor.pas',
  uEffect.Basic in 'Source\Effect\uEffect.Basic.pas' {FrmEffectBasic: TFrame},
  uEffect.Apperance in 'Source\Effect\uEffect.Apperance.pas' {FrmEffectApperance: TFrame},
  uEffect.Physics in 'Source\Effect\uEffect.Physics.pas' {FrmEffectPhysics: TFrame},
  uEffect.Emittor in 'Source\Effect\uEffect.Emittor.pas' {FrmEffectEmittor: TFrame},
  uPreview in 'Source\uPreview.pas' {FrmEffectPreview: TFrame},
  uDialog.Texture in 'Source\Dialogs\uDialog.Texture.pas' {TextureDialog},
  uEffect.Affector in 'Source\Effect\uEffect.Affector.pas' {FrmEffectAffector: TFrame},
  uEffect in 'Source\Effect\uEffect.pas' {FrmEffect: TFrame},
  uAffector.Attractor in 'Source\Affector\uAffector.Attractor.pas' {FrmAffectorAttractor: TFrame},
  uSystems in 'Source\uSystems.pas' {FrmSystems: TFrame},
  uGraph1f in 'Source\Graphs\uGraph1f.pas',
  uDebugParticles in 'Source\Debug\uDebugParticles.pas' {FrmDebugParticles},
  uDebugGraphs in 'Source\Debug\uDebugGraphs.pas' {FrmDebugGraphs},
  uGraphRGB in 'Source\Graphs\uGraphRGB.pas' {FrmGraphRGB: TFrame},
  uGraphs in 'Source\Graphs\uGraphs.pas',
  uEmissionPoints in 'Source\Points\uEmissionPoints.pas' {FrmEmissionPoints},
  uEmissionPoints.Bitmap in 'Source\Points\uEmissionPoints.Bitmap.pas' {FrmEmittorTemplateBitmap: TFrame},
  uEmissionPoints.Box in 'Source\Points\uEmissionPoints.Box.pas' {FrmEmittorTemplateBox: TFrame},
  uEmissionPoints.Circle in 'Source\Points\uEmissionPoints.Circle.pas' {FrmEmittorTemplateCircle: TFrame},
  uEmissionPoints.Line in 'Source\Points\uEmissionPoints.Line.pas' {FrmEmittorTemplateLine: TFrame},
  uDialog.Pattern in 'Source\Dialogs\uDialog.Pattern.pas' {PatternDialog},
  uAffector.Default in 'Source\Affector\uAffector.Default.pas' {FrmAffectorDefault: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmDebugParticles, FrmDebugParticles);
  Application.CreateForm(TFrmDebugGraphs, FrmDebugGraphs);
  Application.CreateForm(TFrmEmissionPoints, FrmEmissionPoints);
  Application.CreateForm(TPatternDialog, PatternDialog);
  Application.Run;
end.
