unit uEmissionPoints;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls,

  phxParticle,

  uEmissionPoints.Line,
  uEmissionPoints.Box,
  uEmissionPoints.Circle,
  uEmissionPoints.Bitmap;

type

//------------------------------------------------------------------------------
TFrmEmissionPoints = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    cbTemplateType: TComboBox;
    edCount: TJvSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    pnEmittorTemplate: TPanel;
    procedure cbTemplateTypeChange(Sender: TObject);
  private
    FrmEmittorTemplateLine  : TFrmEmittorTemplateLine;
    FrmEmittorTemplateCircle: TFrmEmittorTemplateCircle;
    FrmEmittorTemplateBox   : TFrmEmittorTemplateBox;
    FrmEmittorTemplateBitmap: TFrmEmittorTemplateBitmap;

    procedure ApplyTemplate(Effect: TPHXParticleEffect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(Effect: TPHXParticleEffect): Boolean;
  end;

var
  FrmEmissionPoints: TFrmEmissionPoints;

implementation

{$R *.dfm}

// TFrmEmissionPoints
//=============================================================================
constructor TFrmEmissionPoints.Create(AOwner: TComponent);
begin
  inherited;

  FrmEmittorTemplateLine  := TFrmEmittorTemplateLine.Create(Self);
  FrmEmittorTemplateCircle:= TFrmEmittorTemplateCircle.Create(Self);
  FrmEmittorTemplateBox   := TFrmEmittorTemplateBox.Create(Self);
  FrmEmittorTemplateBitmap:= TFrmEmittorTemplateBitmap.Create(Self);

end;

//------------------------------------------------------------------------------
destructor TFrmEmissionPoints.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TFrmEmissionPoints.Execute(Effect: TPHXParticleEffect): Boolean;
begin
  Result:= ShowModal = mrOk;

  if Result then
  begin
    ApplyTemplate(Effect);
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmEmissionPoints.ApplyTemplate(Effect: TPHXParticleEffect);
var Count : Integer;
begin
  Count:= Round(edCount.Value);

  case cbTemplateType.ItemIndex of
    1:
    begin
      FrmEmittorTemplateLine.Apply(Effect, Count);
    end;
    2:
    begin
      FrmEmittorTemplateCircle.Apply(Effect, Count);
    end;
    3:
    begin
      FrmEmittorTemplateBox.Apply(Effect, Count);
    end;
    4:
    begin
      FrmEmittorTemplateBitmap.Apply(Effect, Count);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEmissionPoints.cbTemplateTypeChange(Sender: TObject);
begin
  FrmEmittorTemplateLine.Parent:= nil;
  FrmEmittorTemplateCircle.Parent:= nil;
  FrmEmittorTemplateBox.Parent:= nil;
  FrmEmittorTemplateBitmap.Parent:= nil;

  case cbTemplateType.ItemIndex of
    1:
    begin
      FrmEmittorTemplateLine.Parent:= pnEmittorTemplate;
      FrmEmittorTemplateLine.Align:= alClient;
    end;
    2:
    begin
      FrmEmittorTemplateCircle.Parent:= pnEmittorTemplate;
      FrmEmittorTemplateCircle.Align:= alClient;
    end;
    3:
    begin
      FrmEmittorTemplateBox.Parent:= pnEmittorTemplate;
      FrmEmittorTemplateBox.Align:= alClient;
    end;
    4:
    begin
      FrmEmittorTemplateBitmap.Parent:= pnEmittorTemplate;
      FrmEmittorTemplateBitmap.Align:= alClient;
    end;
  end;
end;





end.
