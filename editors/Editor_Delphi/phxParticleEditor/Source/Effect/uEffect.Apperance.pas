unit uEffect.Apperance;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls,

  JvExStdCtrls, JvEdit, JvExMask, JvSpin, JvToolEdit,

  phxTypes,
  phxParticle,

  phxTextureDialog;

type

//------------------------------------------------------------------------------
TFrmEffectApperance = class(TFrame)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label15: TLabel;
    edPatternSize: TComboBox;
    edPatternVariance: TJvSpinEdit;
    edTexture: TJvComboEdit;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edShape: TComboBox;
    edSizeMin: TJvSpinEdit;
    edSizeMax: TJvSpinEdit;
    edBlending: TComboBox;
    edGrowthMax: TJvSpinEdit;
    edGrowthMin: TJvSpinEdit;
    GroupBox4: TGroupBox;
    Label8: TLabel;
    Label13: TLabel;
    edGrayscale: TCheckBox;
    edColorMin: TColorBox;
    edColorMax: TColorBox;
    edPatternIndex: TJvComboEdit;
    procedure edTextureChange(Sender: TObject);
    procedure edTextureClick(Sender: TObject);
    procedure edColorMinChange(Sender: TObject);
    procedure edColorMaxChange(Sender: TObject);
    procedure edPatternSizeChange(Sender: TObject);
    procedure edPatternIndexChange(Sender: TObject);
    procedure edPatternVarianceChange(Sender: TObject);
    procedure edSizeMaxChange(Sender: TObject);
    procedure edSizeMinChange(Sender: TObject);
    procedure edPatternIndexButtonClick(Sender: TObject);
    procedure edBlendingChange(Sender: TObject);
    procedure edShapeChange(Sender: TObject);
  private
    FEffect: TPHXParticleEffect;

    procedure Changed;

    procedure EnableControls(Enabled: Boolean);

    procedure SetEffect(const Value: TPHXParticleEffect);
  public
    constructor Create(AOwner: TComponent); override;

    property Effect: TPHXParticleEffect read FEffect write SetEffect;
  end;

implementation

{$R *.dfm}
uses uActions, uDialog.Texture, uDialog.Pattern;

// TFrmEffectApperance
//==============================================================================
constructor TFrmEffectApperance.Create(AOwner: TComponent);
begin
  inherited;

  SetEffect(nil);
end;


//------------------------------------------------------------------------------
procedure TFrmEffectApperance.Changed;
begin
  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edTextureChange(Sender: TObject);
var Value: String;
begin
  Value:= edTexture.Text;

  if Effect.TextureName <> Value then
  begin
    Effect.TextureName:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edTextureClick(Sender: TObject);
var Dialog: TPHXTextureDialog;
begin
  Dialog:= TPHXTextureDialog.Create(Application);
  try
    Dialog.Device  := ModActions.Device;
    Dialog.Textures:= ModActions.Textures;
    Dialog.texture := Effect.Texture;

    if Dialog.Execute then
    begin
      Effect.Texture:= Dialog.Texture;

      edTexture.Text:= Effect.TextureName;

      Changed;
    end;

  finally
    Dialog.free;
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edPatternSizeChange(Sender: TObject);
var Value: TPHXParticlePatternSize;
begin
  Value:= TPHXParticlePatternSize(edPatternSize.ItemIndex);

  if Effect.PatternSize <> Value then
  begin
    Effect.PatternSize:= Value;

    Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edPatternIndexChange(Sender: TObject);
var Value: Word;
begin
  Value:= StrToIntDef(edPatternIndex.Text, Effect.PatternIndex);

  if Effect.PatternIndex <> Value then
  begin
    Effect.PatternIndex:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edPatternIndexButtonClick(Sender: TObject);
begin
  if Assigned(Effect.Texture) then
  begin
    PatternDialog.Texture:= Effect.Texture;

    PatternDialog.PatternSize := Effect.PatternSize;
    PatternDialog.PatternIndex:= Effect.PatternIndex;

    if PatternDialog.Execute then
    begin
      Effect.PatternIndex:= PatternDialog.PatternIndex;

      edPatternIndex.Text:= IntToStr(Effect.PatternIndex);

      Changed;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edPatternVarianceChange(Sender: TObject);
var Value: Word;
begin
  Value:= StrToIntDef(edPatternVariance.Text, Effect.PatternVariance);

  if Effect.PatternVariance <> Value then
  begin
    Effect.PatternVariance:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edShapeChange(Sender: TObject);
var Value: TPHXParticleShape;
begin
  Value:= TPHXParticleShape(edShape.ItemIndex);

  if Effect.Shape <> Value then
  begin
    Effect.Shape:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edBlendingChange(Sender: TObject);
var Value: TPHXParticleBlending;
begin
  Value:= TPHXParticleBlending(edBlending.ItemIndex);

  if Effect.Blending <> Value then
  begin
    Effect.Blending:= Value;

    Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edSizeMinChange(Sender: TObject);
var Value: Single;
begin
  Value:= edSizeMin.Value;

  if Effect.SizeMin <> Value then
  begin
    Effect.SizeMin:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edSizeMaxChange(Sender: TObject);
var Value: Single;
begin
  Value:= edSizeMax.Value;

  if Effect.SizeMax <> Value then
  begin
    Effect.SizeMax:= Value;

    Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edColorMinChange(Sender: TObject);
var Value: TColor4f;
begin
  Value:= Color4f(edColorMin.Selected, 255);

  if Effect.ColorValue <> Value then
  begin
    Effect.ColorValue:= Value;

    Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectApperance.edColorMaxChange(Sender: TObject);
var Value: TColor4f;
begin
  Value:= Color4f(edColorMax.Selected, 255);

  if Effect.ColorVariance <> Value then
  begin
    Effect.ColorVariance:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edTexture.Enabled:= Enabled;
  edTexture.Color  := EnabledColors[Enabled];

  edPatternIndex.Enabled:= Enabled;
  edPatternIndex.Color  := EnabledColors[Enabled];

  edPatternVariance.Enabled:= Enabled;
  edPatternVariance.Color  := EnabledColors[Enabled];

  edPatternSize.Enabled:= Enabled;
  edPatternSize.Color  := EnabledColors[Enabled];

  edShape.Enabled:= Enabled;
  edShape.Color  := EnabledColors[Enabled];

  edBlending.Enabled:= Enabled;
  edBlending.Color  := EnabledColors[Enabled];

  edSizeMin.Enabled:= Enabled;
  edSizeMin.Color  := EnabledColors[Enabled];

  edSizeMax.Enabled:= Enabled;
  edSizeMax.Color  := EnabledColors[Enabled];

  edGrowthMin.Enabled:= Enabled;
  edGrowthMin.Color  := EnabledColors[Enabled];

  edGrowthMax.Enabled:= Enabled;
  edGrowthMax.Color  := EnabledColors[Enabled];

  edGrayscale.Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectApperance.SetEffect(const Value: TPHXParticleEffect);
begin
  FEffect := Value;

  if Assigned(Effect) then
  begin
    EnableControls(True);

    if Effect.TextureName <> '' then
    begin
      edTexture.Text:= Effect.TextureName;
      edTexture.Font.Color:= clBlack;
    end else
    begin
      edTexture.Text:= '(none)';
      edTexture.Font.Color:= clSilver;
    end;


    edPatternSize.ItemIndex:= Ord(Effect.PatternSize);
    edPatternIndex.   Text   := IntToStr(Effect.PatternIndex);
    edPatternVariance.Value  :=          Effect.PatternVariance;

    edGrayscale.Checked := Value.Grayscale;

    edShape       .ItemIndex:= Ord( Effect.Shape);
    edBlending    .ItemIndex:= Ord( Effect.Blending);

    edSizeMin.Value   := Effect.SizeMin;
    edSizeMax.Value   := Effect.SizeMax;

    edGrowthMin.Value   := Effect.GrowthMin;
    edGrowthMax.Value   := Effect.GrowthMax;

  //  edInitalUpdateCount    .Value:= Effect.InitalUpdateCount;
  //  edInitalUpdateInterval .Value:= Effect.InitalUpdateInterval;

    edColorMin.Selected:= ColorToRGB(Value.ColorValue);
    edColorMax.Selected:= ColorToRGB(Value.ColorVariance);

  end else
  begin
    EnableControls(False);
  end;
end;


end.
