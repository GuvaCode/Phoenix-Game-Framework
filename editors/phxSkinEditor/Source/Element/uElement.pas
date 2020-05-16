unit uElement;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, Buttons, Mask, JvExMask, JvSpin, JvToolEdit,

  TypInfo,

  uActions,

  phxTypes,

  phxGraphics,
  phxSkin;

type
  TFrmElement = class(TFrame)
    Timer1: TTimer;
    GroupBox1: TGroupBox;
    edName: TJvComboEdit;
    Label5: TLabel;
    Label18: TLabel;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    edBoundsLeft: TJvSpinEdit;
    edBoundsTop: TJvSpinEdit;
    edBoundsRight: TJvSpinEdit;
    edBoundsBottom: TJvSpinEdit;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    edMarginLeft: TJvSpinEdit;
    edMarginTop: TJvSpinEdit;
    edMarginRight: TJvSpinEdit;
    edMarginBottom: TJvSpinEdit;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    Label13: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    edShadowLeft: TJvSpinEdit;
    edShadowTop: TJvSpinEdit;
    edShadowRight: TJvSpinEdit;
    edShadowBottom: TJvSpinEdit;
    GroupBox6: TGroupBox;
    Label17: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    edContentMarginLeft: TJvSpinEdit;
    edContentMarginTop: TJvSpinEdit;
    edContentMarginRight: TJvSpinEdit;
    edContentMarginBottom: TJvSpinEdit;
    GroupBox5: TGroupBox;
    Label16: TLabel;
    edContentColor: TColorBox;
    btnPickForeground: TSpeedButton;
    btnPickBackground: TSpeedButton;
    edPart: TEdit;
    Label11: TLabel;
    ColorDialog1: TColorDialog;
    Panel1: TPanel;
    edColor: TPanel;
    procedure btnPickForegroundClick(Sender: TObject);
    procedure btnPickBackgroundClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure edNameButtonClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure ed2ColorChange(Sender: TObject);
    procedure edBoundsLeftChange(Sender: TObject);
    procedure edBoundsTopChange(Sender: TObject);
    procedure edBoundsRightChange(Sender: TObject);
    procedure edBoundsBottomChange(Sender: TObject);
    procedure edMarginLeftChange(Sender: TObject);
    procedure edMarginTopChange(Sender: TObject);
    procedure edMarginRightChange(Sender: TObject);
    procedure edMarginBottomChange(Sender: TObject);
    procedure edShadowLeftChange(Sender: TObject);
    procedure edShadowTopChange(Sender: TObject);
    procedure edShadowRightChange(Sender: TObject);
    procedure edShadowBottomChange(Sender: TObject);
    procedure edColorClick(Sender: TObject);
  private
    FElement  : TPHXSkinElement;
    FOnChanged: TNotifyEvent;
    procedure SetElement(const Value: TPHXSkinElement);

    procedure EnableEvents(Enabled: Boolean);
    procedure EnableControls(Enabled: Boolean);
    procedure SkinChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;


    property Element: TPHXSkinElement read FElement write SetElement;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

uses uTools, uElementName, uMain;

{$R *.dfm}

//------------------------------------------------------------------------------
constructor TFrmElement.Create(AOwner: TComponent);
begin
  inherited;

  DoubleBuffered:= True;
end;



{$REGION 'Edit events'}

//------------------------------------------------------------------------------
procedure TFrmElement.edNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edName.Text;

  if Assigned(Element) and (Element.Name <> Value) then
  begin
    Element.Name:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edNameButtonClick(Sender: TObject);
begin
  frmElementName:= TfrmElementName.Create(Self);
//  frmElementName.Element:= Element;
  frmElementName.Caption:= 'Element Name';

  frmElementName.ElementName:= edName.Text;

  if frmElementName.ShowModal = mrOk then
  begin
    edName.Text:= frmElementName.ElementName
    ;
  end;
  frmElementName.Free;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.ed2ColorChange(Sender: TObject);
var Value: TColor4f;
begin
  Value:= TColor4f.Create(edColor.Color);

  if Assigned(Element) and (Element.Color <> Value) then
  begin
    Element.Color:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edColorClick(Sender: TObject);
begin
  ColorDialog1.Color:= edColor.Color;

  if ColorDialog1.Execute then
  begin
    edColor.Color:= ColorDialog1.Color;
  end;

  ed2ColorChange(Sender);
end;



//------------------------------------------------------------------------------
procedure TFrmElement.edBoundsTopChange(Sender: TObject);
var Value : Integer;
var Bounds: TRecti;
begin
  Value:= Round(edBoundsTop.Value);

  if Assigned(Element) and (Element.Bounds.Top <> Value) then
  begin
    Bounds:= Element.Bounds;
    Bounds.Top:= Value;

    Element.Bounds:= Bounds;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edBoundsLeftChange(Sender: TObject);
var Value : Integer;
var Bounds: TRecti;
begin
  Value:= Round(edBoundsLeft.Value);

  if Assigned(Element) and (Element.Bounds.Left <> Value) then
  begin
    Bounds:= Element.Bounds;
    Bounds.Left:= Value;

    Element.Bounds:= Bounds;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edBoundsBottomChange(Sender: TObject);
var Value : Integer;
var Bounds: TRecti;
begin
  Value:= Round(edBoundsBottom.Value);

  if Assigned(Element) and (Element.Bounds.Bottom <> Value) then
  begin
    Bounds:= Element.Bounds;
    Bounds.Bottom:= Value;

    Element.Bounds:= Bounds;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edBoundsRightChange(Sender: TObject);
var Value : Integer;
var Bounds: TRecti;
begin
  Value:= Round(edBoundsRight.Value);

  if Assigned(Element) and (Element.Bounds.Right <> Value) then
  begin
    Bounds:= Element.Bounds;
    Bounds.Right:= Value;

    Element.Bounds:= Bounds;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edMarginTopChange(Sender: TObject);
var Value  : Integer;
var Margins: TRecti;
begin
  Value:= Round(edMarginTop.Value);

  if Assigned(Element) and (Element.Margins.Top <> Value) then
  begin
    Margins:= Element.Margins;
    Margins.Top:= Value;

    Element.Margins:= Margins;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edMarginLeftChange(Sender: TObject);
var Value  : Integer;
var Margins: TRecti;
begin
  Value:= Round(edMarginLeft.Value);

  if Assigned(Element) and (Element.Margins.Left <> Value) then
  begin
    Margins:= Element.Margins;
    Margins.Left:= Value;

    Element.Margins:= Margins;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edMarginBottomChange(Sender: TObject);
var Value  : Integer;
var Margins: TRecti;
begin
  Value:= Round(edMarginBottom.Value);

  if Assigned(Element) and (Element.Margins.Bottom <> Value) then
  begin
    Margins:= Element.Margins;
    Margins.Bottom:= Value;

    Element.Margins:= Margins;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edMarginRightChange(Sender: TObject);
var Value  : Integer;
var Margins: TRecti;
begin
  Value:= Round(edMarginRight.Value);

  if Assigned(Element) and (Element.Margins.Right <> Value) then
  begin
    Margins:= Element.Margins;
    Margins.Right:= Value;

    Element.Margins:= Margins;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edShadowTopChange(Sender: TObject);
var Value : Integer;
var Shadow: TRecti;
begin
  Value:= Round(edShadowTop.Value);

  if Assigned(Element) and (Element.Shadow.Top <> Value) then
  begin
    Shadow:= Element.Shadow;
    Shadow.Top:= Value;

    Element.Shadow:= Shadow;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edShadowLeftChange(Sender: TObject);
var Value : Integer;
var Shadow: TRecti;
begin
  Value:= Round(edShadowLeft.Value);

  if Assigned(Element) and (Element.Shadow.Left <> Value) then
  begin
    Shadow:= Element.Shadow;
    Shadow.Left:= Value;

    Element.Shadow:= Shadow;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edShadowBottomChange(Sender: TObject);
var Value : Integer;
var Shadow: TRecti;
begin
  Value:= Round(edShadowBottom.Value);

  if Assigned(Element) and (Element.Shadow.Bottom <> Value) then
  begin
    Shadow:= Element.Shadow;
    Shadow.Bottom:= Value;

    Element.Shadow:= Shadow;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmElement.edShadowRightChange(Sender: TObject);
var Value : Integer;
var Shadow: TRecti;
begin
  Value:= Round(edShadowRight.Value);

  if Assigned(Element) and (Element.Shadow.Right <> Value) then
  begin
    Shadow:= Element.Shadow;
    Shadow.Right:= Value;

    Element.Shadow:= Shadow;

    ModActions.Document.Changed;
  end;
end;



{$ENDREGION}

//------------------------------------------------------------------------------
function PartToStr(const Part: TPHXThemedPart): String;
begin
  Result:= GetEnumName( TypeInfo(TPHXThemedPart), Ord(Part));
end;

//------------------------------------------------------------------------------
procedure TFrmElement.SetElement(const Value: TPHXSkinElement);
begin
  EnableEvents(False);

  FElement := Value;

  if Assigned(FElement) then
  begin
    EnableControls(True);

    edName .Text :=             Element.Name;
    edPart .Text :=  PartToStr (Element.Part);
    edColor.Color:=  ColorToRGB(Element.Color);

    edBoundsLeft  .Value:= Element.Bounds.Left;
    edBoundsRight .Value:= Element.Bounds.Right;
    edBoundsTop   .Value:= Element.Bounds.Top;
    edBoundsBottom.Value:= Element.Bounds.Bottom;

    edMarginLeft  .Value:= Element.Margins.Left;
    edMarginRight .Value:= Element.Margins.Right;
    edMarginTop   .Value:= Element.Margins.Top;
    edMarginBottom.Value:= Element.Margins.Bottom;

    edShadowLeft  .Value:= Element.Shadow.Left;
    edShadowRight .Value:= Element.Shadow.Right;
    edShadowTop   .Value:= Element.Shadow.Top;
    edShadowBottom.Value:= Element.Shadow.Bottom;


    edContentMarginLeft  .Value:= Element.TextPadding.Left;
    edContentMarginRight .Value:= Element.TextPadding.Right;
    edContentMarginTop   .Value:= Element.TextPadding.Top;
    edContentMarginBottom.Value:= Element.TextPadding.Bottom;

    edContentColor.Selected:= TColor( Element.TextColor.ToColor );


    EnableEvents(True);
  end else
  begin
    edName .Text    := '';
    edPart .Text    := '';
//    edColor.Selected:= clNone;

    EnableControls(False);
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmElement.SkinChange(Sender: TObject);
begin
  Element.TextPadding:= TRecti.Create(
    Round(edContentMarginLeft  .Value),
    Round(edContentMarginTop   .Value),
    Round(edContentMarginRight .Value),
    Round(edContentMarginBottom.Value)
  );

  Element.TextColor:= TColor4f.Create(edContentColor.Selected );

  ModActions.Editor.Invalidate;

  FrmMain.FrmElementPreview.PaintBox1.Invalidate;

  if Assigned(OnChanged) then OnChanged(Self);
end;




//------------------------------------------------------------------------------
procedure TFrmElement.EnableEvents(Enabled: Boolean);
var EnabledEvents: Array[Boolean] of TNotifyEvent;
begin
  EnabledEvents[False]:= nil;
  EnabledEvents[True]:= SkinChange;

  edContentMarginLeft  .OnChange:= EnabledEvents[Enabled];
  edContentMarginRight .OnChange:= EnabledEvents[Enabled];
  edContentMarginTop   .OnChange:= EnabledEvents[Enabled];
  edContentMarginBottom.OnChange:= EnabledEvents[Enabled];

  edContentColor.OnChange:= EnabledEvents[Enabled];
end;


//------------------------------------------------------------------------------
procedure TFrmElement.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName.Enabled:= Enabled;
  edName.Color  := EnabledColors[Enabled];

  edColor.Enabled:= Enabled;
 // edColor.Color  := EnabledColors[Enabled];

  edBoundsLeft.Enabled:= Enabled;
  edBoundsLeft.Color  := EnabledColors[Enabled];

  edBoundsRight.Enabled:= Enabled;
  edBoundsRight.Color  := EnabledColors[Enabled];

  edBoundsTop.Enabled:= Enabled;
  edBoundsTop.Color  := EnabledColors[Enabled];

  edBoundsBottom.Enabled:= Enabled;
  edBoundsBottom.Color  := EnabledColors[Enabled];


  edMarginLeft.Enabled:= Enabled;
  edMarginLeft.Color  := EnabledColors[Enabled];

  edMarginRight.Enabled:= Enabled;
  edMarginRight.Color  := EnabledColors[Enabled];

  edMarginTop.Enabled:= Enabled;
  edMarginTop.Color  := EnabledColors[Enabled];

  edMarginBottom.Enabled:= Enabled;
  edMarginBottom.Color  := EnabledColors[Enabled];


  edShadowLeft.Enabled:= Enabled;
  edShadowLeft.Color  := EnabledColors[Enabled];

  edShadowRight.Enabled:= Enabled;
  edShadowRight.Color  := EnabledColors[Enabled];

  edShadowTop.Enabled:= Enabled;
  edShadowTop.Color  := EnabledColors[Enabled];

  edShadowBottom.Enabled:= Enabled;
  edShadowBottom.Color  := EnabledColors[Enabled];



  edContentMarginLeft.Enabled:= Enabled;
  edContentMarginLeft.Color  := EnabledColors[Enabled];

  edContentMarginRight.Enabled:= Enabled;
  edContentMarginRight.Color  := EnabledColors[Enabled];

  edContentMarginTop.Enabled:= Enabled;
  edContentMarginTop.Color  := EnabledColors[Enabled];

  edContentMarginBottom.Enabled:= Enabled;
  edContentMarginBottom.Color  := EnabledColors[Enabled];

  edContentColor.Enabled:= Enabled;
  edContentColor.Color  := EnabledColors[Enabled];






  btnPickForeground.Enabled:= Enabled;
  btnPickBackground.Enabled:= Enabled;
end;








//------------------------------------------------------------------------------
procedure TFrmElement.Timer1Timer(Sender: TObject);
begin
  btnPickBackground.Down:= modTools.Tools.IsActive(TOOL_SELECT_STATE_BACKGROUND);
  btnPickForeground.Down:= modTools.Tools.IsActive(TOOL_SELECT_STATE_FOREGROUND);
end;


//------------------------------------------------------------------------------
procedure TFrmElement.btnPickBackgroundClick(Sender: TObject);
begin
  modTools.Tools.SetActive(TOOL_SELECT_STATE_BACKGROUND);

  Timer1Timer(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmElement.btnPickForegroundClick(Sender: TObject);
begin
  modTools.Tools.SetActive(TOOL_SELECT_STATE_FOREGROUND);

  Timer1Timer(Sender);
end;



end.
