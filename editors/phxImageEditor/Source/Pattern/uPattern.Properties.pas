unit uPattern.Properties;

interface

uses
  SysUtils, Variants, Classes,
  Graphics, Controls, Forms,Dialogs,StdCtrls,
  Spin, ExtCtrls,

  phxTypes,
  phxImage,

  uActions;

type

//------------------------------------------------------------------------------

{ TFrmPatternProperties }

TFrmPatternProperties = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edPatternName: TEdit;
    edPatternX: TSpinEdit;
    edPatternY: TSpinEdit;
    edPatternWidth: TSpinEdit;
    edPatternHeight: TSpinEdit;
    edPatternPivotX: TSpinEdit;
    edPatternPivotY: TSpinEdit;
    edPatternMirror: TCheckBox;
    edPatternFlip: TCheckBox;
    btnCenterPivot: TButton;
    procedure edPatternNameChange(Sender: TObject);
    procedure edPatternXChange(Sender: TObject);
    procedure edPatternYChange(Sender: TObject);
    procedure edPatternWidthChange(Sender: TObject);
    procedure edPatternHeightChange(Sender: TObject);
    procedure edPatternPivotXChange(Sender: TObject);
    procedure edPatternPivotYChange(Sender: TObject);
    procedure btnCenterPivotClick(Sender: TObject);
    procedure edPatternMirrorClick(Sender: TObject);
    procedure edPatternFlipClick(Sender: TObject);
  private
    FImage: TPHXImage;
    FPattern: Integer;

    function GetPattern: TPHXPattern;

    procedure Changed;

    procedure EnableControls(Enabled: Boolean);
    procedure EnableEvents(Enabled: Boolean);

    procedure SetImage(const Value: TPHXImage);
    procedure SetPattern(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    Property Image: TPHXImage read FImage write SetImage;
    property Pattern: Integer read FPattern write SetPattern;
  end;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
constructor TFrmPatternProperties.Create(AOwner: TComponent);
begin
  inherited;
  EnableControls(False);
  EnableEvents(False);
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.Changed;
begin
  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = ( clBtnFace, clWindow);
begin
  edPatternName        .Enabled:= Enabled;
  edPatternX           .Enabled:= Enabled;
  edPatternY           .Enabled:= Enabled;
  edPatternWidth       .Enabled:= Enabled;
  edPatternHeight      .Enabled:= Enabled;
  edPatternPivotX      .Enabled:= Enabled;
  edPatternPivotY      .Enabled:= Enabled;
  edPatternFlip        .Enabled:= Enabled;
  edPatternMirror      .Enabled:= Enabled;

  edPatternName   .Color:= EnabledColors[Enabled];
  edPatternX      .Color:= EnabledColors[Enabled];
  edPatternY      .Color:= EnabledColors[Enabled];
  edPatternWidth  .Color:= EnabledColors[Enabled];
  edPatternHeight .Color:= EnabledColors[Enabled];
  edPatternPivotX .Color:= EnabledColors[Enabled];
  edPatternPivotY .Color:= EnabledColors[Enabled];


  btnCenterPivot .Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edPatternName        .OnChange:= edPatternNameChange;
    edPatternX           .OnChange:= edPatternXChange;
    edPatternY           .OnChange:= edPatternYChange;
    edPatternWidth       .OnChange:= edPatternWidthChange;
    edPatternHeight      .OnChange:= edPatternHeightChange;
    edPatternPivotX      .OnChange:= edPatternPivotXChange;
    edPatternPivotY      .OnChange:= edPatternPivotYChange;
  end else
  begin
    edPatternName        .OnChange:= nil;
    edPatternX           .OnChange:= nil;
    edPatternY           .OnChange:= nil;
    edPatternWidth       .OnChange:= nil;
    edPatternHeight      .OnChange:= nil;
    edPatternPivotX      .OnChange:= nil;
    edPatternPivotY      .OnChange:= nil;

  end;
end;

//------------------------------------------------------------------------------
function TFrmPatternProperties.GetPattern: TPHXPattern;
begin
  Result:= Image.Patterns.List^[Pattern];
end;

{$REGION 'OnChange'}

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternNameChange(Sender: TObject);
var Value: ShortString;
begin
  Value:= ShortString(edPatternName.Text);

  if Image.Patterns.List^[Pattern].Name <> Value then
  begin
    Image.Patterns.List^[Pattern].Name:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternXChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edPatternX.Value);

  if Image.Patterns.List^[Pattern].X <> Value then
  begin
    Image.Patterns.List^[Pattern].X:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternYChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edPatternY.Value);

  if Image.Patterns.List^[Pattern].Y <> Value then
  begin
    Image.Patterns.List^[Pattern].Y:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternWidthChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edPatternWidth.Value);

  if Image.Patterns.List^[Pattern].Width <> Value then
  begin
    Image.Patterns.List^[Pattern].Width:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternHeightChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edPatternHeight.Value);

  if Image.Patterns.List^[Pattern].Height <> Value then
  begin
    Image.Patterns.List^[Pattern].Height:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternPivotXChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edPatternPivotX.Value);

  if Image.Patterns.List^[Pattern].Pivot.X <> Value then
  begin
    Image.Patterns.List^[Pattern].Pivot.X:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternPivotYChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edPatternPivotY.Value);

  if Image.Patterns.List^[Pattern].Pivot.Y <> Value then
  begin
    Image.Patterns.List^[Pattern].Pivot.Y:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternFlipClick(Sender: TObject);
var Value: Boolean;
begin
  Value:= edPatternFlip.Checked;

  if Image.Patterns.List^[Pattern].Flip <> Value then
  begin
    Image.Patterns.List^[Pattern].Flip:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.edPatternMirrorClick(Sender: TObject);
var Value: Boolean;
begin
  Value:= edPatternMirror.Checked;

  if Image.Patterns.List^[Pattern].Mirror <> Value then
  begin
    Image.Patterns.List^[Pattern].Mirror:= Value;

    Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmPatternProperties.btnCenterPivotClick(Sender: TObject);
var Size: TVector2i;
begin
  Size.X:= StrToIntDef(edPatternWidth .Text, 0);
  Size.Y:= StrToIntDef(edPatternHeight.Text, 0);

  edPatternPivotX.Value:= (Size.X div 2);
  edPatternPivotY.Value:= (Size.Y div 2);

  Changed;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.SetImage(const Value: TPHXImage);
begin
  FImage := Value;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternProperties.SetPattern(const Value: Integer);
var APattern: TPHXPattern;
begin
  EnableEvents(False);

  FPattern := Value;
  if Assigned(Image) and (Pattern >= 0) and (Pattern < Image.Patterns.Count) then
  begin
    EnableControls(True);

    APattern:= GetPattern;

    edPatternName  .Text  := String(APattern.Name);
    edPatternX     .Value :=        APattern.X;
    edPatternY     .Value :=        APattern.Y;
    edPatternWidth .Value :=        APattern.Width;
    edPatternHeight.Value :=        APattern.Height;
    edPatternPivotX.Value :=        APattern.Pivot.X;
    edPatternPivotY.Value :=        APattern.Pivot.Y;
    edPatternMirror.Checked:=        APattern.Mirror;
    edPatternFlip  .Checked:=        APattern.Flip;

    EnableEvents(True);
  end else
  begin
    EnableControls(False);

    edPatternName  .Text := '';
    edPatternX     .Value:= 0;
    edPatternY     .Value:= 0;
    edPatternWidth .Value:= 0;
    edPatternHeight.Value:= 0;
    edPatternPivotX.Value:= 0;
    edPatternPivotY.Value:= 0;
  end;
end;



end.
