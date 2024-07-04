unit uTag.Properties;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,
  JvExMask, JvSpin,

  phxTypes,
  phxImage,

  uActions;

type
  TFrmTagProperties = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    edName: TEdit;
    edPattern: TComboBox;
    edX: TJvSpinEdit;
    edY: TJvSpinEdit;
    edRotation: TJvSpinEdit;
    procedure edNameChange(Sender: TObject);
    procedure edXChange(Sender: TObject);
    procedure edPatternChange(Sender: TObject);
    procedure edYChange(Sender: TObject);
    procedure edRotationChange(Sender: TObject);
  private
    FImage: TPHXImage;
    FTag  : Integer;

    function GetTag : TPHXTag ;

    procedure Changed;

    procedure EnableControls(Enabled: Boolean);

    procedure SetImage(const Value: TPHXImage);
    procedure SetTag(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    Property Image: TPHXImage read FImage write SetImage;
    property Tag: Integer read FTag write SetTag;
  end;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
constructor TFrmTagProperties.Create(AOwner: TComponent);
begin
  inherited;

  SetTag(-1);
end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.Changed;
begin
  ModActions.Document.Changed;
end;



{$REGION 'OnChange'}

//------------------------------------------------------------------------------
procedure TFrmTagProperties.edNameChange(Sender: TObject);
var Value: ShortString;
begin
  Value:= ShortString(edName.Text);

  if Assigned(Image) and (Image.Tags.List^[Tag].Name <> Value) then
  begin
    Image.Tags.List^[Tag].Name:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.edPatternChange(Sender: TObject);
var Value: Integer;
begin
  Value:= edPattern.ItemIndex;

  if Assigned(Image) and (Image.Tags.List^[Tag].Pattern <> Value) then
  begin
    Image.Tags.List^[Tag].Pattern:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.edXChange(Sender: TObject);
var Value: Single;
begin
  Value:= edX.Value;

  if Assigned(Image) and (Image.Tags.List^[Tag].X <> Value) then
  begin
    Image.Tags.List^[Tag].X:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.edYChange(Sender: TObject);
var Value: Single;
begin
  Value:= edY.Value;

  if Assigned(Image) and (Image.Tags.List^[Tag].Y <> Value) then
  begin
    Image.Tags.List^[Tag].Y:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.edRotationChange(Sender: TObject);
var Value: Single;
begin
  Value:= edRotation.Value;

  if Assigned(Image) and (Image.Tags.List^[Tag].Rotation <> Value) then
  begin
    Image.Tags.List^[Tag].Rotation:= Value;

    Changed;
  end;
end;


{$ENDREGION}

//------------------------------------------------------------------------------
procedure TFrmTagProperties.SetTag(const Value: Integer);
var ATag: TPHXTag;
begin
  FTag := Value;

  if Assigned(Image) and (Tag >= 0) and (Tag < Image.Tags.Count) then
  begin
    EnableControls(True);

    ATag:= GetTag;

    edName    .Text  := String(ATag.Name);
    edPattern .ItemIndex :=    ATag.Pattern;
    edX       .Value :=        ATag.X;
    edY       .Value :=        ATag.Y;
    edRotation.Value :=        ATag.Rotation;
  end else
  begin
    EnableControls(False);

    edName    .Text := '';
    edPattern .Text:= '';
    edX       .Value:= 0;
    edY       .Value:= 0;
    edRotation.Value:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.SetImage(const Value: TPHXImage);
var Index: Integer;
begin
  FImage:= Value;

  if Assigned(FImage) then
  begin
    edPattern.Items.BeginUpdate;
    edPattern.Items.Clear;
    for Index := 0 to FImage.Patterns.Count-1 do
    begin
      edPattern.Items.Add(String(FImage.Patterns[Index].Name));
    end;

    edPattern.Items.EndUpdate;
  end else
  begin
    edPattern.Items.Clear;
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmTagProperties.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = ( clBtnFace, clWindow);
begin
  edName    .Enabled:= Enabled;
  edPattern .Enabled:= Enabled;
  edX       .Enabled:= Enabled;
  edY       .Enabled:= Enabled;
  edRotation.Enabled:= Enabled;

  edName    .Color:= EnabledColors[Enabled];
  edPattern .Color:= EnabledColors[Enabled];
  edX       .Color:= EnabledColors[Enabled];
  edY       .Color:= EnabledColors[Enabled];
  edRotation.Color:= EnabledColors[Enabled];
end;


//------------------------------------------------------------------------------
function TFrmTagProperties.GetTag: TPHXTag;
begin
  Result:= Image.Tags.List^[Tag];
end;


end.
