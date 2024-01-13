unit uModel.Tags;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Mask,

  uActions,

  phxModel;

type
  TFrmModelTags = class(TFrame)
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    lwTags: TListView;
    Label1: TLabel;
    Label2: TLabel;
    edName: TEdit;
    GroupBox3: TGroupBox;
    edRotationX: TSpinEdit;
    edRotationY: TSpinEdit;
    edRotationZ: TSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edJoint: TComboBox;
    edPositionX: TSpinEdit;
    edPositionY: TSpinEdit;
    edPositionZ: TSpinEdit;
    procedure lwTagsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lwTagsClick(Sender: TObject);

    procedure edPositionXChange(Sender: TObject);
    procedure edPositionYChange(Sender: TObject);
    procedure edPositionZChange(Sender: TObject);
  private
    procedure EditChange(Sender: TObject);
  private
    FMesh: TPHXMesh;
    FSelected: Integer;

    procedure Changed;

    procedure EnableEvents  (Enabled: Boolean);
    procedure EnableControls(Enabled: Boolean);

    procedure SetMesh(const Value: TPHXMesh);
    procedure SetSelected(const Value: Integer);
    procedure lwTagsUpdate;
  public
    Property Mesh: TPHXMesh read FMesh write SetMesh;

    Property Selected: Integer read FSelected write SetSelected;
  end;

implementation

{$R *.dfm}
//------------------------------------------------------------------------------
procedure TFrmModelTags.Changed;
begin
//  ModActions.Document.Changed;
end;

// TFrmTags
//------------------------------------------------------------------------------
procedure TFrmModelTags.SetMesh(const Value: TPHXMesh);
var Index: Integer;
begin
  EnableEvents(False);

  if FMesh <> Value then
  begin
    FMesh := Value;

    SetSelected(-1);
  end;

  if Assigned(FMesh) then
  begin
    EnableControls(Selected <> -1);

    edJoint.Items.BeginUpdate;
    edJoint.Items.Clear;
    for Index := 0 to Mesh.Joints.Count - 1 do
    begin
      edJoint.Items.Add( Mesh.Joints.List^[Index].Name );
    end;
    edJoint.Items.EndUpdate;

    lwTagsUpdate;

    lwTags.Color:= clWindow;
  end else
  begin
    lwTags.Color:= clBtnFace;

    EnableControls(False);
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.SetSelected(const Value: Integer);
begin
  FSelected := Value;

  EnableEvents(False);

  if Assigned(FMesh) and (FSelected >= 0) and (FSelected < FMesh.Tags.Count) then
  begin
    EnableControls(True);

    edName     .Text:= Mesh.Tags.List^[Selected].Name;
    edJoint    .Text:= Mesh.Tags.List^[Selected].JointName;

    edPositionX.Text:= FloatToStr(Mesh.Tags.List^[Selected].Position.X);
    edPositionY.Text:= FloatToStr(Mesh.Tags.List^[Selected].Position.Y);
    edPositionZ.Text:= FloatToStr(Mesh.Tags.List^[Selected].Position.Z);

    edRotationX.Value:= Mesh.Tags.List^[Selected].Direction.X;
    edRotationY.Value:= Mesh.Tags.List^[Selected].Direction.Y;
    edRotationZ.Value:= Mesh.Tags.List^[Selected].Direction.Z;

    EnableEvents(True);
  end else
  begin
    EnableControls(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.lwTagsUpdate;
var Index: Integer;
var Tag  : TPHXMeshTag;
var Item : TListItem;
begin
  lwTags.Items.BeginUpdate;
  lwTags.Items.Clear;

  for Index := 0 to Mesh.Tags.Count - 1 do
  begin
    Tag:= Mesh.Tags.List^[Index];

    Item:= lwTags.Items.Add;
    Item.Caption:= Tag.Name;
  end;
  lwTags.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.lwTagsClick(Sender: TObject);
begin
  SetSelected(lwTags.ItemIndex);
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.lwTagsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  SetSelected(lwTags.ItemIndex);
end;


//------------------------------------------------------------------------------
procedure TFrmModelTags.EditChange(Sender: TObject);
begin
  Mesh.Tags.List^[Selected].Name      := ShortString(edName.Text);


  Mesh.Tags.List^[Selected].Direction.X:= edRotationX.Value;
  Mesh.Tags.List^[Selected].Direction.Y:= edRotationY.Value;
  Mesh.Tags.List^[Selected].Direction.Z:= edRotationZ.Value;

 // Mesh.Tags.List^[Selected].Name      := JointName    .Text;

//  Mesh.Tags.List^[Selected].Material  := edMaterial.Text;
 // Mesh.Author     := edAuthor     .Text;
 // Mesh.Version    := edVersion    .Text;
 // Mesh.Description:= edDescription.Text;

end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.EnableControls(Enabled: Boolean);
const EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName     .Color:= EnabledColors[Enabled];
  edJoint    .Color:= EnabledColors[Enabled];
  edPositionX.Color:= EnabledColors[Enabled];
  edPositionY.Color:= EnabledColors[Enabled];
  edPositionZ.Color:= EnabledColors[Enabled];
  edRotationX.Color:= EnabledColors[Enabled];
  edRotationY.Color:= EnabledColors[Enabled];
  edRotationZ.Color:= EnabledColors[Enabled];

  edName      .Enabled:= Enabled;
  edJoint     .Enabled:= Enabled;
  edPositionX .Enabled:= Enabled;
  edPositionY .Enabled:= Enabled;
  edPositionZ .Enabled:= Enabled;
  edRotationX .Enabled:= Enabled;
  edRotationY .Enabled:= Enabled;
  edRotationZ .Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.EnableEvents(Enabled: Boolean);
var EnabledEvents: array[Boolean] of TNotifyEvent;
begin
  EnabledEvents[True ]:= EditChange;
  EnabledEvents[False]:= nil;

  edName      .OnChange:= EnabledEvents[Enabled];
  edJoint     .OnChange:= EnabledEvents[Enabled];
  edRotationX .OnChange:= EnabledEvents[Enabled];
  edRotationY .OnChange:= EnabledEvents[Enabled];
  edRotationZ .OnChange:= EnabledEvents[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.edPositionXChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(edPositionX.Text, Mesh.Tags.List^[Selected].Position.X);

  if Mesh.Tags.List^[Selected].Position.X <> Value then
  begin
    Mesh.Tags.List^[Selected].Position.X:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.edPositionYChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(edPositionY.Text, Mesh.Tags.List^[Selected].Position.Y);

  if Mesh.Tags.List^[Selected].Position.Y <> Value then
  begin
    Mesh.Tags.List^[Selected].Position.Y:= Value;

    Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelTags.edPositionZChange(Sender: TObject);
var Value: Single;
begin
  Value:= StrToFloatDef(edPositionZ.Text, Mesh.Tags.List^[Selected].Position.Z);

  if Mesh.Tags.List^[Selected].Position.Z <> Value then
  begin
    Mesh.Tags.List^[Selected].Position.Z:= Value;

    Changed;
  end;
end;



end.
