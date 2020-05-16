unit uModel.Joints;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ComCtrls, Mask, JvExMask, JvSpin,

  phxModel;

type
  TFrmModelJoints = class(TFrame)
    lwJoints: TListView;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edName: TEdit;
    edJoint: TComboBox;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edRotationX: TJvSpinEdit;
    edRotationY: TJvSpinEdit;
    edRotationZ: TJvSpinEdit;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edPositionX: TJvSpinEdit;
    edPositionY: TJvSpinEdit;
    edPositionZ: TJvSpinEdit;
    procedure lwJointsClick(Sender: TObject);
    procedure lwJointsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure EditChange(Sender: TObject);
  private
    FMesh: TPHXMesh;
    FSelected: Integer;

    procedure EnableEvents  (Enabled: Boolean);
    procedure EnableControls(Enabled: Boolean);

    procedure SetMesh(const Value: TPHXMesh);
    procedure SetSelected(const Value: Integer);
    procedure lwJointsUpdate;
  public
    Property Mesh: TPHXMesh read FMesh write SetMesh;

    Property Selected: Integer read FSelected write SetSelected;
  end;

implementation

{$R *.dfm}

// TFrmJoints
//------------------------------------------------------------------------------
procedure TFrmModelJoints.SetMesh(const Value: TPHXMesh);
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
      edJoint.Items.Add( String(Mesh.Joints.List^[Index].Name) );
    end;
    edJoint.Items.EndUpdate;


    lwJointsUpdate;
    lwJoints.Color:= clWindow;
  end else
  begin
    lwJoints.Color:= clBtnFace;

    EnableControls(False);
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmModelJoints.SetSelected(const Value: Integer);
begin
  FSelected := Value;

  EnableEvents(False);

  if Assigned(Mesh) and (FSelected >= 0) and (FSelected < FMesh.Joints.Count) then
  begin
    EnableControls(True);

    edName     .Text:= String(Mesh.Joints.List^[Selected].Name);
    edJoint    .Text:= String(Mesh.Joints.List^[Selected].ParentName);

    edPositionX.Value:= Mesh.Joints.List^[Selected].Position.X;
    edPositionY.Value:= Mesh.Joints.List^[Selected].Position.Y;
    edPositionZ.Value:= Mesh.Joints.List^[Selected].Position.Z;

    edRotationX.Value:= Mesh.Joints.List^[Selected].Rotation.X;
    edRotationY.Value:= Mesh.Joints.List^[Selected].Rotation.Y;
    edRotationZ.Value:= Mesh.Joints.List^[Selected].Rotation.Z;

    EnableEvents(True);
  end else
  begin
    EnableControls(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelJoints.EditChange(Sender: TObject);
begin
  Mesh.Tags.List^[Selected].Name := ShortString(edName.Text);

  Mesh.Tags.List^[Selected].Position.X:= edPositionX.Value;
  Mesh.Tags.List^[Selected].Position.Y:= edPositionY.Value;
  Mesh.Tags.List^[Selected].Position.Z:= edPositionZ.Value;

  Mesh.Tags.List^[Selected].Direction.X:= edRotationX.Value;
  Mesh.Tags.List^[Selected].Direction.Y:= edRotationY.Value;
  Mesh.Tags.List^[Selected].Direction.Z:= edRotationZ.Value;

  //  Mesh.Joints.List^[Selected].ParentName:= edJoint    .Text;

end;

//------------------------------------------------------------------------------
procedure TFrmModelJoints.EnableControls(Enabled: Boolean);
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
procedure TFrmModelJoints.EnableEvents(Enabled: Boolean);
var EnabledEvents: array[Boolean] of TNotifyEvent;
begin
  EnabledEvents[True ]:= EditChange;
  EnabledEvents[False]:= nil;

  edName      .OnChange:= EnabledEvents[Enabled];
  edJoint     .OnChange:= EnabledEvents[Enabled];
  edPositionX .OnChange:= EnabledEvents[Enabled];
  edPositionY .OnChange:= EnabledEvents[Enabled];
  edPositionZ .OnChange:= EnabledEvents[Enabled];
  edRotationX .OnChange:= EnabledEvents[Enabled];
  edRotationY .OnChange:= EnabledEvents[Enabled];
  edRotationZ .OnChange:= EnabledEvents[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmModelJoints.lwJointsUpdate;
var Index: Integer;
var Joint: TPHXMeshJoint;
var Item : TListItem;
begin
  lwJoints.Items.BeginUpdate;
  lwJoints.Items.Clear;

  for Index := 0 to Mesh.Joints.Count - 1 do
  begin
    Joint:= Mesh.Joints.List^[Index];

    Item:= lwJoints.Items.Add;
    Item.Caption:= String(Joint.Name);
  end;
  lwJoints.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmModelJoints.lwJointsClick(Sender: TObject);
begin
  SetSelected(lwJoints.ItemIndex);
end;

//------------------------------------------------------------------------------
procedure TFrmModelJoints.lwJointsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  SetSelected(lwJoints.ItemIndex);
end;

end.
