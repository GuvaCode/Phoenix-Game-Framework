unit uModel.Groups;

interface

uses
 Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,

  phxModel;


type
  TFrmModelGroups = class(TFrame)
    lwGroups: TListView;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    edName: TEdit;
    Label1: TLabel;
    edVertexCount: TEdit;
    Label2: TLabel;
    edVertexOffset: TEdit;
    procedure lwGroupsClick(Sender: TObject);
    procedure lwGroupsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure edNameChange(Sender: TObject);
  private
    FMesh: TPHXMesh;
    FSelected: Integer;


    procedure EnableControls(Enabled: Boolean);

    procedure SetMesh(const Value: TPHXMesh);
    procedure SetSelected(const Value: Integer);
  public

    Procedure lwGroupsUpdate;
    constructor Create(AOwner: TComponent); override;

    Property Mesh: TPHXMesh read FMesh write SetMesh;

    Property Selected: Integer read FSelected write SetSelected;
  end;

implementation

uses uActions;

{$R *.dfm}

// TFrmGroups
//------------------------------------------------------------------------------
constructor TFrmModelGroups.Create(AOwner: TComponent);
begin
  inherited;

end;

// TFrmGroups
//------------------------------------------------------------------------------
procedure TFrmModelGroups.SetMesh(const Value: TPHXMesh);
begin
  if FMesh <> Value then
  begin
    FMesh := Value;

    SetSelected(-1);
  end;

  if Assigned(FMesh) then
  begin
    lwGroupsUpdate;

    lwGroups.Color:= clWindow;
  end else
  begin
    lwGroups.Color:= clBtnFace;

    EnableControls(False);
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmModelGroups.lwGroupsUpdate;
var Index: Integer;
var Group: TPHXMeshGroup;
var Item : TListItem;
begin
  lwGroups.Items.BeginUpdate;
  lwGroups.Items.Clear;

  for Index := 0 to Mesh.Groups.Count - 1 do
  begin
    Group:= Mesh.Groups.List^[Index];

    Item:= lwGroups.Items.Add;
    Item.Caption:= Group.Name;
  end;
  lwGroups.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmModelGroups.lwGroupsClick(Sender: TObject);
begin
  SetSelected(lwGroups.ItemIndex);
end;

//------------------------------------------------------------------------------
procedure TFrmModelGroups.lwGroupsKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
  SetSelected(lwGroups.ItemIndex);
end;


//------------------------------------------------------------------------------
procedure TFrmModelGroups.SetSelected(const Value: Integer);
begin
  FSelected := Value;

  if Assigned(FMesh) and (FSelected >= 0) and (FSelected < FMesh.Groups.Count) then
  begin
    EnableControls(True);

    edName.Text:= Mesh.Groups.List^[Selected].Name;

    edVertexOffset.Text:= Format('%d', [Mesh.Groups.List^[Selected].TriangleOffset]);
    edVertexCount .Text:= Format('%d', [Mesh.Groups.List^[Selected].TriangleCount]);
  end else
  begin
    EnableControls(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelGroups.EnableControls(Enabled: Boolean);
const EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName.Enabled:= Enabled;
  edName.Color  := EnabledColors[Enabled];

  edVertexOffset.Enabled:= Enabled;
  edVertexCount .Enabled:= Enabled;
end;


//------------------------------------------------------------------------------
procedure TFrmModelGroups.edNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edName.Text;

  if Mesh.Groups.List^[Selected].Name <> Value then
  begin
    Mesh.Groups.List^[Selected].Name:= ShortString(Value);

    ModActions.Document.Changed;
  end;
end;




end.
