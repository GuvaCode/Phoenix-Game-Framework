unit uModel.Properties;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin,

  phxModel;

type

//------------------------------------------------------------------------------
TFrmModelProperties = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edName: TEdit;
    edAuthor: TEdit;
    edDescription: TMemo;
    edVersion: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edMinX: TSpinEdit;
    edMaxX: TSpinEdit;
    edMinY: TSpinEdit;
    edMaxY: TSpinEdit;
    edMinZ: TSpinEdit;
    edMaxZ: TSpinEdit;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    edVertexCount: TEdit;
    edTriangleCount: TEdit;
    procedure edNameChange(Sender: TObject);
    procedure edAuthorChange(Sender: TObject);
    procedure edVersionChange(Sender: TObject);
    procedure edDescriptionChange(Sender: TObject);
  private
    FMesh: TPHXMesh;

    procedure EnableControls(Enabled: Boolean);

    procedure SetMesh(const Value: TPHXMesh);
  public
    Property Mesh: TPHXMesh read FMesh write SetMesh;
  end;

implementation

{$R *.dfm}

uses uActions;

// TFrmModelProperties
//==============================================================================
procedure TFrmModelProperties.SetMesh(const Value: TPHXMesh);
begin
  FMesh := Value;

  if Assigned(FMesh) then
  begin
    EnableControls(True);

    edName       .Text:= Mesh.Name;
    edAuthor     .Text:= Mesh.Author;
    edVersion    .Text:= Mesh.Version;
    edDescription.Text:= Mesh.Comment;

    edMinX.Value:= Mesh.Bounds.MinX;
    edMaxX.Value:= Mesh.Bounds.MaxX;

    edMinY.Value:= Mesh.Bounds.MinY;
    edMaxY.Value:= Mesh.Bounds.MaxY;

    edMinZ.Value:= Mesh.Bounds.MinZ;
    edMaxZ.Value:= Mesh.Bounds.MaxZ;

    edVertexCount  .Text:=  IntToStr(Mesh.Vertices.Count);
    edTriangleCount.Text:=  IntToStr(Mesh.Triangles.Count);
  end else
  begin
    EnableControls(False);
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmModelProperties.EnableControls(Enabled: Boolean);
const EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName       .Color:= EnabledColors[Enabled];
  edAuthor     .Color:= EnabledColors[Enabled];
  edVersion    .Color:= EnabledColors[Enabled];
  edDescription.Color:= EnabledColors[Enabled];

  edMinX.Color:= EnabledColors[Enabled];
  edMaxX.Color:= EnabledColors[Enabled];
  edMinY.Color:= EnabledColors[Enabled];
  edMaxY.Color:= EnabledColors[Enabled];
  edMinZ.Color:= EnabledColors[Enabled];
  edMaxZ.Color:= EnabledColors[Enabled];



  edName       .Enabled:= Enabled;
  edAuthor     .Enabled:= Enabled;
  edVersion    .Enabled:= Enabled;
  edDescription.Enabled:= Enabled;

  edMinX.Enabled:= Enabled;
  edMaxX.Enabled:= Enabled;
  edMinY.Enabled:= Enabled;
  edMaxY.Enabled:= Enabled;
  edMinZ.Enabled:= Enabled;
  edMaxZ.Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmModelProperties.edNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edName.Text;

  if Mesh.Name <> Value then
  begin
    Mesh.Name:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelProperties.edAuthorChange(Sender: TObject);
var Value: String;
begin
  Value:= edAuthor.Text;

  if Mesh.Author <> Value then
  begin
    Mesh.Author:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelProperties.edVersionChange(Sender: TObject);
var Value: String;
begin
  Value:= edVersion.Text;

  if Mesh.Version <> Value then
  begin
    Mesh.Version:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmModelProperties.edDescriptionChange(Sender: TObject);
var Value: String;
begin
  Value:= edDescription.Text;

  if Mesh.Comment <> Value then
  begin
    Mesh.Comment:= Value;

    ModActions.Document.Changed;
  end;
end;



end.
