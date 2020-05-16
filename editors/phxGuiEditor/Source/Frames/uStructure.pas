unit uStructure;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  uActions,
  uProject;

type
  TFrmStructure = class(TFrame)
    twStructure: TTreeView;
    procedure twStructureChange(Sender: TObject; Node: TTreeNode);
  private
    FProject: TGuiProject;
    FControl: TGuiControl;

    procedure twStructureUpdate;

    procedure SetProject(const Value: TGuiProject);
    procedure SetControl(const Value: TGuiControl);
  public
    constructor Create(AOwner: TComponent); override;

    property Project: TGuiProject read FProject write SetProject;
    property Control: TGuiControl read FControl write SetControl;
  end;

var FrmStructure: TFrmStructure;

implementation

{$R *.dfm}

// TFrmStructure
//==============================================================================
constructor TFrmStructure.Create(AOwner: TComponent);
begin
  inherited;

  SetProject(nil);
end;


//------------------------------------------------------------------------------
procedure TFrmStructure.twStructureUpdate;
var Index: Integer;
var Control: TGuiControl;
var Node   : TTreeNode;
var Image  : Integer;
begin
  twStructure.Items.BeginUpdate;
     (*
  if twStructure.Items.Count <> Project.Controls.Count then
  begin
    twStructure.Items.Clear;

    for Index := 0 to Project.Controls.Count-1 do
    begin
      twStructure.Items.AddChild(nil, '');
    end;
  end;
  *)

  twStructure.Items.Clear;

  for Index := 0 to Project.Controls.Count-1 do
  begin
    Control:=  Project.Controls[Index];

    Image:= ModActions.GetControlImage(Control.ClassName);

    Node:= twStructure.Items.AddChild(nil, Control.Name);
    Node.Data:= Control;

    Node.StateIndex   := Image;
    Node.ImageIndex   := Image;
    Node.SelectedIndex:= Image;

    if Control = FControl then
    begin
      Node.Selected:=  True;
    end;
  end;

  twStructure.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmStructure.twStructureChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(twStructure.Selected) then
  begin
    ModActions.Control:= TGuiControl(twStructure.Selected.Data);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmStructure.SetProject(const Value: TGuiProject);
begin
  FProject := Value;

  if Assigned(Project) then
  begin
    twStructureUpdate;

    twStructure.Enabled:= True;
    twStructure.Color  := clWindow;
  end else
  begin
    twStructure.Items.Clear;

    twStructure.Enabled:= False;
    twStructure.Color  := clBtnFace;
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmStructure.SetControl(const Value: TGuiControl);
begin
  FControl := Value;
   (*
  if Assigned(Project) then
  begin
    twStructureUpdate;
  end else
  begin
    twStructure.Items.Clear;
  end;
  *)
end;



end.
