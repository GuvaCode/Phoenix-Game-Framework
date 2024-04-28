unit uSettings;

interface

uses
 Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
 Dialogs, StdCtrls, ExtCtrls,
 ColorBox,
 Spin,

  phxEditor,

  uActions;


type
  TFrmSettings = class(TForm)
    Panel2: TPanel;
    Panel1: TPanel;
    btnOk: TButton;
    Button4: TButton;
    cbCenterPivots: TCheckBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label11: TLabel;
    Label1: TLabel;
    cbSnapToGrid: TCheckBox;
    cbShowGrid: TCheckBox;
    edGridColor: TColorBox;
    edGridStyle: TComboBox;
    Label2: TLabel;
    edGridSizeY: TSpinEdit;
    edGridSizeX: TSpinEdit;
  private
    FSettings: TSettings;

    Procedure FormToSettings;
    Procedure SettingsToForm;

    procedure SetSettings(const Value: TSettings);
  public
    function Execute(Settings: TSettings): Boolean;

    property Settings: TSettings read FSettings write SetSettings;
  end;

var
  FrmSettings: TFrmSettings;

implementation

{$R *.dfm}

// TFrmSettings
//------------------------------------------------------------------------------
function TFrmSettings.Execute(Settings: TSettings): Boolean;
begin
  SetSettings(Settings);

  if ShowModal = mrOk then
  begin
    FormToSettings;

    Result:= True;
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSettings.FormToSettings;
begin
  Settings.CenterPivots:= cbCenterPivots.Checked;

  Settings.Grid.Visible:=       cbShowGrid  .Checked;
  Settings.Grid.Enabled:=       cbSnapToGrid.Checked;
  Settings.Grid.Width  := Round(edGridSizeX.Value);
  Settings.Grid.Height := Round(edGridSizeY.Value);
  Settings.Grid.Color  :=               edGridColor.Selected;
  Settings.Grid.Style  := TPHXGridStyle(edGridStyle.ItemIndex);
end;

//------------------------------------------------------------------------------
procedure TFrmSettings.SettingsToForm;
begin
  cbCenterPivots.Checked:= Settings.CenterPivots;

  cbShowGrid  .Checked :=     Settings.Grid.Visible;
  cbSnapToGrid.Checked :=     Settings.Grid.Enabled;
  edGridSizeX.Value    :=     Settings.Grid.Width;
  edGridSizeY.Value    :=     Settings.Grid.Height;
  edGridColor.Selected :=     Settings.Grid.Color;
  edGridStyle.ItemIndex:= Ord(Settings.Grid.Style);
end;

//------------------------------------------------------------------------------
procedure TFrmSettings.SetSettings(const Value: TSettings);
begin
  FSettings := Value;

  SettingsToForm;
end;



end.
