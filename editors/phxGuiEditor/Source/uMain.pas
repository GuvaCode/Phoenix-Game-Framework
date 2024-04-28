unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, Vcl.ActnList, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.ToolWin,

  uActions,
  uProject,
  uPalette,
  uDesigner,
  uInspector,
  uStructure;

type

//------------------------------------------------------------------------------
TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    Project1: TMenuItem;
    actCompile1: TMenuItem;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    File1: TMenuItem;
    StatusBar1: TStatusBar;
    actFileNew1: TMenuItem;
    actFileOpen1: TMenuItem;
    actFileSave1: TMenuItem;
    actFileSaveAs1: TMenuItem;
    actFileNew2: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Assets1: TMenuItem;
    Loadfont1: TMenuItem;
    Loadskin1: TMenuItem;
    actAssetBackground1: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToolBarStandard: TToolBar;
    ToolButton6: TToolButton;
    btnFileNew: TToolButton;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    ToolButton1: TToolButton;
    PanelStructure: TPanel;
    Splitter3: TSplitter;
  private
    FProject: TGuiProject;
    procedure SetProject(const Value: TGuiProject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Project: TGuiProject read FProject write SetProject;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

// TFrmMain
//==============================================================================
constructor TFrmMain.Create(AOwner: TComponent);
begin
  inherited;

  ModActions:= TModActions.Create(nil);

  FrmStructure:= TFrmStructure.Create(Self);
  FrmStructure.Align:= alClient;
  FrmStructure.Parent:= PanelStructure;

  FrmInspector:= TFrmInspector.Create(Self);
  FrmInspector.Align := alClient;
  FrmInspector.Parent:= PanelLeft;

  FrmPalette:= TFrmPalette.Create(Self);
  FrmPalette.Align := alClient;
  FrmPalette.Parent:= PanelRight;

  FrmDesigner:= TFrmDesigner.Create(Self);
  FrmDesigner.Align:= alClient;
  FrmDesigner.Parent:= Self;

end;

//------------------------------------------------------------------------------
destructor TFrmMain.Destroy;
begin
  ModActions.Free;

  inherited;

end;
//------------------------------------------------------------------------------
procedure TFrmMain.SetProject(const Value: TGuiProject);
begin
  FProject := Value;

  if Assigned(Project) then
  begin
    if dsChanged in Project.State then
    begin
      Caption:= 'phxGuiEditor - ' + Project.Name + '*';
    end else
    begin
      Caption:= 'phxGuiEditor - ' + Project.Name;
    end;

  end else
  begin
    Caption:= 'phxGuiEditor';
  end;
end;

end.
