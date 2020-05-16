unit uActions;

interface

uses
  System.SysUtils, System.Classes, Vcl.ImgList, Vcl.Controls, Vcl.ActnList,
  Vcl.Forms, Vcl.Dialogs,

  phxGraphics,
  phxGraphics_FreeImage,

  uProject,
  uControls;

const
  ICON_FOLDER_CLOSED =  0;
  ICON_FOLDER_OPEN   =  1;
  ICON_POINTER       =  2;

  ICON_CODE_VARIABLE  =  3;
  ICON_CODE_FUNCTION  =  4;
  ICON_CODE_CLASS     =  5;

  ICON_CONTROL        =  6;
  ICON_BUTTON         =  7;
  ICON_LABEL          =  8;
  ICON_WINDOW         =  9;
  ICON_IMAGE          = 10;
  ICON_ROOT           = 11;
  ICON_EDIT           = 12;
  ICON_SCROLLBAR      = 13;
  ICON_LISTBOX        = 14;
  ICON_CHECKBOX       = 15;
  ICON_RADIOBUTTON    = 16;
  ICON_SLIDER         = 17;
  ICON_GRID           = 18;
  ICON_PAGECONTROL    = 19;
  ICON_PANEL          = 20;
  ICON_ANIMATION      = 21;
  ICON_SPINNER        = 22;
  ICON_KEYBOARD       = 23;

  ICON_LOCKED         = 24;

type
  TModActions = class(TDataModule)
    ControlImages: TImageList;
    ActionImages: TImageList;
    ActionList1: TActionList;
    actProjectCompile: TAction;
    actFileDemo: TAction;
    OpenProjectDialog: TOpenDialog;
    SaveProjectDialog: TSaveDialog;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    actAssetSkin: TAction;
    actAssetFont: TAction;
    OpenAssetDialog: TOpenDialog;
    actAssetBackground: TAction;
    procedure actProjectCompileExecute(Sender: TObject);
    procedure actFileDemoExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actAssetSkinExecute(Sender: TObject);
    procedure actAssetFontExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actProjectUpdate(Sender: TObject);
    procedure actAssetUpdate(Sender: TObject);
    procedure actAssetBackgroundExecute(Sender: TObject);
  private
    FProject: TGuiProject;
    FControl: TGuiControl;
    procedure SetProject(const Value: TGuiProject);
    procedure SetControl(const Value: TGuiControl);
  public
    constructor Create(AOwner: TComponent); override;

    function GetControlImage(const ClassName: String): Integer;

    // Current gui project
    property Project: TGuiProject read FProject write SetProject;
    // Selected gui control
    property Control: TGuiControl read FControl write SetControl;
  end;

var
  ModActions: TModActions;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  uOutput,
  uDesigner,
  uStructure,
  uInspector, uMain;

{$R *.dfm}

//------------------------------------------------------------------------------
constructor TModActions.Create(AOwner: TComponent);
begin
  inherited;

end;

//------------------------------------------------------------------------------
function TModActions.GetControlImage(const ClassName: String): Integer;
begin
  // TPHXWindow
  if SameText(ClassName, TGuiWindow.ClassName) then
  begin
    Result:= ICON_WINDOW;
  end else
  // TPHXLabel
  if SameText(ClassName, TGuiText.ClassName) then
  begin
    Result:= ICON_LABEL;
  end else
  // TPHXButton
  if SameText(ClassName, TGuiButton.ClassName) then
  begin
    Result:= ICON_BUTTON;
  end else
  // TPHXEdit
  if SameText(ClassName, TGuiEdit.ClassName) then
  begin
    Result:= ICON_EDIT;
  end else
  {// TPHXPicture
  if SameText(ClassName, TPHXPicture.ClassName) then
  begin
    Result:= ICON_IMAGE;
  end else

  // TPHXCheckbox
  if SameText(ClassName, TPHXCheckbox.ClassName) then
  begin
    Result:= ICON_CHECKBOX;
  end else
  // TPHXIcon
  if SameText(ClassName, TPHXPicture.ClassName) then
  begin
    Result:= ICON_IMAGE;
  end else
  // TPHXEdit
  if SameText(ClassName, TPHXEdit.ClassName) then
  begin
    Result:= ICON_EDIT;
  end else
  // TPHXListBox
  if SameText(ClassName, TPHXListBox.ClassName) then
  begin
    Result:= ICON_LISTBOX;
  end else
  // TPHXSlider
  if SameText(ClassName, TPHXSlider.ClassName) then
  begin
    Result:= ICON_SLIDER;
  end else

  if SameText(ClassName, TPHXScrollBar.ClassName) then
  begin
    Result:= ICON_SCROLLBAR;
  end else
  // TPHXAnimationCtrl
  if SameText(ClassName, TPHXAnimationCtrl.ClassName) then
  begin
    Result:= ICON_ANIMATION;
  end else
  // TPHXSpinner
  if SameText(ClassName, TPHXSpinner.ClassName) then
  begin
    Result:= ICON_SPINNER;
  end else
  // TPHXKeyBinder
  if SameText(ClassName, TPHXKeyBinder.ClassName) then
  begin
    Result:= ICON_KEYBOARD;
  end else
 //   ICON_RADIOBUTTON   = 13;
//  ICON_SLIDER        = 14;
                 }
  // Other control
  begin
    Result:= ICON_CONTROL;
  end;
end;

{$REGION 'File Actions'}

var ProjectCounter : Integer = 1;

procedure TModActions.actFileUpdate(Sender: TObject);
begin
  actFileSave.Enabled:= Assigned(Project);
  actFileSaveAs.Enabled:= Assigned(Project);
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileNewExecute(Sender: TObject);
var Project: TGuiProject;
begin
  Project:= TGuiProject.Create;
  Project.Name:= Format('Project%d', [ProjectCounter]);
  Project.State:= [dsNew];

  SetProject(Project);

  Inc(ProjectCounter);
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileOpenExecute(Sender: TObject);
var Project: TGuiProject;
begin
  if OpenProjectDialog.Execute then
  begin
    Project:= TGuiProject.Create;
    Project.Name := OpenProjectDialog.FileName;
    Project.State:= [];

    Project.LoadProject;

    SetProject(Project);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveExecute(Sender: TObject);
begin
  if dsNew in Project.State then
  begin
    actFileSaveAs.Execute;
  end else
  begin
    Project.SaveProject;
    Project.State:= [];
  end;

end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveAsExecute(Sender: TObject);
begin
  if SaveProjectDialog.Execute then
  begin
    Project.Name := SaveProjectDialog.FileName;
    Project.State:= [];

    Project.SaveProject;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileDemoExecute(Sender: TObject);
var Project: TGuiProject;
var Control: TGuiControl;
begin
  Project:= TGuiProject.Create;

  Control:= TGuiWindow.Create(Project);

  Project.Controls.Add(Control);

  Control:= TGuiButton.Create(Project);
  Control.X:= 100;
  Control.Y:= 50;
  Control.Width:= 200;
  Control.Height:= 25;

  Project.Controls.Add(Control);

  Control:= TGuiButton.Create(Project);
  Control.X:= 400;
  Control.Y:= 50;
  Control.Width:= 200;
  Control.Height:= 25;
  Project.Controls.Add(Control);

  SetProject(Project);
end;

{$ENDREGION}


{$REGION 'Project Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actProjectUpdate(Sender: TObject);
begin
  actProjectCompile.Enabled:= Assigned(Project);
end;

//------------------------------------------------------------------------------
procedure TModActions.actProjectCompileExecute(Sender: TObject);
begin
  Project.Compile(FrmOutput.SynEdit1.Lines);

  FrmOutput.Show;
end;


{$ENDREGION}

{$REGION 'Asset Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actAssetUpdate(Sender: TObject);
begin
  actAssetBackground.Enabled:= Assigned(Project);
  actAssetSkin      .Enabled:= Assigned(Project);
  actAssetFont      .Enabled:= Assigned(Project);
end;

//------------------------------------------------------------------------------
procedure TModActions.actAssetBackgroundExecute(Sender: TObject);
var Path: String;
begin
  Path:= ExtractFilePath(Project.Name);

  OpenAssetDialog.InitialDir:= Path;
  OpenAssetDialog.Filter:= GraphicFormats.Filter;

  OpenAssetDialog.FileName:= ExtractFileName(Project.Settings.Background);

  if OpenAssetDialog.Execute then
  begin
    Project.Settings.Background:= ExtractRelativePath(Path, OpenAssetDialog.FileName);

    SetProject(Project);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actAssetSkinExecute(Sender: TObject);
var Path: String;
begin
  Path:= ExtractFilePath(Project.Name);

  OpenAssetDialog.InitialDir:= Path;
  OpenAssetDialog.Filter:= 'Phoenix Skin Files (*.phxskn)|*.phxskn|All Files (*.*)|*.*';

  OpenAssetDialog.FileName:= ExtractFileName(Project.Settings.Skin);

  if OpenAssetDialog.Execute then
  begin
    Project.Settings.Skin:= ExtractRelativePath(Path, OpenAssetDialog.FileName);

    SetProject(Project);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actAssetFontExecute(Sender: TObject);
var Path: String;
begin
  Path:= ExtractFilePath(Project.Name);

  OpenAssetDialog.InitialDir:= Path;
  OpenAssetDialog.Filter:= 'Phoenix Font Files (*.phxfnt)|*.phxfnt|All Files (*.*)|*.*';

  OpenAssetDialog.FileName:= ExtractFileName(Project.Settings.Font);
  if OpenAssetDialog.Execute then
  begin
    Project.Settings.Font:= ExtractRelativePath(Path, OpenAssetDialog.FileName);

    SetProject(Project);
  end;
end;


{$ENDREGION}


//------------------------------------------------------------------------------
procedure TModActions.SetProject(const Value: TGuiProject);
begin
  if FProject <> Value then
  begin
    SetControl(nil);

    FrmInspector.Project:= nil;
    FrmDesigner .Project:= nil;
    FrmStructure.Project:= nil;

    if Assigned(FProject) then
    begin
      FProject.Free;
    end;

    FProject := Value;
  end;

  FrmMain     .Project:= Project;
  FrmInspector.Project:= Project;
  FrmDesigner .Project:= Project;
  FrmStructure.Project:= Project;
end;

//------------------------------------------------------------------------------
procedure TModActions.SetControl(const Value: TGuiControl);
begin
  FControl := Value;

  FrmInspector.Control:= FControl;
  FrmDesigner .Control:= FControl;
  FrmStructure.Control:= FControl;
end;

end.
