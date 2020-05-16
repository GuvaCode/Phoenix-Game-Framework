unit uProject;

interface

uses SysUtils, Classes, Types,

  Generics.Collections,

  Xml.XmlDoc, Xml.XmlIntf,

  phxSimpleGui;

type

// Forward
TGuiProject = class;
TGuiControl = class;

//------------------------------------------------------------------------------
TGuiState = set of (
  dsNew,
  dsChanged
);

TGuiControlClass = class of TGuiControl;

//------------------------------------------------------------------------------
TGuiControl = class(TPersistent)
  private
    FProject: TGuiProject;
    FName: String;
    FX: Integer;
    FY: Integer;
    FWidth: Integer;
    FHeight: Integer;
    function GetControlRect: TRect;
  protected
    procedure LoadControl(Node: IXMLNode); virtual;
    procedure SaveControl(Node: IXMLNode); virtual;
  public
    constructor Create(AProject: TGuiProject); virtual;

    // Compile the control to pascal source
    procedure Compile(const Output: TStringBuilder); virtual; abstract;
    // Render the control
    procedure Render(Gui: TPHXSimpleGUI); virtual; abstract;

    // The owning gui project
    property Project: TGuiProject read FProject;
    // Get the
    property ControlRect: TRect read GetControlRect;
  published
    // Name of the control
    property Name: String read FName write FName;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
  end;



{$REGION 'TGuiSettings'}

//------------------------------------------------------------------------------
TGuiSettings = class
  private
    FName: String;
    FBackground: String;
    FSkin: String;
    FFont: String;
  protected
    procedure LoadSettings(Node: IXMLNode); overload;
    procedure SaveSettings(Node: IXMLNode); overload;
  public
    constructor Create(AProject: TGuiProject);

    // Name of the gui variable
    property Name: String read FName write FName;
    // Name of the background texture
    property Background: String read FBackground write FBackground;
    // Name of the gui skin file
    property Skin: String read FSkin write FSkin;
    // Name of the gui font file
    property Font: String read FFont write FFont;
  end;

{$ENDREGION}

{$REGION 'TGuiProject'}

//------------------------------------------------------------------------------
TGuiProject = class
  private
    FName: String;
    FState: TGuiState;
    FControls: TList<TGuiControl>;
    FSettings: TGuiSettings;
  protected
    procedure LoadProject(Root: IXMLNode); overload;
    procedure SaveProject(Root: IXMLNode); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadProject; overload;
    procedure SaveProject; overload;

    procedure Compile(const Output: TStringBuilder); overload;
    procedure Compile(const Lines: TStrings); overload;

    // Render the gui
    procedure Render(Gui: TPHXSimpleGUI);

    // get the control at a position
    function ControlAt(const X,Y: Integer): TGuiControl;

    // Mark the project as changed
    procedure Changed;

    // Name of the project
    property Name: String read FName write FName;
    // The project state
    property State: TGuiState read FState write FState;
    // Settings for the gui
    property Settings: TGuiSettings read FSettings;
    // List of gui controls
    property Controls: TList<TGuiControl> read FControls;
  end;

{$ENDREGION}

function GetControlClass(const Control: String): TGuiControlClass;
function GetControlName(const Control: TGuiControl): String;


implementation

uses
  uActions,
  uControls;

//------------------------------------------------------------------------------
function GetControlName(const Control: TGuiControl): String;
begin
  if Control is TGuiWindow then
  begin
    Result:= 'window';
  end else
  if Control is TGuiText then
  begin
    Result:= 'text';
  end else
  if Control is TGuiButton then
  begin
    Result:= 'button';
  end else
  if Control is TGuiEdit then
  begin
    Result:= 'edit';
  end else
  begin
    raise Exception.Create('Error Message');
  end;
end;

//------------------------------------------------------------------------------
function GetControlClass(const Control: String): TGuiControlClass;
begin
  if Control = 'window' then
  begin
    Result:= TGuiWindow;
  end else
  if Control = 'text' then
  begin
    Result:= TGuiText;
  end else

  if Control = 'button' then
  begin
    Result:= TGuiButton;
  end else
  if Control = 'edit' then
  begin
    Result:= TGuiEdit;
  end else
  begin
    raise Exception.Create('Error Message');
  end;
end;



{$REGION 'TGuiControl'}

// TGuiControl
//==============================================================================
constructor TGuiControl.Create(AProject: TGuiProject);
begin
  FProject:= AProject;
end;

//------------------------------------------------------------------------------
function TGuiControl.GetControlRect: TRect;
begin
  Result.Left:= X;
  Result.Right:= X + Width;
  Result.Top:= Y;
  Result.Bottom:= Y + Height;
end;

//------------------------------------------------------------------------------
procedure TGuiControl.LoadControl(Node: IXMLNode);
begin
  if Node.HasAttribute('name')   then FName  := Node.Attributes['name'];
  if Node.HasAttribute('x')      then FX     := Node.Attributes['x'];
  if Node.HasAttribute('y')      then FY     := Node.Attributes['y'];
  if Node.HasAttribute('width')  then FWidth := Node.Attributes['width'];
  if Node.HasAttribute('height') then FHeight:= Node.Attributes['height'];

end;

//------------------------------------------------------------------------------
procedure TGuiControl.SaveControl(Node: IXMLNode);
begin
  Node.Attributes['name'  ] := FName;
  Node.Attributes['x'     ] := FX;
  Node.Attributes['y'     ] := FY;
  Node.Attributes['width' ] := FWidth;
  Node.Attributes['height'] := FHeight;
end;

{$ENDREGION}




{$REGION 'TGuiSettings'}

// TGuiSettings
//==============================================================================
constructor TGuiSettings.Create(AProject: TGuiProject);
begin
  FName:= 'Gui';
  FSkin:= '';
end;

//------------------------------------------------------------------------------
procedure TGuiSettings.LoadSettings(Node: IXMLNode);
begin
  if Node.HasAttribute('name'      ) then FName      := Node.Attributes['name'];
  if Node.HasAttribute('background') then FBackground:= Node.Attributes['background'];
  if Node.HasAttribute('skin'      ) then FSkin      := Node.Attributes['skin'];
  if Node.HasAttribute('font'      ) then FFont      := Node.Attributes['font'];
end;

//------------------------------------------------------------------------------
procedure TGuiSettings.SaveSettings(Node: IXMLNode);
begin
  Node.Attributes['name'      ]:= FName;
  Node.Attributes['background']:= FBackground;
  Node.Attributes['skin'      ]:= FSkin;
  Node.Attributes['font'      ]:= FFont;
end;

{$ENDREGION}

{$REGION 'TGuiProject'}

// TGuiProject
//==============================================================================
constructor TGuiProject.Create;
begin
  FControls:= TObjectList<TGuiControl>.Create;
  FName    := '';
  FState   := [];
  FSettings:= TGuiSettings.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TGuiProject.Destroy;
begin
  FControls.Free;
  FSettings.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TGuiProject.LoadProject(Root: IXMLNode);
var Index  : Integer;
var Node   : IXMLNode;
var Control: TGuiControl;
begin
  Settings.LoadSettings(Root);

  for Index := 0 to Root.ChildNodes.Count-1 do
  begin
    Node:= Root.ChildNodes[Index];

    Control:= GetControlClass(Node.NodeName).Create(Self);
    Control.LoadControl(Node);

    Controls.Add(Control);
  end;
end;

//------------------------------------------------------------------------------
procedure TGuiProject.SaveProject(Root: IXMLNode);
var Index  : Integer;
var Node   : IXMLNode;
var Control: TGuiControl;
begin
  Settings.SaveSettings(Root);

  for Index := 0 to Controls.Count-1 do
  begin
    Control:=Controls[Index];

    Node:= Root.AddChild( GetControlName(Control) );

    Control.SaveControl(Node);
  end;
end;

//------------------------------------------------------------------------------
procedure TGuiProject.LoadProject;
var Document: IXMLDocument;
var Root: IXMLNode;
begin
  Document:= LoadXMLDocument(Name);

  Root:= Document.DocumentElement;

  if Root.NodeName = 'project' then
  begin
    LoadProject(Root);

    State:= [];
  end;

end;

//------------------------------------------------------------------------------
procedure TGuiProject.SaveProject;
var Document: IXMLDocument;
var Root: IXMLNode;
begin
  Document:= NewXMLDocument;
  Document.Options:= Document.Options + [doNodeAutoIndent];

  Root:= Document.AddChild('project');

  SaveProject(Root);

  Document.SaveToFile(Name);

  State:= [];
end;




//------------------------------------------------------------------------------
procedure TGuiProject.Compile(const Lines: TStrings);
var Output: TStringBuilder;
begin
  Output:= TStringBuilder.Create;
  try
    Compile(Output);

    Lines.Text:= Output.ToString;
  finally
    Output.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TGuiProject.Compile(const Output: TStringBuilder);
var Control: TGuiControl;
begin
  Output.Append(Settings.Name).Append('.Prepare;').AppendLine;
  for Control in Controls do
  begin
    Control.Compile(Output);

    Output.AppendLine;
  end;
  Output.Append(Settings.Name).Append('.Finish;').AppendLine;
end;


//------------------------------------------------------------------------------
procedure TGuiProject.Render(Gui: TPHXSimpleGUI);
var Control: TGuiControl;
begin
  Gui.Prepare;
  for Control in Controls do
  begin
    Control.Render(Gui);
  end;
  Gui.Finish;
end;


//------------------------------------------------------------------------------
function TGuiProject.ControlAt(const X, Y: Integer): TGuiControl;
var Index: Integer;
var Control: TGuiControl;
begin
  for Index := Controls.Count-1 downto 0 do
  begin
    Control:= Controls[Index];

    if PtInRect(Control.ControlRect, Point(X,Y)) then
    begin
      Result:= Control;
      Exit;
    end;
  end;
  Result:= nil;
end;

{$ENDREGION}


//------------------------------------------------------------------------------
procedure TGuiProject.Changed;
begin
  Include(FState, dsChanged);

  ModActions.Project:=  ModActions.Project;
end;


end.
