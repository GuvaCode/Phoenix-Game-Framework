unit uControls;

interface

uses SysUtils, Classes, Types,

  Generics.Collections,

  Xml.XmlDoc, Xml.XmlIntf,

  phxSimpleGui,

  uProject;

type


{$REGION 'TGuiWindow'}

//------------------------------------------------------------------------------
TGuiWindow = class(TGuiControl)
  private
  protected
    procedure LoadControl(Node: IXMLNode); override;
    procedure SaveControl(Node: IXMLNode); override;
  public
    constructor Create(AProject: TGuiProject); override;

    // Compile the control to code
    procedure Compile(const Output: TStringBuilder); override;

    // Render the control
    procedure Render(Gui: TPHXSimpleGUI); override;
  end;

{$ENDREGION}

{$REGION 'TGuiText'}

//------------------------------------------------------------------------------
TGuiText = class(TGuiControl)
  private
    FText: String;
  protected
    procedure LoadControl(Node: IXMLNode); override;
    procedure SaveControl(Node: IXMLNode); override;
  public
    constructor Create(AProject: TGuiProject); override;

    // Compile the control to code
    procedure Compile(const Output: TStringBuilder); override;

    // Render the control
    procedure Render(Gui: TPHXSimpleGUI); override;
  published
    property Text: String read FText write FText;
  end;

{$ENDREGION}

{$REGION 'TGuiButton'}

//------------------------------------------------------------------------------
TGuiButton = class(TGuiControl)
  private
    FText: String;
    FEvent: String;
  protected
    procedure LoadControl(Node: IXMLNode); override;
    procedure SaveControl(Node: IXMLNode); override;
  public
    constructor Create(AProject: TGuiProject); override;

    // Compile the control to code
    procedure Compile(const Output: TStringBuilder); override;

    // Render the control
    procedure Render(Gui: TPHXSimpleGUI); override;
  published
    property Text: String read FText write FText;
    property Event: String read FEvent write FEvent;
  end;

{$ENDREGION}

{$REGION 'TGuiEdit'}

//------------------------------------------------------------------------------
TGuiEdit = class(TGuiControl)
  private
    FVariable: String;
  protected
    procedure LoadControl(Node: IXMLNode); override;
    procedure SaveControl(Node: IXMLNode); override;
  public
    constructor Create(AProject: TGuiProject); override;

    // Compile the control to code
    procedure Compile(const Output: TStringBuilder); override;

    // Render the control
    procedure Render(Gui: TPHXSimpleGUI); override;
  published
    property Variable: String read FVariable write FVariable;
  end;

{$ENDREGION}

implementation

const Indent = '  ';


{$REGION 'TGuiWindow'}

var WindowCounter: Integer = 1;

// TGuiWindow
//==============================================================================
constructor TGuiWindow.Create(AProject: TGuiProject);
begin
  inherited;

  Name  := Format('Window%d', [WindowCounter]);
  Width := 320;
  Height:= 240;

  Inc(WindowCounter);
end;

//------------------------------------------------------------------------------
procedure TGuiWindow.LoadControl(Node: IXMLNode);
begin
  inherited;

end;

//------------------------------------------------------------------------------
procedure TGuiWindow.SaveControl(Node: IXMLNode);
begin
  inherited;

end;

//------------------------------------------------------------------------------
procedure TGuiWindow.Compile(const Output: TStringBuilder);
begin
  Output.Append(Indent).Append(Project.Settings.Name).Append('.').Append('Window');

  Output.Append('(')
    .Append(X).Append(', ')
    .Append(Y).Append(', ')
    .Append(Width).Append(', ')
    .Append(Height)
  .Append(');');
end;

//------------------------------------------------------------------------------
procedure TGuiWindow.Render(Gui: TPHXSimpleGUI);
begin
  Gui.Window(X,Y,Width,Height);
end;

{$ENDREGION}


{$REGION 'TGuiText'}

var TextCounter: Integer = 1;

// TGuiText
//==============================================================================
constructor TGuiText.Create(AProject: TGuiProject);
begin
  inherited;

  Name  := Format('Text%d', [TextCounter]);
  Width := 120;
  Height:= 20;
  Text  := 'Text';

  Inc(TextCounter);
end;

//------------------------------------------------------------------------------
procedure TGuiText.LoadControl(Node: IXMLNode);
begin
  inherited;

  if Node.HasAttribute('text') then FText:= Node.Attributes['text'];
end;

//------------------------------------------------------------------------------
procedure TGuiText.SaveControl(Node: IXMLNode);
begin
  inherited;

  Node.Attributes['text']:= FText;
end;

//------------------------------------------------------------------------------
procedure TGuiText.Compile(const Output: TStringBuilder);
begin
  Output.Append(Indent).Append(Project.Settings.Name).Append('.').Append('Text');

  Output.Append('(')
    .Append(X).Append(', ')
    .Append(Y).Append(', ')
    .Append(Width).Append(', ')
    .Append(Height).Append(', ')
    .Append(QuotedStr(Text))
  .Append(');');
end;

//------------------------------------------------------------------------------
procedure TGuiText.Render(Gui: TPHXSimpleGUI);
begin
  Gui.Text(X, Y, Width, Height, Text);

end;

{$ENDREGION}



{$REGION 'TGuiButton'}


var ButtonCounter: Integer = 1;

// TGuiButton
//==============================================================================
constructor TGuiButton.Create(AProject: TGuiProject);
begin
  inherited;
  Name  := Format('Button%d', [ButtonCounter]);
  Width := 120;
  Height:= 40;
  Text  := 'Button';

  Inc(ButtonCounter);
end;

//------------------------------------------------------------------------------
procedure TGuiButton.LoadControl(Node: IXMLNode);
begin
  inherited;

  if Node.HasAttribute('text' ) then FText := Node.Attributes['text'];
  if Node.HasAttribute('event') then FEvent:= Node.Attributes['event'];
end;

//------------------------------------------------------------------------------
procedure TGuiButton.SaveControl(Node: IXMLNode);
begin
  inherited;

  Node.Attributes['text' ]:= FText;
  Node.Attributes['event']:= FEvent;
end;

//------------------------------------------------------------------------------
procedure TGuiButton.Compile(const Output: TStringBuilder);
begin
  if Event <> '' then
  begin
    Output.Append(Indent).Append('if ').Append(Project.Settings.Name).Append('.').Append('Button');
  end else
  begin
    Output.Append(Indent).Append(Project.Settings.Name).Append('.').Append('Button');
  end;

  Output.Append('(')
    .Append(X).Append(', ')
    .Append(Y).Append(', ')
    .Append(Width).Append(', ')
    .Append(Height).Append(', ')
    .Append(QuotedStr(Text))
  .Append(')');

  if Event <> '' then
  begin
    Output.Append(' then').AppendLine;
    Output.Append(Indent).Append('begin').AppendLine;
    Output.Append(Indent).Append(Indent).Append(Event).Append(';').AppendLine;
    Output.Append(Indent).Append('end').Append(';').AppendLine;
  end else
  begin
    Output.Append(';');
  end;

end;

//------------------------------------------------------------------------------
procedure TGuiButton.Render(Gui: TPHXSimpleGUI);
begin
  Gui.Button(X,Y, Width, Height, Text);
end;



{$ENDREGION}


{$REGION 'TGuiEdit'}


var EditCounter: Integer = 1;

// TGuiEdit
//==============================================================================
constructor TGuiEdit.Create(AProject: TGuiProject);
begin
  inherited;
  Name    := Format('Edit%d', [EditCounter]);
  Width   := 120;
  Height  := 40;
  Variable:= '';

  Inc(EditCounter);
end;

//------------------------------------------------------------------------------
procedure TGuiEdit.LoadControl(Node: IXMLNode);
begin
  inherited;

  if Node.HasAttribute('variable') then FVariable:= Node.Attributes['variable'];
end;

//------------------------------------------------------------------------------
procedure TGuiEdit.SaveControl(Node: IXMLNode);
begin
  inherited;

  Node.Attributes['variable']:= FVariable;
end;

//------------------------------------------------------------------------------
procedure TGuiEdit.Compile(const Output: TStringBuilder);
begin
  Output.Append(Indent).Append(Project.Settings.Name).Append('.').Append('Edit');

  Output.Append('(')
    .Append(X).Append(', ')
    .Append(Y).Append(', ')
    .Append(Width).Append(', ')
    .Append(Height).Append(', ')
    .Append(Variable)
  .Append(');');
end;

//------------------------------------------------------------------------------
procedure TGuiEdit.Render(Gui: TPHXSimpleGUI);
var Temp: String;
begin
  Temp:= Variable;

  Gui.Edit(X,Y, Width, Height, Temp);
end;



{$ENDREGION}

end.
