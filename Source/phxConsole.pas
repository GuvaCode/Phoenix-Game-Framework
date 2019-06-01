////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxConsole;
//< Implements an in-game console for adding commands and showing debug information

interface

{$I phxConfig.inc}

Uses Classes, SysUtils,

  phxDevice,
  phxTypes,
  phxCanvas,
  phxEvents,
  phxFont;

const

MAX_HISTORY_ITEMS = 100;

type

TPHXConsole          = class;
TPHXConsoleItem      = class;
TPHXConsoleParameter = class;

// State for the console
//------------------------------------------------------------------------------
TPHXConsoleState = (
  // The console is closed
  csClosed,
  // The console is opening
  csShowing,
  // The console is opened
  csOpen,
  // The console is hiding
  csHiding
);

{$REGION 'TPHXConsoleParameter'}

// A parameter for the console
//------------------------------------------------------------------------------
TPHXConsoleParameter = class
  private
    FText: String;

    function GetInteger: Integer;
    function GetFloat: Single;
    function GetBoolean: Boolean;
  public
    constructor Create;

    property Text: String read FText write FText;

    // Return the value as a string
    property AsString : String  read FText;
    // Return the value of the parameter as a integer
    property AsInteger: Integer read GetInteger;
    // Return the value of the parameter as a float
    property AsFloat: Single  read GetFloat;
    // Return the value of the parameter as a boolean
    property AsBoolean: Boolean read GetBoolean;
  end;

// List of parameters for the console
//------------------------------------------------------------------------------
TPHXConsoleParameters = class
  private
    FItems: TList;
    FText : String;

    // Parses the parameters from the text
    procedure Parse;

    function GetCount: Integer;
    function GetParameter(Index: Integer): TPHXConsoleParameter;

    procedure SetText(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    // Remove all parameters
    procedure Clear;
    // Add a new parameter
    function Add(const Parameter: TPHXConsoleParameter): TPHXConsoleParameter; overload;
    // Add a new parameter
    function Add(const Parameter: String): TPHXConsoleParameter; overload;

    property Text: String read FText write SetText;
    // Returns the number of parameters
    property Count: Integer read GetCount;
    // Return a parameter
    property Parameters[Index: Integer]: TPHXConsoleParameter read GetParameter; default;
  end;

{$ENDREGION}

{$REGION 'TPHXConsoleItem'}

// Abstract console item
//------------------------------------------------------------------------------
TPHXConsoleItem = class
  private
    FConsole: TPHXConsole;
    FName   : String;
  public
    // Default constructor
    constructor Create(AConsole: TPHXConsole);

    // Execute the console item
    procedure Execute(Parameters: TPHXConsoleParameters); virtual; abstract;

    // The owning console
    property Console: TPHXConsole read FConsole write FConsole;
    // Name of the console item
    property Name: String read FName write FName;
  end;

// Console item containing a integer variable
//------------------------------------------------------------------------------
TPHXConsoleVariableString = class(TPHXConsoleItem)
  private
    FVariable: PString;
  public
    // Execute the console item, reads or writes the value of the variable
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    property Variable: PString read FVariable write FVariable;
  end;

// Console item containing a integer variable
//------------------------------------------------------------------------------
TPHXConsoleVariable1i = class(TPHXConsoleItem)
  private
    FVariable : PInteger;
  public
    // Execute the console item, reads or writes the value of the variable
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    property Variable: PInteger read FVariable write FVariable;
  end;

// Console item containing a boolean variable
//------------------------------------------------------------------------------
TPHXConsoleVariable1b = class(TPHXConsoleItem)
  private
    FVariable : PBoolean;
  public
    // Execute the console item, reads or writes the value of the variable
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    property Variable: PBoolean read FVariable write FVariable;
  end;

// Console item containing a TVector4 variable
//------------------------------------------------------------------------------
TPHXConsoleVariable4f = class(TPHXConsoleItem)
  private
    FVariable : PVector4f;
  public
    // Execute the console item, reads or writes the value of the variable
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    property Variable: PVector4f read FVariable write FVariable;
  end;

// A console item containing a TColor3f variable
//------------------------------------------------------------------------------
TPHXConsoleColor3f = class(TPHXConsoleItem)
  private
    FColor : PColor3f;
  public
    constructor Create(AConsole: TPHXConsole; AColor: PColor3f);

    // Execute the console item, reads or writes the value of the color
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    // The pointer to the color variable
    property Color: PColor3f read FColor;
  end;

// A console item containing a TColor4f variable
//------------------------------------------------------------------------------
TPHXConsoleColor4f = class(TPHXConsoleItem)
  private
    FColor : PColor4f;
  public
    constructor Create(AConsole: TPHXConsole; AColor: PColor4f);

    // Execute the console item, reads or writes the value of the color
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    // The pointer to the color variable
    property Color: PColor4f read FColor;
  end;

// Event for the console function item
TPHXConsoleCallback = procedure(Console: TPHXConsole; Parameters: TPHXConsoleParameters) of object;

// A console item containing a function call
//------------------------------------------------------------------------------
TPHXConsoleFunction = class(TPHXConsoleItem)
  private
    FCallback: TPHXConsoleCallback;
  public
    constructor Create(AConsole: TPHXConsole; const AName: String; const ACallback: TPHXConsoleCallback);

    // Execute the function event
    procedure Execute(Parameters: TPHXConsoleParameters); override;

    // The function callback
    property Callback: TPHXConsoleCallback read FCallback;
  end;

{$ENDREGION}

{$REGION 'TPHXConsoleItems'}

PConsoleItemList = ^TConsoleItemList;
TConsoleItemList = array[0..$00FFFFFF] of TPHXConsoleItem;

// Container for a list of console items
//------------------------------------------------------------------------------
TPHXConsoleItems = class
  private
    FOwner: TPHXConsole;
    FList: TList;

    function GetCount: Integer;
    function GetList: PConsoleItemList;
    function GetItem(const Index: Integer): TPHXConsoleItem;
  public
    constructor Create(AOwner: TPHXConsole);
    destructor Destroy; override;

    // Remove and free all console items
    procedure Clear;

    // Add a integer variable
    function Add(const Name: String; var Value: String): TPHXConsoleVariableString; overload;
    // Add a integer variable
    function Add(const Name: String; var Value: Integer): TPHXConsoleVariable1i; overload;
    // Add a boolean variable
    function Add(const Name: String; var Value: Boolean): TPHXConsoleVariable1b; overload;
    // Add a TVector4f variable
    function Add(const Name: String; var Value: TVector4f): TPHXConsoleVariable4f; overload;
    // Add a TColor3f variable
    function Add(const Name: String; var Value: TColor3f): TPHXConsoleColor3f; overload;
    // Add a TColor4f variable
    function Add(const Name: String; var Value: TColor4f): TPHXConsoleColor4f; overload;
    // Add a function item
    function Add(const Name: String; Value: TPHXConsoleCallback): TPHXConsoleFunction; overload;

    // Return the index of a console variable
    function IndexOf(const Name: String): Integer;

    // The owning console
    property Owner: TPHXConsole read FOwner;
    // The number of console items
    property Count: Integer read GetCount;
    // Pointer to the internal list
    property List: PConsoleItemList read GetList;
    // Return a single item from the list
    property Items[const Index: Integer]: TPHXConsoleItem read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXConsoleLine'}

// A single line in the console
//------------------------------------------------------------------------------
TPHXConsoleLine = record
  public
    // Time the item was added to the console
    Time: Double;
    // Color of the item
    Color: TColor4f;
    // Text of the item
    Text: String;
  public
    class function Create(const ATime: Double; const  AColor: TColor4f; const AText: String): TPHXConsoleLine; static;
  end;

PConsoleLineList = ^TConsoleLineList;
TConsoleLineList = array[0..$00FFFFFF] of TPHXConsoleLine;

// Lines in the console
//------------------------------------------------------------------------------
TPHXConsoleLines = class
  private
    FConsole: TPHXConsole;
    FCapacity: Integer;
    FCount   : Integer;
    FList    : PConsoleLineList;

    procedure Grow;

    function GetItem(const Index: Integer): TPHXConsoleLine;

    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
  public
    constructor Create(AConsole: TPHXConsole);
    destructor Destroy; override;
    // Clear all lines
    procedure Clear;

    // Add a new line
    procedure Add(const Line: TPHXConsoleLine); overload;
    // Add a new line
    procedure Add(const Text: String); overload;
    // Add a new line with a specific color
    procedure Add(const Text: String; const Color: TColor4f); overload;

    // Add a variable name and value to the console
    procedure AddVariable(const Name: String; const Value: String); overload;

    // Delete a item in the console
    procedure Delete(Index: Integer);

    // Number of items in the console
    property Count: Integer read FCount write SetCount;
    // Returns a pointer to the internal list
    property List: PConsoleLineList read FList;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Return a single item from the list
    property Items[const Index: Integer]: TPHXConsoleLine read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXConsoleParameter'}

// The console class
//------------------------------------------------------------------------------
TPHXConsole = class
  private
    FDevice: TPHXDevice;
    FCanvas: TPHXCanvas;

    FState: TPHXConsoleState;
    FItems: TPHXConsoleItems;
    FLines: TPHXConsoleLines;

    // The key for the popup, default is §
    FShortcut       : TPHXVirtualKey;
    FShortcutPressed: Boolean;

    FHeight: Integer;
    FFont  : TPHXFont;
    FText  : String;

    // Colors
    FColorBackground: TColor4f;
    FColorText      : TColor4f;
    FColorItems     : TColor4f;

    FScrollTime: Single;

    FParameters: TPHXConsoleParameters;

    procedure AutoComplete;

    procedure RenderLines(const Dest: TRecti);
    procedure RenderBackground(const Dest: TRecti);

    function GetVisible: Boolean;

    procedure SetHeight(const Value: Integer);
  protected
    // The phoenix event handler
    procedure EventHandler(Sender: TObject; const Event: TPHXEvent);
    // Event that is called when a key is pressed
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
    // Event that is called when a key is released
    procedure KeyReleased(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
    // Event for keyboard text input
    procedure KeyCharacter(Key: WideChar; Shift: TPHXShiftStates);
  public
    // Create a new console
    constructor Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas; AFont: TPHXFont);
    // Default destructor
    destructor Destroy; override;

    // Show the console
    procedure Show;
    // Hide the console
    procedure Hide;

    // Update the console
    procedure Update(FrameTime: Double);
    // Render the console
    procedure Render;

    // Add text to the console
    procedure Add(const Text: String); overload;
    // Add text to the console
    procedure Add(const Text: String; Args: Array of const); overload;

    // Execute a console command
    procedure Execute(const Command: String);

    // The owning device
    property Device: TPHXDevice read FDevice;
    // The canvas the console is rendering to
    property Canvas: TPHXCanvas read FCanvas;
    // The font
    property Font: TPHXFont read FFont write FFont;
    // The key that is used for showing the console
    property Shortcut: TPHXVirtualKey read FShortcut write FShortcut;
    // Height of the console when opened
    property Height: Integer read FHeight write SetHeight;
    // The items in the console
    property Items: TPHXConsoleItems read FItems;
    // Current input text of the console
    property Text: String read FText write FText;
    // Lines of the console
    property Lines: TPHXConsoleLines read FLines;

    // Current State of the console
    property State : TPHXConsoleState read FState;
    // Returns if the console is visible (expanded)
    property Visible: Boolean read GetVisible;
  end;

{$ENDREGION}



implementation

resourcestring
  SUnknownCommand = 'Unknown command: ';
  SInvalidParameters =  'Invalid parameters';

// TPHXConsoleParameter
//------------------------------------------------------------------------------
constructor TPHXConsoleParameter.Create;
begin
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameter.GetInteger: Integer;
begin
  Result:= StrToIntDef( FText, 0);
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameter.GetFloat: Single;
var FS: TFormatSettings;
begin
  {$IFDEF FPC}
  FS:= DefaultFormatSettings;
  {$ELSE}
  FS:= TFormatSettings.Create;
  FS.DecimalSeparator:= '.';
  {$ENDIF}


  Result:= StrToFloatDef( FText, 0, FS);
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameter.GetBoolean: Boolean;
begin
  Result:= StrToBoolDef( FText, False)
end;

// TPHXConsoleParameters
//==============================================================================
constructor TPHXConsoleParameters.Create;
begin
  FItems:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXConsoleParameters.Destroy;
begin
  Clear;

  FItems.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleParameters.Clear;
var Index: Integer;
begin
  For Index:=0 to FItems.Count-1 do
  begin
    TPHXConsoleParameter( FItems[Index]).Free;
  end;
  FItems.Clear;
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameters.Add(const Parameter: TPHXConsoleParameter): TPHXConsoleParameter;
begin
  Result:= Parameter;

  FItems.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameters.Add(const Parameter: String): TPHXConsoleParameter;
begin
  Result:= TPHXConsoleParameter.Create;
  Result.Text:= Parameter;

  FItems.Add(Result);
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleParameters.Parse;
var Len  : Integer;
var Index: Integer;
var Start: Integer;
var Param: TPHXConsoleParameter;
begin

  Clear;

  Index:= 1;
  Len  := Length(FText);
  while Index <= Len do
  begin
    // Skip whitespace
    while (FText[Index] <= #32) do
    begin
      if (Index > Len) then Exit;

      Inc(Index);
    end;

    if (Index <= Len) then
    begin
      Start:= Index;

      // Find the next seperator
      while (FText[Index] <> ' ') and (Text[Index] <> ',') do
      begin
        if (Index > Len) then Break;

        Inc(Index);
      end;

      // Add the parameter if not empty
      if Start <> Index then
      begin
        Param:= TPHXConsoleParameter.Create;
        Param.Text:= Copy(FText, Start, Index-Start);

        FItems.Add(Param);
      end;
      // Skip the seperator
      Inc(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameters.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

//------------------------------------------------------------------------------
function TPHXConsoleParameters.GetParameter(Index: Integer): TPHXConsoleParameter;
begin
  Result:= TPHXConsoleParameter( FItems[Index] );
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleParameters.SetText(const Value: String);
begin
  FText:= Value;

  Parse;
end;

// TPHXConsoleItem
//==============================================================================
constructor TPHXConsoleItem.Create(AConsole: TPHXConsole);
begin
  FConsole:= AConsole;
end;

// TPHXConsoleVariableString
//==============================================================================
procedure TPHXConsoleVariableString.Execute(Parameters: TPHXConsoleParameters);
begin
  // Write the variable ?
  if Parameters.Count = 1 then
  begin
    Variable^:= Parameters.Text;
  end;

  Console.Lines.AddVariable(Name, Variable^);
end;


// TPHXConsoleVariable1i
//==============================================================================
procedure TPHXConsoleVariable1i.Execute(Parameters: TPHXConsoleParameters);
begin
  // Write the variable ?
  if Parameters.Count >= 1 then
  begin
     Variable^:= Parameters[0].AsInteger ;
  end;

  Console.Lines.AddVariable(Name, IntToStr(Variable^));
end;

// TPHXConsoleVariableb
//==============================================================================
procedure TPHXConsoleVariable1b.Execute(Parameters: TPHXConsoleParameters);
begin
  // Write the variable ?
  if Parameters.Count >= 1 then
  begin
     Variable^:= Parameters[0].AsBoolean;
  end;

  if Variable^ then
  begin
    FConsole.Lines.AddVariable(Name, '1');
  end else
  begin
    FConsole.Lines.AddVariable(Name, '0');
  end;
end;


// TPHXConsoleVariable4f
//==============================================================================
procedure TPHXConsoleVariable4f.Execute(Parameters: TPHXConsoleParameters);
var Value: String;
begin
  // Write the variable ?
  if Parameters.Count >= 4 then
  begin
     Variable^.X:= Parameters[1].AsFloat;
     Variable^.Y:= Parameters[2].AsFloat;
     Variable^.Z:= Parameters[3].AsFloat;
     Variable^.W:= Parameters[4].AsFloat;
  end;

  Value:= Format('%.2f %.2f %.2f %.2f', [Variable^.X, Variable^.Y, Variable^.Z, Variable^.W]);

  FConsole.Lines.AddVariable(Name, Value);
end;

// TPHXConsoleColor3f
//==============================================================================
constructor TPHXConsoleColor3f.Create(AConsole: TPHXConsole; AColor: PColor3f);
begin
  inherited Create(AConsole);

  Assert(Assigned(AColor));

  FColor:= AColor;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleColor3f.Execute(Parameters: TPHXConsoleParameters);
var Color4f: TColor4f;
begin
  // Should we write the variable
  if Parameters.Count > 0 then
  begin
    // Named color
    if Parameters.Count = 1 then
    begin
      Color4f:= StringToColor( Parameters[0].AsString );

      Color^.Red  := Color4f.Red;
      Color^.Green:= Color4f.Green;
      Color^.Blue := Color4f.Blue;
    end;

  end;

  Color4f.Red  := Color^.Red;
  Color4f.Green:= Color^.Green;
  Color4f.Blue := Color^.Blue;
  Color4f.Alpha:= 1.0;

  // Show the value
  FConsole.Lines.AddVariable(Name, ColorToString(Color4f));
end;

// TPHXConsoleColor3f
//==============================================================================
constructor TPHXConsoleColor4f.Create(AConsole: TPHXConsole; AColor: PColor4f);
begin
  inherited Create(AConsole);

  Assert(Assigned(AColor));

  FColor:= AColor;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleColor4f.Execute(Parameters: TPHXConsoleParameters);
begin
  // Should we write the variable
  if Parameters.Count > 0 then
  begin
    // Named color
    if Parameters.Count = 1 then
    begin
      Color^:= StringToColor( Parameters[0].AsString );
     // Color^:= HexToColor( Parameters[0].AsString );
    end else
    // RGB
    if Parameters.Count = 3 then
    begin
      Color^.Red  := Parameters[0].AsFloat / 255;
      Color^.Green:= Parameters[1].AsFloat / 255;
      Color^.Blue := Parameters[2].AsFloat / 255;
      Color^.Alpha:= 1.0;
    end else
    // RGBA
    if Parameters.Count = 4 then
    begin
      Color^.Red  := Parameters[0].AsFloat / 255;
      Color^.Green:= Parameters[1].AsFloat / 255;
      Color^.Blue := Parameters[2].AsFloat / 255;
      Color^.Alpha:= Parameters[3].AsFloat / 255;
    end else
    begin
      FConsole.Lines.Add(SInvalidParameters);
    end;
  end;

  FConsole.Lines.AddVariable(FName, ColorToString(Color^));
end;

// TPHXConsoleFunction
//==============================================================================
constructor TPHXConsoleFunction.Create(AConsole: TPHXConsole; const AName: String; const ACallback: TPHXConsoleCallback);
begin
  inherited Create(AConsole);

  Assert(Assigned(ACallback));

  FName    := AName;
  FCallback:= ACallback;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleFunction.Execute(Parameters: TPHXConsoleParameters);
begin
  FCallback(Console, Parameters);
end;








// TPHXConsoleItems
//==============================================================================
constructor TPHXConsoleItems.Create(AOwner: TPHXConsole);
begin
  FOwner:= AOwner;
  FList := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXConsoleItems.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleItems.Clear;
var Index: Integer;
begin
  For Index:=0 to FList.Count-1 do
  begin
    TPHXConsoleItem( FList[Index] ).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; var Value: String): TPHXConsoleVariableString;
begin
  Result:= TPHXConsoleVariableString.Create(Owner);
  Result.Name    := Name;
  Result.Variable:= @Value;

  FList.Add(Result);
end;


//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; var Value: Integer): TPHXConsoleVariable1i;
begin
  Result:= TPHXConsoleVariable1i.Create(Owner);
  Result.Name    := Name;
  Result.Variable:= @Value;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; var Value: Boolean): TPHXConsoleVariable1b;
begin
  Result:= TPHXConsoleVariable1b.Create(Owner);
  Result.Name    := Name;
  Result.Variable:= @Value;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; var Value: TVector4f): TPHXConsoleVariable4f;
begin
  Result:= TPHXConsoleVariable4f.Create(Owner);
  Result.Name    := Name;
  Result.Variable:= @Value;

  FList.Add(Result);
end;


//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; var Value: TColor3f): TPHXConsoleColor3f;
begin
  Result:= TPHXConsoleColor3f.Create(Owner, @Value);
  Result.Name := Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; var Value: TColor4f): TPHXConsoleColor4f;
begin
  Result:= TPHXConsoleColor4f.Create(Owner, @Value);
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.Add(const Name: String; Value: TPHXConsoleCallback): TPHXConsoleFunction;
begin
  Result:= TPHXConsoleFunction.Create(Owner, Name, Value);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.IndexOf(const Name: String): Integer;
var Index: Integer;
var Item : TPHXConsoleItem;
begin
  For Index:=0 to FList.Count-1 do
  begin
    Item:= TPHXConsoleItem(FList.List[Index] );

    if SameText(Item.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.GetList: PConsoleItemList;
begin
  Result:= PConsoleItemList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXConsoleItems.GetItem(const Index: Integer): TPHXConsoleItem;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= TPHXConsoleItem( FList.List[Index]);
end;

{$REGION 'TPHXConsoleLine'}

// TPHXConsoleLine
//==============================================================================
class function TPHXConsoleLine.Create(const ATime: Double; const AColor: TColor4f; const AText: String): TPHXConsoleLine;
begin
  Result.Time:= ATime;
  Result.Color:= AColor;
  Result.Text:= AText;
end;

{$ENDREGION}


{$REGION 'TPHXConsoleLines'}

// TPHXConsoleLines
//==============================================================================
constructor TPHXConsoleLines.Create(AConsole: TPHXConsole);
begin
  FConsole := AConsole;
  FList    := nil;
  FCapacity:= 0;
  FCount   := 0;
end;

//------------------------------------------------------------------------------
destructor TPHXConsoleLines.Destroy;
begin
  SetCapacity(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.Clear;
begin
  FCount:= 0;

  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.Delete(Index: Integer);
begin
  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);

  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TPHXConsoleLine));

    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount])^ := nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.Add(const Line: TPHXConsoleLine);
begin
  Inc(FCount);

  if FCount > FCapacity then Grow;

  Pointer(FList^[FCount-1].Text):= nil;

  FList^[FCount-1].Time  := Line.Time;
  FList^[FCount-1].Color := Line.Color;
  FList^[FCount-1].Text  := Line.Text;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.Add(const Text: String);
var Line: TPHXConsoleLine;
begin
  Line.Time := 0;
  Line.Color:= FConsole.FColorItems;
  Line.Text := Text;

  Add(Line);
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.Add(const Text: String; const Color: TColor4f);
var Line: TPHXConsoleLine;
begin
  Line.Time := 0;
  Line.Color:= Color;
  Line.Text := Text;

  Add(Line);
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.AddVariable(const Name: String; const Value: String);
var Line: TPHXConsoleLine;
begin
  Line.Time := 0;
  Line.Color:= FConsole.FColorItems;

  if Value <> '' then
  begin
    Line.Text:= Name + ' ' + Value;
  end else
  begin
    Line.Text:= Name;
  end;

  Add(Line);
end;


//------------------------------------------------------------------------------
function TPHXConsoleLines.GetItem(const Index: Integer): TPHXConsoleLine;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList,  FCapacity * SizeOf(TPHXConsoleLine));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXConsoleLines.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;

    if FCount > FCapacity then
    begin
      SetCapacity(FCount);
    end;
  end;
end;

{$ENDREGION}

// TPHXConsole
//==============================================================================
constructor TPHXConsole.Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas; AFont: TPHXFont);
begin
  FDevice   := ADevice;
  FCanvas   := ACanvas;
  FFont     := AFont;
  FItems    := TPHXConsoleItems.Create(Self);
  FLines    := TPHXConsoleLines.Create(Self);
  FShortcut := VK_TILDE;
  FHeight   := 200;
  FState    := csClosed;

  FColorBackground:= Color4f(0.0, 0.0, 0.0, 0.5);
  FColorText      := clrWhite;
  FColorItems     := clrGray;

  FParameters:= TPHXConsoleParameters.Create;


  TPHXEvents.AddListener(EventHandler);
end;

//------------------------------------------------------------------------------
destructor TPHXConsole.Destroy;
begin
  TPHXEvents.RemoveListener(EventHandler);

  FParameters.Free;

  FItems.Free;
  FLines.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Hide;
begin
  FState:= csHiding;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Show;
begin
  FState:= csShowing;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Update(FrameTime: Double);
begin
  case State of
    csClosed: ;
   // The console is opening
    csShowing:
    begin
      FScrollTime:= FScrollTime + FrameTime;

      if FScrollTime > 0.5 then
      begin
        FState     := csOpen;
        FScrollTime:= 0;
      end;
    end;
    csOpen: ;
    // The console is closing
    csHiding:
    begin
      FScrollTime:= FScrollTime + FrameTime;

      if FScrollTime > 0.5 then
      begin
        FState     := csClosed;
        FScrollTime:= 0;
      end;
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Render;
var Dest : TRecti;
begin
  // Console is closed
  if State = csClosed then Exit;

  Dest.Left := 0;
  Dest.Right := Device.Width;
  Dest.Top   := 0;
  Dest.Bottom:= Height;

  case FState of
    csShowing:  Dest.Bottom:=          Trunc(Height * FScrollTime * 2);
    csHiding :  Dest.Bottom:= Height - Trunc(Height * FScrollTime * 2);
  end;

  RenderBackground(Dest);
  RenderLines(Dest);
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.RenderBackground(const Dest: TRecti);
begin
  FCanvas.Color:= FColorBackground;
  FCanvas.Texture:= nil;

  FCanvas.FilledRectangle(Dest.Left, Dest.Top, Dest.Right, Dest.Bottom);
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.RenderLines(const Dest: TRecti);
var Index: Integer;
var Y    : Integer;
begin
  Y:= Dest.Bottom - FFont.Metric.Height;

  FFont.TextOut(0, Y, FText, FColorText);

  Dec(Y, FFont.Metric.Height);
  Dec(Y, 4);

  for Index:= Lines.Count-1 downto 0 do
  begin
    FFont.TextOut(0, Y, Lines.FList[Index].Text, Lines.FList[Index].Color);

    Dec(Y, FFont.Metric.Height);

    if Y < 0 then Break;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Add(const Text: String);
begin
  FLines.Add(Text);
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Add(const Text: String; Args: array of const);
begin
  FLines.Add( Format(Text, Args) );
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.EventHandler(Sender: TObject; const Event: TPHXEvent);
begin
  case Event.Event of
    PHX_KEY_PRESSED  : KeyPressed (Event.Keyboard.Key, Event.Keyboard.Shift);
    PHX_KEY_RELEASED : KeyReleased(Event.Keyboard.Key, Event.Keyboard.Shift);
    PHX_KEY_CHARACTER: KeyCharacter(Event.Keyboard.Char, Event.Keyboard.Shift);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  if Key = FShortcut then
  begin
    FShortcutPressed:= True;

    // Hide the console
    if FState in [csOpen, csShowing] then
    begin
      Hide;
    end else
    // Show the console
    begin
      Show;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.KeyReleased(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  if Key = FShortcut then
  begin
    FShortcutPressed:= False;
  end;

  // The console is closed
  if not (State in [csShowing, csOpen]) then Exit;

  // Execute the text
  IF Key = VK_RETURN then
  begin
    Execute(Text);

    FText:= '';
  end else
  // erase text
  IF Key = VK_BACKSPACE then
  begin
    Delete(FText, Length(FText), 1);
  end else
  // Auto completion
  IF Key = VK_TAB then
  begin
    AutoComplete;
  end else
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.KeyCharacter(Key: WideChar; Shift: TPHXShiftStates);
begin
  // The console is closed
  if not (State in [csShowing, csOpen]) then Exit;

  // Add text
  if (Key >= #32) and (Key <= #255) and not FShortcutPressed then
  begin
    FText:= FText + Key;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.AutoComplete;
var Index    : Integer;
var Item     : TPHXConsoleItem;
var ItemStart: String;

var Matches: array of TPHXConsoleItem;
begin
  Matches:= nil;

  for Index:=0 to FItems.Count-1 do
  begin
    Item:= TPHXConsoleItem( FItems.List^[Index] );

    // Test if the current console item name matches the entered text
    if Length(Item.Name) >  Length(FText) then
    begin
      // Extract the start name of the text
      ItemStart:= Copy(Item.Name,1, Length(FText));

      // IF the item matches the text add it to the matches array 
      if SameText( ItemStart, FText) then
       begin
        SetLength(Matches, Length(Matches)+1);

        Matches[Length(Matches) - 1]:= Item;
      end;

    end;
  end;
  // Matches a single item
  if Length(Matches) = 1 then
  begin
    FText:= Matches[0].Name + ' ';
  end else
  // Matches more then one item, show all matchibg
  begin
    Add('');
    for Index:= 0 to Length(Matches)-1 do
    begin
      Add( Matches[Index].Name);
    end;
    Add('');
  end;
  SetLength(Matches, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.Execute(const Command: String);
var Index : Integer;
var Item  : TPHXConsoleItem;
var Name  : String;
var Params: String;
begin
  FLines.Add(Command, FColorText);

  // Special syntax, item=value
  Index:= Pos('=', Command);
  if Index > 0 then
  begin
    Name  := Trim(Copy(Command, 1, Index-1));
    Params:= Trim(Copy(Command, Index+1, MaxInt));
  end else
  begin
    // Extract the name and the parameters of the command
    Index:= Pos(' ', Command);
    if Index > 0 then
    begin
      Name  := Copy(Command, 1, Index-1);
      Params:= Copy(Command, Index+1, MaxInt);
    end else
    begin
      Name  := Command;
      Params:= '';
    end;
  end;

  // Find the item index
  Index:= Items.IndexOf(Name);

  if Index <> -1 then
  begin
    Item:= TPHXConsoleItem(FItems.List^[Index]);

    // Parse the parameters
    FParameters.Text:= Params;

    // Execute the item
    Item.Execute(FParameters);
  end else
  begin
    FLines.Add(SUnknownCommand + Command);
  end;
end;

//------------------------------------------------------------------------------
function TPHXConsole.GetVisible: Boolean;
begin
  Result:= (FState = csOpen) or (FState = csShowing) or (FState = csHiding);
end;

//------------------------------------------------------------------------------
procedure TPHXConsole.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;





end.
