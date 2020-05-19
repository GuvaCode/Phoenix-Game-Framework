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
unit phxApplication;
//< Framework classes

interface

{$I phxConfig.inc}

uses
  Classes, Types, SysUtils,

  phxLogger,
  phxTypes,
  phxEvents,
  phxDevice,
  phxTimer;

type

//
TPHXTimer = phxTimer.TPHXTimer;

{$REGION 'TPHXApplication'}

// Template class for the user application
//------------------------------------------------------------------------------

{ TPHXApplication }

TPHXApplication = class(TObject)
  private

    Terminated: Boolean;

    // Handle a phoenix event
    procedure EventHandler(Sender: TObject; const Event: TPHXEvent);
  protected
    // Event that is called when a mouse button is pressed
    procedure MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton); virtual;
    // Event that is called when the mouse is moved
    procedure MouseMoved(X, Y: Integer; Shift: TPHXShiftStates); virtual;
    // Event that is called when a mouse button is released
    procedure MouseReleased(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton); virtual;
    // Event that is called when a key is pressed
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); virtual;
    // Event that is called when a key is released
    procedure KeyReleased(Key: TPHXVirtualKey; Shift: TPHXShiftStates); virtual;
  protected
    // Return the path containing the content for the application
    function GetContentPath: String; virtual;
  public
    // Create a new application
    constructor Create; virtual;
    // Free the application
    destructor Destroy; override;

    // Initializes the application
    procedure Init; virtual;
    // Shutdown the application
    procedure Shutdown; virtual;

    // Update the application
    procedure Update; virtual;
    // Render the application
    procedure Render; virtual;

    // Called when the device is resized
    procedure Resized; virtual;

    // Run the application
    procedure Run;
    // Terminate the application
    procedure Terminate;

   // Return the path containing the content for the application
    property ContentPath: String read GetContentPath;
  end;

{$ENDREGION}

{$REGION 'TPHXGameScreen'}

TPHXGameScreen  = class;
TPHXGameScreens = class;

//------------------------------------------------------------------------------
TPHXTransitionMode = (
  tmNone,
  tmShow,
  tmHide
);

// Controls transitions when opening and closing screens
//------------------------------------------------------------------------------
TPHXGameTransition = class
  private
    FOwner: TPHXGameScreen;

    FMode    : TPHXTransitionMode;
    FTime    : Single;
    FPosition: Single;
    FDone    : Boolean;

    FTimeShow: Single;
    FTimeHide: Single;
  public
    constructor Create(AOwner: TPHXGameScreen);
    destructor Destroy; override;

    // Reset the transition
    procedure Reset(Mode: TPHXTransitionMode);
    // Update the transition
    procedure Update(const FrameTime: Single);

    property Mode: TPHXTransitionMode read FMode write FMode;
    // Current transition time in seconds
    property Time: Single read FTime write FTime;
    // Current transition position ranging from 0.0 (Hidden) to 1.0 (Visible)
    property Position: Single read FPosition write FPosition;
    // The transition is completed
    property Done: Boolean read FDone;

    // How long the screen takes to transition on when it is activated.
    property TimeShow: Single read FTimeShow write FTimeShow;
    // How long the screen takes to transition off when it is deactivated.
    property TimeHide: Single read FTimeHide write FTimeHide;
  end;

TPHXGameScreenClass = class of TPHXGameScreen;

// A game screen is
//------------------------------------------------------------------------------
TPHXGameScreen = class(TPersistent)
  private
    FOwner     : TPHXGameScreens;
    FName      : String;
    FData      : Pointer;
    FTransition: TPHXGameTransition;
  protected
    // Event that is called when a mouse button is pressed
    procedure MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton); virtual;
    // Event that is called when the mouse is moved
    procedure MouseMoved(X, Y: Integer; Shift: TPHXShiftStates); virtual;
    // Event that is called when a mouse button is released
    procedure MouseReleased(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton); virtual;
    // Event that is called when a key is pressed
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); virtual;
    // Event that is called when a key is released
    procedure KeyReleased(Key: TPHXVirtualKey; Shift: TPHXShiftStates); virtual;
  protected
    // Celled when the screen is opened
    procedure Opened; virtual;
    // Celled when the screen is closed
    procedure Closed; virtual;
  public
    constructor Create(AOwner: TPHXGameScreens; AData: Pointer); virtual;
    destructor Destroy; override;

    // Update the game screen
    procedure Update; virtual;
    // Render the game screen
    procedure Render; virtual;

    // Open this screen
    procedure Open; overload;
    // Open another screen
    procedure Open(const Name: String); overload;
    // Close this screen
    procedure Close;

    // The owning screen manager
    property Owner: TPHXGameScreens read FOwner;
    // Name of the screen
    property Name: String read FName write FName;
    // Custom data
    property Data: Pointer read FData write FData;
    // Screen transition
    property Transition: TPHXGameTransition read FTransition;
  end;

// Manager for game screens
//------------------------------------------------------------------------------
TPHXGameScreens = class
  private
    // List of all known screens
    FScreens: TList;
    // List of the current active screens,
    // The top most scene recieves the update event, and all scenes the render
    FStack: TList;
    // List of screens that is transitioning
    FTransitions: TList;

    // Handle a phoenix event
    procedure EventHandler(Sender: TObject; const Event: TPHXEvent);

    function GetActive: TPHXGameScreen;
  protected
    property Stack : TList read FStack;
    // List of screens that is transitioning
    property Transitions : TList read FTransitions;
  public
    // Creates a screen manager
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    // Remove and free all screens
    procedure Clear;

    // Add a screen to the manager
    procedure Add(Screen: TPHXGameScreen); overload;
    // Add a screen to the manager
    procedure Add(Screen: TPHXGameScreenClass; const Name: String; const Data: Pointer); overload;

    // Open a game screen
    procedure Open(Screen: TPHXGameScreen); overload;
    // Close a game screen
    procedure Close(Screen: TPHXGameScreen); overload;

    // Open a game screen
    procedure Open(const Name: String); overload;
    // Close a game screen
    procedure Close(const Name: String); overload;

    // Update the active screen
    procedure Update(const FrameTime: Single);
    // Renders all visible screens
    Procedure Render;

    // Find a screen by name
    function FindScreen(const Name: String): TPHXGameScreen;

    // Detemines if a screen is visible
    function IsVisible(const Screen: TPHXGameScreen): Boolean; overload;
    // Determines if a screen is active
    function IsActive(const Screen: TPHXGameScreen): Boolean; overload;

    // List of game screens
    property Screens: TList read FScreens;
    // The active screens
    property Active: TPHXGameScreen read GetActive;
  end;

{$ENDREGION}

implementation

{$REGION 'TPHXApplication'}

// TPHXApplication
//==============================================================================
constructor TPHXApplication.Create;
begin
  TPHXEvents.AddListener(EventHandler);
end;

//------------------------------------------------------------------------------
destructor TPHXApplication.Destroy;
begin
  TPHXEvents.RemoveListener(EventHandler);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXApplication.EventHandler(Sender: TObject; const Event: TPHXEvent);
begin
  case Event.Event of
    PHX_DEVICE_RESIZED:
    begin
      Resized;
    end;
    PHX_MOUSE_PRESSED :
    begin
      MousePressed(Event.Mouse.X, Event.Mouse.Y, Event.Mouse.Shift, Event.Mouse.Button);
    end;
    PHX_MOUSE_RELEASED:
    begin
      MouseReleased(Event.Mouse.X, Event.Mouse.Y, Event.Mouse.Shift, Event.Mouse.Button);
    end;
    PHX_MOUSE_MOVED:
    begin
      MouseMoved(Event.Mouse.X, Event.Mouse.Y, Event.Mouse.Shift);
    end;
    PHX_KEY_PRESSED:
    begin
      KeyPressed(Event.Keyboard.Key, Event.Keyboard.Shift);
    end;
    PHX_KEY_RELEASED:
    begin
      KeyReleased  (Event.Keyboard.Key, Event.Keyboard.Shift);
    end;
    PHX_EVENT_QUIT:
    begin
      // Break from the main loop
      Terminated:= True;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXApplication.MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin

end;
//------------------------------------------------------------------------------
procedure TPHXApplication.MouseMoved(X, Y: Integer; Shift: TPHXShiftStates);
begin

end;
//------------------------------------------------------------------------------
procedure TPHXApplication.MouseReleased(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.KeyReleased(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin

end;


//------------------------------------------------------------------------------
procedure TPHXApplication.Init;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.Update;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.Render;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.Shutdown;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.Run;
begin
  Terminated:= False;

  Init;

  Resized;

  // Enter the main loop
  while not Terminated do
  begin
    Update;
    Render;
  end;

  Shutdown;
end;

//------------------------------------------------------------------------------
procedure TPHXApplication.Resized;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXApplication.Terminate;
begin
  TPHXEvents.NotifyQuit(Self);
end;

//------------------------------------------------------------------------------
function TPHXApplication.GetContentPath: String;
begin
  // Keep the original path to /Contents/Resources in the .app - file
  {$IFDEF DARWIN}
  Result = '../resources/';
  {$ELSE}
  Result:= ExtractFilePath( ParamStr(0) ) + 'resources' + PathDelim;
  {$ENDIF}
end;

{$ENDREGION}

{$REGION 'TPHXGameTransition'}

// TPHXGameTransition
//==============================================================================
constructor TPHXGameTransition.Create(AOwner: TPHXGameScreen);
begin
  FOwner := AOwner;

  FMode    := tmNone;
  FTime    := 0;
  FPosition:= 0.0;
  FDone    := True;
  FTimeShow:= 1.0;
  FTimeHide:= 1.0;
end;

//------------------------------------------------------------------------------
destructor TPHXGameTransition.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGameTransition.Reset(Mode: TPHXTransitionMode);
begin
  FMode    := Mode;
  FDone    := False;
  FTime    := 0;
  FPosition:= 0.0;
end;

//------------------------------------------------------------------------------
procedure TPHXGameTransition.Update(const FrameTime: Single);
begin
  // No transition is acitve
  if Done or (Mode = tmNone) then Exit;

  // Show transition
  if Mode = tmShow then
  begin
    FTime:= FTime + FrameTime;

    FPosition:= (FTime / FTimeShow);

    if FPosition >= 1.0 then
    begin
      FPosition:= 1.0;

      FDone:= True;
    end;
  end else
  // Hide transition
  if Mode = tmHide then
  begin
    FTime:= FTime + FrameTime;

    FPosition:= 1.0 - (FTime / FTimeHide);

    if FPosition <= 0.0 then
    begin
      FPosition:= 0.0;

      FDone:= True;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXGameScreen'}

// TPHXGameScreen
//==============================================================================
constructor TPHXGameScreen.Create(AOwner: TPHXGameScreens; AData: Pointer);
begin
  FOwner := AOwner;
  FData  := AData;
  FName  := '';

  FTransition:= TPHXGameTransition.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TPHXGameScreen.Destroy;
begin
  FTransition.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Open;
begin
  FOwner.Open(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Open(const Name: String);
begin
  FOwner.Open(Name);
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Close;
begin
  FOwner.Close(Self);
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Opened;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Closed;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Update;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.Render;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.MouseMoved(X, Y: Integer; Shift: TPHXShiftStates);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.MouseReleased(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXGameScreen.KeyReleased(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin

end;

{$ENDREGION}

{$REGION 'TPHXGameScreens'}


{$IFDEF PHOENIX_LUA}

function Lua_ScreenManagerOpen (L: PLua_State): Integer; cdecl; forward;
function Lua_ScreenManagerClose(L: PLua_State): Integer; cdecl; forward;

//------------------------------------------------------------------------------
procedure Lua_PushScreens(L: PLuaState; Screens: TPHXGameScreens);
begin
  if Assigned(Screens) then
  begin
    // Create table for the bindings
    lua_newtable(L);

    lua_pushstring(L, 'Open');
    lua_pushcfunction(L, Lua_ScreenManagerOpen);
    lua_rawset(L, -3);

    lua_pushstring(L, 'Close');
    lua_pushcfunction(L, Lua_ScreenManagerClose);
    lua_rawset(L, -3);

    // Create meta table for the bindings
    lua_newtable(L);

      // Save the objet instance in the meta table
      lua_pushstring(L, 'this');
      lua_pushlightuserdata(L, Manager);
      lua_rawset(L, -3);

    // set the meta table for the bindings
    lua_setmetatable(L, -2);
  end else
  begin
    lua_pushnil(L);
  end;
end;

//------------------------------------------------------------------------------
function Lua_ScreenManagerOpen(L: PLua_State): Integer; cdecl;
var Manager: TPHXStateManager;
var Name   : String;
begin
  Manager:= TPHXStateManager( Lua_This(L, 1) );
  Result := 0;

  if not Assigned(Manager) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStateManager.');
    lua_error(L);
    Exit;
  end;

  Name:= luaL_checkstring(L, 2);

  Manager.Open(Name);
end;

//------------------------------------------------------------------------------
function Lua_ScreenManagerClose(L: PLua_State): Integer; cdecl;
var Manager: TPHXStateManager;
var Name   : String;
begin
  Manager:= TPHXStateManager( Lua_This(L, 1) );
  Result := 0;

  if not Assigned(Manager) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStateManager.');
    lua_error(L);
    Exit;
  end;

  Name:= luaL_checkstring(L, 2);

  Manager.Close(Name);
end;

{$ENDIF}


// TPHXGameScreens
//==============================================================================
constructor TPHXGameScreens.Create;
begin
  FScreens     := TList.Create;
  FStack      := TList.Create;
  FTransitions:= TList.Create;

  TPHXEvents.AddListener(EventHandler);
end;

//------------------------------------------------------------------------------
destructor TPHXGameScreens.Destroy;
begin
  TPHXEvents.RemoveListener(EventHandler);

  Clear;

  FScreens.Free;
  FStack.Free;
  FTransitions.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.EventHandler(Sender: TObject; const Event: TPHXEvent);
var Screen: TPHXGameScreen;
begin
  // Update the screen on top of the stac
  if Stack.Count > 0 then
  begin
    Screen:= TPHXGameScreen(Stack.Last);

    case Event.Event of
      PHX_MOUSE_PRESSED:
      begin
        Screen.MousePressed(Event.Mouse.X, Event.Mouse.Y, Event.Mouse.Shift, Event.Mouse.Button);
      end;
      PHX_MOUSE_RELEASED:
      begin
        Screen.MouseReleased(Event.Mouse.X, Event.Mouse.Y, Event.Mouse.Shift, Event.Mouse.Button);
      end;
      PHX_MOUSE_MOVED:
      begin
        Screen.MouseMoved(Event.Mouse.X, Event.Mouse.Y, Event.Mouse.Shift);
      end;
      PHX_KEY_PRESSED:
      begin
        Screen.KeyPressed(Event.Keyboard.Key, Event.Keyboard.Shift);
      end;
      PHX_KEY_RELEASED:
      begin
        Screen.KeyReleased(Event.Keyboard.Key, Event.Keyboard.Shift);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Clear;
var Screen: TPHXGameScreen;
var Index: Integer;
begin
  for Index := 0 to FScreens.Count - 1 do
  begin
    Screen:=TPHXGameScreen(FScreens.List[Index]);
    Screen.Free;
  end;
  FScreens.Clear;
  FStack.Clear;
  FTransitions.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Add(Screen: TPHXGameScreen);
begin
  FScreens.Add(Screen);
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Add(Screen: TPHXGameScreenClass; const Name: String; const Data: Pointer);
var AScreen: TPHXGameScreen;
begin
  AScreen:= Screen.Create(Self, Data);
  AScreen.Name:= Name;

  Add(AScreen);
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Open(Screen: TPHXGameScreen);
begin
  // The screen is already opened
  if Stack.IndexOf(Screen) >= 0 then Exit;
  // The screen is already opened
  if Transitions.IndexOf(Screen) >= 0 then Exit;

  if Screen.Transition.TimeShow > 0 then
  begin
    FTransitions.Add(Screen);

    Screen.Transition.Reset(tmShow);
  end;

  FStack.Add(Screen);

  Screen.Opened;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Close(Screen: TPHXGameScreen);
begin
  // The screen is already opened
  if Transitions.IndexOf(Screen) >= 0 then Exit;

  if Screen.Transition.TimeHide > 0 then
  begin
    Screen.Transition.Reset(tmHide);

    FTransitions.Add(Screen);
  end else
  begin
    Screen.Closed;

    FStack.Remove(Screen);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Open(const Name: String);
var Screen: TPHXGameScreen;
begin
  Screen:= FindScreen(Name);

  if Assigned(Screen) then
  begin
    Open(Screen);
  end else
  begin
    raise Exception.CreateFmt('The game screen "%s" was not found', [Name]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Close(const Name: String);
var Screen: TPHXGameScreen;
begin
  Screen:= FindScreen(Name);

  if Assigned(Screen) then
  begin
    Close(Screen);
  end else
  begin
    raise Exception.CreateFmt('The game screen "%s" was not found', [Name]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Update(const FrameTime: Single);
var Index : Integer;
var Screen: TPHXGameScreen;
begin
  Index:= 0;
  while Index < Transitions.Count do
  begin
    Screen:= TPHXGameScreen(Transitions.List[Index]);

    // Update the transition
    Screen.Transition.Update(FrameTime);

    // When the transition is completed, remove the screen from the transition list
    if Screen.Transition.Done then
    begin
      FTransitions.Delete(Index);

      if Screen.Transition.Mode = tmHide then
      begin
        Screen.Closed;

        FStack.Remove(Screen);
      end;

    end else
    begin
      Inc(Index);
    end;
  end;

  // Update the screen on top of the stack
  if Stack.Count > 0 then
  begin
    Screen:= TPHXGameScreen( FStack.Last );
    Screen.Update;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXGameScreens.Render;
var Index: Integer;
var Item : TPHXGameScreen;
begin
  for Index := 0 to Stack.Count - 1 do
  begin
    Item:= TPHXGameScreen( Stack.List[Index] );

    Item.Render;
  end;
end;

//------------------------------------------------------------------------------
function TPHXGameScreens.FindScreen(const Name: String): TPHXGameScreen;
var Index: Integer;
var Item : TPHXGameScreen;
begin
  for Index := 0 to FScreens.Count - 1 do
  begin
    Item:= TPHXGameScreen( FScreens.List[Index] );

    if SameText(Item.Name, Name) then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXGameScreens.IsVisible(const Screen: TPHXGameScreen): Boolean;
var Index  : Integer;
var Current: TPHXGameScreen;
begin
  for Index := 0 to FStack.Count - 1 do
  begin
    Current:= TPHXGameScreen( Stack.List[Index] );

    if Current = Screen then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXGameScreens.IsActive(const Screen: TPHXGameScreen): Boolean;
begin
  if Stack.Count > 0 then
  begin
    Result:= TPHXGameScreen( FStack.Last ) = Screen;
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
function TPHXGameScreens.GetActive: TPHXGameScreen;
begin
  if Stack.Count > 0 then
  begin
    Result:= TPHXGameScreen( Stack.Last );
  end else
  begin
    Result:= nil;
  end;
end;

{$ENDREGION}



end.
