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
unit phxEvents;
//< Event management

interface

{$I phxConfig.inc}

uses Classes, SysUtils,

  phxTypes;

const
  scNone     = $0000;
  scShift    = $0800;
  scCtrl     = $1000;
  scAlt      = $2000;
  scMouse    = $4000;
  scJoystick = $8000;

  ModShift    = $0800;
  ModCtrl     = $1000;
  ModAlt      = $2000;
  ModMouse    = $4000;
  ModJoystick = $8000;

type

// Event for mouse buttons
TPHXMouseButtonEvent = procedure(X, Y: Integer; Button: Integer) of object;
// Event for mouse movement
TPHXMouseMoveEvent = procedure(X, Y: Integer) of object;

// Event for key press or release
TPHXKeyEvent = procedure(Key : Integer; Shift: TPHXShiftStates) of object;


// Event types
//------------------------------------------------------------------------------
TPHXEventType = (
  // No event
  PHX_EVENT_NONE,
  // The user has requested to quit the application
  PHX_EVENT_QUIT,
  // User generated event
  PHX_EVENT_USER,

  // TPHXDeviceEvent

  // The device window has been resized
  PHX_DEVICE_RESIZED,
  // Occurs after a device is reset, allowing an application to re-create all resources.
  PHX_DEVICE_RESET,
  // Occurs if the device is activated, ie gaining focus
  PHX_DEVICE_ACTIVATED,
  // Occurs if the device is deactivated, ie lost focus
  PHX_DEVICE_DEACTIVATED,

  // TPHXMouseButtonEvent

  // A mouse button has been presssed
  PHX_MOUSE_PRESSED,
  // A mouse button has been released
  PHX_MOUSE_RELEASED,
  // The mouse has been moved,
  PHX_MOUSE_MOVED,
  // Mouse wheel up
  PHX_MOUSE_WHEELUP,
  // Mouse wheel down
  PHX_MOUSE_WHEELDOWN,

  // TPHXKeyboardEvent

  // A keyboard key has been pressed,
  PHX_KEY_PRESSED,
  // A keyboard key has been released,
  PHX_KEY_RELEASED,
  // Event for keyboard text input
  PHX_KEY_CHARACTER,

  // ***** TPHXJoystickEvent *****

  PHX_JOYSTICK_INFO,
  PHX_JOYSTICK_AXIS,
  PHX_JOYSTICK_PRESSED,
  PHX_JOYSTICK_RELEASED
);

// Event parameters for the mouse events (PHX_MOUSE_PRESSED, PHX_MOUSE_RELEASED or PHX_MOUSE_MOVED)
//------------------------------------------------------------------------------
TPHXMouseEvent = record
  // Event type (PHX_MOUSE_PRESSED, PHX_MOUSE_RELEASED or PHX_MOUSE_MOVED)
  Event : TPHXEventType;
  // Shift states
  Shift : TPHXShiftStates;
  // X position of the mouse
  X: Integer;
  // Y position of the mouse
  Y: Integer;
  // The pressed or released button
  Button: TPHXMouseButton;
end;

// Event parameters for the keyboard events (PHX_KEY_PRESSED, PHX_KEY_RELEASED or PHX_KEY_CHARACTER)
//------------------------------------------------------------------------------
TPHXKeyboardEvent = record
  // Event type (PHX_KEY_PRESSED, PHX_KEY_RELEASED or PHX_KEY_CHARACTER)
  Event: TPHXEventType;
  // Shift states
  Shift : TPHXShiftStates;
  // The virtual key code
  Key: TPHXVirtualKey;
  // The character
  Char: WideChar;
end;

// Event parameters for the joystick events (PHX_JOYSTICK_AXIS, PHX_JOYSTICK_PRESSED, PHX_JOYSTICK_RELEASED)
//------------------------------------------------------------------------------
TPHXJoystickEvent = record
  // Event type (PHX_JOYSTICK_AXIS, PHX_JOYSTICK_PRESSED, PHX_JOYSTICK_RELEASED)
  Event: TPHXEventType;
  // Shift states
  Shift : TPHXShiftStates;
  // Index of the joystick
  Index: TPHXJoystickIndex;
  // The joystick button that was released or pressed
  Button: TPHXJoystickButton;
  // The joystick axis that was moved
  Axis: TPHXJoystickAxis;
  // The position of the joystick axis
  Position: Integer;
end;

// Event parameters for the user event (PHX_EVENT_USER)
//------------------------------------------------------------------------------
TPHXUserEvent = record
  // Event type (PHX_EVENT_USER)
  Event : TPHXEventType;
  // Shift states
  Shift : TPHXShiftStates;
  // User code
  Code  : Cardinal;
  // User data #1
  Param1: Pointer;
  // User data #2
  Param2: Pointer;
end;

// Event parameters for device events ( PHX_DEVICE_RESIZED, PHX_DEVICE_RESET, PHX_DEVICE_ACTIVATED, PHX_DEVICE_DEACTIVATED )
//------------------------------------------------------------------------------
TPHXDeviceEvent = record
  // Event type ( PHX_DEVICE_RESIZED, PHX_DEVICE_RESET, PHX_DEVICE_ACTIVATED, PHX_DEVICE_DEACTIVATED )
  Event : TPHXEventType;
  // Shift states
  Shift : TPHXShiftStates;
  // With of the device window
  Width : Integer;
  // Height of the device window
  Height: Integer;
end;

// Union event record
//------------------------------------------------------------------------------
TPHXEvent = record
  case Integer of
  0: (Event    : TPHXEventType; );
  1: (User     : TPHXUserEvent; );
  2: (Mouse    : TPHXMouseEvent; );
  4: (Keyboard : TPHXKeyboardEvent; );
  5: (Joystick : TPHXJoystickEvent);
  6: (Device   : TPHXDeviceEvent);
end;

// Pointer to a phoenix event
PPHXEvent = ^TPHXEvent;

PMethod = ^TMethod;

// Event listener for adding to the event
TPHXEventListener = procedure(Sender: TObject; const Event: TPHXEvent) of object;

//------------------------------------------------------------------------------
TPHXEventInstance = class
  private
    FListeners: TList;
  public
    constructor Create;
    destructor Destroy; override;

    // Add a event listener to the handler
    procedure AddListener(Listener: TPHXEventListener);
    // Remove a event listener from the handler
    procedure RemoveListener(Listener: TPHXEventListener);

    // Add a event
    procedure Notify(Sender: TObject; const Event: TPHXEvent);
  end;

// Singleton class for managing events
//------------------------------------------------------------------------------
TPHXEvents = class
  private
    // Get the current event handler
    class function GetInstance: TPHXEventInstance; static;
    // Set the current event handler
    class procedure SetInstance(const Value: TPHXEventInstance); static;
  public
    // Add a event listener to the active event handler
    class procedure AddListener(Listener: TPHXEventListener); static;
    // Remove a event listener from the active event handler
    class procedure RemoveListener(Listener: TPHXEventListener); static;
  public
    // Notify all listeners of a event
    class procedure Notify(Sender: TObject; const Event: TPHXEvent); static;
    // Add a quit event
    class procedure NotifyQuit(Sender: TObject); static;
    // Add a device reset event
    class procedure NotifyDeviceReset(Sender: TObject; const Width, Height: Integer); static;
    // Add a device reset resized
    class procedure NotifyDeviceResized(Sender: TObject; const Width, Height: Integer); static;
  public
    // Get and set the current event handler
    class property Instance: TPHXEventInstance read GetInstance write SetInstance;
  end;

function PHXEventToString(const Event: TPHXEvent): String;



implementation

// Array to convert a TPHXEventType to a string
//------------------------------------------------------------------------------
const PHXEventTypeNames: Array[TPHXEventType] of String = (
  'PHX_EVENT_NONE',
  'PHX_EVENT_QUIT',
  'PHX_EVENT_USER',

  'PHX_DEVICE_RESET',
  'PHX_DEVICE_RESIZED',
  'PHX_DEVICE_ACTIVATED',
  'PHX_DEVICE_DEACTIVATED',

  'PHX_MOUSE_PRESSED',
  'PHX_MOUSE_RELEASED',
  'PHX_MOUSE_MOVED',
  'PHX_MOUSE_WHEELUP',
  'PHX_MOUSE_WHEELDOWN',

  'PHX_KEY_PRESSED',
  'PHX_KEY_RELEASED',
  'PHX_KEY_TEXT',

  'PHX_JOYSTICK_INFO',
  'PHX_JOYSTICK_AXIS',
  'PHX_JOYSTICK_PRESSED',
  'PHX_JOYSTICK_RELEASED'
);

//------------------------------------------------------------------------------
function PHXEventToString(const Event: TPHXEvent): String;
begin
  Result:= PHXEventTypeNames[Event.Event];

  case Event.Event of
    PHX_EVENT_NONE: ;
    PHX_EVENT_QUIT: ;
    PHX_EVENT_USER: ;
    PHX_DEVICE_RESIZED: ;
    PHX_DEVICE_RESET: ;
    PHX_DEVICE_ACTIVATED: ;
    PHX_DEVICE_DEACTIVATED: ;
    PHX_MOUSE_PRESSED: 
    begin
      Result:= Format('PHX_MOUSE_PRESSED(X:= %d Y:= %d Button:= %d)', [Event.Mouse.X, Event.Mouse.Y, Ord(Event.Mouse.Button)]);
    end;
    PHX_MOUSE_RELEASED:
    begin
      Result:= Format('PHX_MOUSE_RELEASED(X:= %d Y:= %d Button:= %d)', [Event.Mouse.X, Event.Mouse.Y, Ord(Event.Mouse.Button)]);
    end;
    PHX_MOUSE_MOVED: 
    begin
      Result:= Format('PHX_MOUSE_MOVED(X:= %d Y:= %d )', [Event.Mouse.X, Event.Mouse.Y]);
    end;
    PHX_KEY_PRESSED: 
    begin
      Result:= Format('PHX_KEY_PRESSED(Key:= %d)', [Ord(Event.Keyboard.Key)]);
    end;
    PHX_KEY_RELEASED:
    begin
      Result:= Format('PHX_KEY_RELEASED(Key:= %d)', [Ord(Event.Keyboard.Key)]);
    end;
    PHX_KEY_CHARACTER:
    begin
      Result:= Format('PHX_KEY_CHARACTER(Key:= %d)', [Ord(Event.Keyboard.Key)]);
    end;
  end;
end;

// TPHXEventHandler
//==============================================================================
constructor TPHXEventInstance.Create;
begin
  FListeners := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXEventInstance.Destroy;
var Index : Integer;
var Method: PMethod;
begin
  for Index := 0 to -1 + FListeners.Count do
  begin
    Method:= FListeners[Index];
    Dispose(Method);
  end;

  FListeners.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXEventInstance.AddListener(Listener: TPHXEventListener);
var Method: PMethod;
begin
  Method:= New(PMethod);
  Method^.Code := TMethod(Listener).Code;
  Method^.Data := TMethod(Listener).Data;

  FListeners.Add(Method);
end;

//------------------------------------------------------------------------------
procedure TPHXEventInstance.RemoveListener(Listener: TPHXEventListener);
var Index : Integer;
var Method: PMethod;
begin
  for Index:= 0 to FListeners.Count - 1  do
  begin
    if (TMethod(Listener).Code = TMethod(FListeners[Index]^).Code) and (TMethod(Listener).Data = TMethod(FListeners[Index]^).Data) then
    begin
      Method:= FListeners[Index];

      Dispose(Method);

      FListeners.Delete(Index);
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEventInstance.Notify(Sender: TObject; const Event: TPHXEvent);
var Index: Integer;
var Listener: TPHXEventListener;
begin
  for Index:= 0 to FListeners.Count - 1  do
  begin
    Listener:=TPHXEventListener(FListeners[Index]^);

    Listener(Sender, Event);
  end;
end;


var GlobalEvents: TPHXEventInstance;

// TPHXEventManager
//==============================================================================
class procedure TPHXEvents.AddListener(Listener: TPHXEventListener);
begin
  GlobalEvents.AddListener(Listener);
end;

//------------------------------------------------------------------------------
class procedure TPHXEvents.RemoveListener(Listener: TPHXEventListener);
begin
  GlobalEvents.RemoveListener(Listener);
end;


//------------------------------------------------------------------------------
class procedure TPHXEvents.Notify(Sender: TObject; const Event: TPHXEvent);
begin
  GlobalEvents.Notify(Sender, Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXEvents.NotifyQuit(Sender: TObject);
var Event: TPHXEvent;
begin
  FillChar(Event, SizeOf(TPHXEvent), #0);

  Event.Event:= PHX_EVENT_QUIT;

  GlobalEvents.Notify(Sender, Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXEvents.NotifyDeviceReset(Sender: TObject; const Width, Height: Integer);
var Event: TPHXEvent;
begin
  FillChar(Event, SizeOf(TPHXEvent), #0);

  Event.Device.Event := PHX_DEVICE_RESET;
  Event.Device.Width := Width;
  Event.Device.Height:= Height;

  GlobalEvents.Notify(Sender, Event);
end;

//------------------------------------------------------------------------------
class procedure TPHXEvents.NotifyDeviceResized(Sender: TObject; const Width, Height: Integer);
var Event: TPHXEvent;
begin
  FillChar(Event, SizeOf(TPHXEvent), #0);

  Event.Device.Event := PHX_DEVICE_RESIZED;
  Event.Device.Width := Width;
  Event.Device.Height:= Height;

  GlobalEvents.Notify(Sender, Event);
end;

//------------------------------------------------------------------------------
class function TPHXEvents.GetInstance: TPHXEventInstance;
begin
  Result:= GlobalEvents;
end;

//------------------------------------------------------------------------------
class procedure TPHXEvents.SetInstance(const Value: TPHXEventInstance);
begin
  Assert(Value <> nil);

  if GlobalEvents <> Value then
  begin
    GlobalEvents.Free;

    GlobalEvents:= Value;
  end;
end;





initialization
  GlobalEvents:= TPHXEventInstance.Create;
finalization
  GlobalEvents.Free;
end.

