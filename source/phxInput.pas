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
unit phxInput;
//< Input framework

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  {$IFDEF WIN32}
  Windows,
  {$ELSE}
  LclIntf,
  {$ENDIF}

  phxLogger,
  phxTypes,
  phxClasses,
  phxEvents,
  phxDevice;

type

// Forward declarations
TPHXInput       = class;
TPHXInputDevice = class;

// Posssible input states
//------------------------------------------------------------------------------
TPHXInputState = (
  // Unused
  isNone,
  // Up key
  isUp,
  // Down key
  isDown,
  // Left
  isLeft,
  // Right
  isRight,
  // Custom button 1
  isButton1,
  isButton2,
  isButton3,
  isButton4,
  isButton5,
  isButton6,
  isButton7,
  isButton8,
  isButton9,
  isButton10,
  isButton11,
  isButton12,
  isButton13,
  isButton14,
  isButton15,
  isButton16,
  isButton17,
  isButton18,
  isButton19,
  isButton20,
  isButton21,
  isButton22,
  isButton23,
  isButton24,
  isButton25,
  isButton26,
  isButton27,
  isButton28,
  isButton29,
  isButton30,
  isButton31,
  isButton32,
  isButton33,
  isButton34,
  isButton35,
  isButton36,
  isButton37,
  isButton38,
  isButton39,
  isButton40,
  isButton41,
  isButton42,
  isButton43,
  isButton44,
  isButton45,
  isButton46,
  isButton47,
  isButton48,
  isButton49,
  isButton50
);

// Set of input states
TPHXInputStates = set of TPHXInputState;

// Source a binding is originating from
//------------------------------------------------------------------------------
TPHXInputSource = (
  // Unknown source
  isUnknown = 0,
  // The input comes from the keyboard
  isKeyboard = 1,
  // The input comes from the mouse
  isMouse = 2,
  // The input comes from joystick #1
  isJoystick1 = 3,
  // The input comes from joystick #2
  isJoystick2 = 4,
  // The input comes from joystick #3
  isJoystick3 = 5,
  // The input comes from joystick #4
  isJoystick4 = 6,
  // The input comes from joystick #5
  isJoystick5 = 7,
  // The input comes from joystick #6
  isJoystick6 = 8,
  // The input comes from joystick #7
  isJoystick7 = 9,
  // The input comes from joystick #8
  isJoystick8 = 10
);

{$REGION 'TPHXShortCut'}

// Shortcut for bindings and events
//------------------------------------------------------------------------------
TPHXShortCut = record
  public
    // Create a empty shortcut from a string
    class function Create: TPHXShortCut; overload; static;
    // Create a new shortcut from a string
    class function Create(const Text: String): TPHXShortCut; overload; static;
    // Create a new shortcut for a keyboard key
    class function Create(const Key: TPHXVirtualKey): TPHXShortCut; overload; static;
    // Create a new shortcut for a mouse button
    class function Create(const Button: TPHXMouseButton): TPHXShortCut; overload; static;
    // Create a new shortcut for a joystick button
    class function Create(const Button: TPHXJoystickButton; const Index: Integer): TPHXShortCut; overload; static;
  public
    // Returns a string representation of this shortcut
    function ToString: String;
  public
    // Returns if a shortcut equals a key
    class operator Equal(const ShortCut: TPHXShortCut; const Key: TPHXVirtualKey) : Boolean; overload;
    // Returns if a shortcut equals a mouse button
    class operator Equal(const ShortCut: TPHXShortCut; const Button: TPHXMouseButton) : Boolean; overload;
    // Returns if a shortcut equals a joystick button
    class operator Equal(const ShortCut: TPHXShortCut; const Button: TPHXJoystickButton) : Boolean; overload;
  public
    // Source of the input
    case Source: TPHXInputSource of
    // Unknown source
    isUnknown: (Data: Cardinal);
    // Keyboard shortcut
    isKeyboard: (Keyboard: TPHXVirtualKey);
    // Mouse shortcut
    isMouse: (Mouse: TPHXMouseButton);
    // Joystick shortcut
    isJoystick1: (Joystick: TPHXJoystickButton; Index: Byte);
  end;

{$ENDREGION}


{$REGION 'TPHXInputBinding'}

// Binds a input state to a device
//------------------------------------------------------------------------------
TPHXInputBinding = record
  public
    // The input state for the binding
    State: TPHXInputState;
    // Shortcut of the binding
    Shortcut: TPHXShortCut;
  public
    // Create a new input binding from a string
    class function Create(const Text: String): TPHXInputBinding; overload; static;
    // Create a new input binding with a shortcut
    class function Create(const State: TPHXInputState; const Shortcut: TPHXShortCut): TPHXInputBinding; overload; static;
    // Create a new input binding for a key
    class function Create(const State: TPHXInputState; const Key: TPHXVirtualKey): TPHXInputBinding; overload; static;
    // Create a new input binding for a mouse button
    class function Create(const State: TPHXInputState; const Button: TPHXMouseButton): TPHXInputBinding; overload; static;
    // Create a new shortcut for a joystick button
    class function Create(const State: TPHXInputState; const Button: TPHXJoystickButton; const Index: Integer): TPHXInputBinding; overload; static;
  public
    // Returns a string representation of this binding
    function ToString: String;
  end;

PBindingList = ^TBindingList;
TBindingList = array[0..$00FFFFFF] of TPHXInputBinding;

// List of input bindings for the device
//------------------------------------------------------------------------------
TPHXInputBindings = class(TObject)
  private
    FInput   : TPHXInput;
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PBindingList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXInputBinding;
    procedure SetItem(Index: Integer; const Value: TPHXInputBinding);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a new binding list
    constructor Create(AInput: TPHXInput);
    // Destroy the binding list
    destructor Destroy; override;

    // Clears the list
    procedure Clear;

    // Add a new input binding to the list
    procedure Add(const Binding: TPHXInputBinding); overload;
    // Parse and add a binding
    procedure Add(const Text: String); overload;

    // Add a new input binding to the list
    procedure Add(const State: TPHXInputState; const Key: TPHXVirtualKey); overload;
    // Add a new input binding to the list
    procedure Add(const State: TPHXInputState; const KeyA, KeyB: TPHXVirtualKey); overload;
    // Add a new input binding to the list
    procedure Add(const State: TPHXInputState; const KeyA, KeyB, KeyC: TPHXVirtualKey); overload;

    // Add a new input binding to the list
    procedure Add(const State: TPHXInputState; const Button: TPHXMouseButton); overload;
    // Add a new input binding to the list
    procedure Add(const State: TPHXInputState; const ButtonA, ButtonB: TPHXMouseButton); overload;
    // Add a new input binding to the list
    procedure Add(const State: TPHXInputState; const ButtonA, ButtonB, ButtonC: TPHXMouseButton); overload;

    // Add input binding for a joystick button
    procedure Add(const State: TPHXInputState; const Joystick: TPHXJoystickIndex; const Button: TPHXJoystickButton); overload;

    // Deletes a input binding at an index
    procedure Delete(Index: Integer);

    // Deletes all input bindings for a state
    procedure Remove(State: TPHXInputState);

    // The owning input
    property AInput: TPHXInput read FInput;
    // Number of items in the pattern list
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // The internal list of patterns
    property List: PBindingList read FList;
    // Gets and sets the patterns
    property Items[Index: Integer]: TPHXInputBinding read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXInputDevice'}

// Abstract input device
//------------------------------------------------------------------------------
TPHXInputDevice = class
  private
    FInput   : TPHXInput;
    FActive  : Boolean;
    FStates  : TPHXInputStates;
  protected
    // Return the name of this device
    function GetName: String; virtual; abstract;
    // Change the active state of this device
    procedure SetActive(const Value: Boolean); virtual;
  public
    // Create a instance of this device
    constructor Create(AInput: TPHXInput); virtual;
    // Default destructor
    destructor Destroy; override;

    // Update the device, called once every frame
    procedure Update; overload; virtual; abstract;
    // Update the device, called when a event is recieved
    procedure Update(const Event: TPHXEvent); overload; virtual; abstract;
    // Update the device for a input a binding
    procedure Update(const Event: TPHXEvent; const Binding: TPHXInputBinding); overload; virtual; abstract;

    // Owning input
    property Input: TPHXInput read FInput;
    // Name of the input device
    property Name: String read GetName;
    // If the input device is active
    property Active: Boolean read FActive write SetActive;
    // The states of the input device
    property States: TPHXInputStates read FStates write FStates;
  end;

{$ENDREGION}

{$REGION 'TPHXKeyboard'}

// Input device for the keyboard
//------------------------------------------------------------------------------
TPHXKeyboard = class(TPHXInputDevice)
  private
    FKeys: array[TPHXVirtualKey] of Boolean;

    function GetKey(Key: TPHXVirtualKey): Boolean;
  protected
    // Return the name of this device
    function GetName: String; override;
  public
    // Creates the keyboard device
    constructor Create(AInput: TPHXInput); override;
    destructor Destroy; override;

    // Updates the keyboard states and keys.
    procedure Update; override;
    // Update the device, called when a event is recieved
    procedure Update(const Event: TPHXEvent); override;
    // Update the device for a input a binding
    procedure Update(const Event: TPHXEvent; const Binding: TPHXInputBinding); override;

    // The state of the individual keys on the key board, can be a ordinal value ie Ord('A') or a virtual keycode.
    property Keys[Key: TPHXVirtualKey]: Boolean read GetKey; default;
  end;

{$ENDREGION}

{$REGION 'TPHXMouse'}

// Input device for the mouse
//------------------------------------------------------------------------------
TPHXMouse = class(TPHXInputDevice)
  private
    FPosition     : TVector2i;
    FDelta        : TVector2i;
    FPrevious     : TVector2i;

    FButtons: array[TPHXMouseButton] of Boolean;

    function GetWheel: Integer;
    function GetButton(Button: TPHXMouseButton): Boolean;
  protected
     // Return the name of this device
     function GetName: String; override;
  public
    // Creates the mouse device
    constructor Create(AInput: TPHXInput); override;
    destructor Destroy; override;

    // Updates the mouse states and keys.
    procedure Update; override;
    // Update the device, called when a event is recieved
    procedure Update(const Event: TPHXEvent);  override;
    // Update the device for a input a binding
    procedure Update(const Event: TPHXEvent; const Binding: TPHXInputBinding); override;

    // Tests if the mouse is in a rect
    function MouseInRect(Rect: TRecti): Boolean;

    // The position of the mouse on the screen
    property Position: TVector2i read FPosition;
    // The absolute x position of the mouse.
    property X: Integer read FPosition.X;
    // The absolute y position of the mouse.
    property Y: Integer read FPosition.Y;

    // The relative xposition of the mouse since last update.
    property DeltaX: Integer read FDelta.X;
    // The relative yposition of the mouse since last update
    property DeltaY: Integer read FDelta.Y;

    // The position of the mouse wheel.
    property Wheel: Integer read GetWheel;

    // The state of the individual buttons on the mouse.
    property Buttons[Button: TPHXMouseButton]: Boolean read GetButton; default;
  end;

{$ENDREGION}

{$REGION 'TPHXJoystick'}

// Input device for a joystick
//------------------------------------------------------------------------------
TPHXJoystick = class(TPHXInputDevice)
  private
    FIndex: TPHXJoystickIndex;

    FButtons: array[TPHXJoystickButton] of Boolean;
    FAxes   : array[TPHXJoystickAxis] of Integer;

    function GetButton(const Button: TPHXJoystickButton): Boolean;
    function GetAxis  (const Axis  : TPHXJoystickAxis): Integer;
  protected
     // Return the name of this device
     function GetName: String; override;
  public
    // Creates joystick
    constructor Create(AInput: TPHXInput; AIndex: TPHXJoystickIndex); reintroduce;

    // Updates the joystick states and keys.
    procedure Update; override;
    // Update the device, called when a event is recieved
    procedure Update(const Event: TPHXEvent);  override;
    // Update the device for a input a binding
    procedure Update(const Event: TPHXEvent; const Binding: TPHXInputBinding); override;

    // The index of the joystick
    property Index: TPHXJoystickIndex read FIndex;

    // Returns the state of the individual buttons on the joystick.
    property Buttons[const Button: TPHXJoystickButton]: Boolean read GetButton;
    // Returns the position of the individual joystick axis
    property Axes[const Axis: TPHXJoystickAxis]: Integer read GetAxis;
  end;

TJoystickList = array[0..$00FFFFFF] of TPHXJoystick;
PJoystickList = ^TJoystickList;

//------------------------------------------------------------------------------
TPHXJoysticks = class
  private
    FList: TList;
    FInput: TPHXInput;

    // Add the registered joysticks to the list
    procedure AddRegistered;

    function GetCount: Integer;
    function GetList: PJoystickList;
    function GetItem(const Index: Integer): TPHXJoystick;
  public
    // Creates the joystick list
    constructor Create(AInput: TPHXInput);
    // Destroy the joystick list
    destructor Destroy; override;

    // Owning input
    property Input: TPHXInput read FInput;
    // The number of avaiable joysticks
    property Count: Integer read GetCount;
    // Return a pointer to the list of joysticks
    property List: PJoystickList read GetList;
    // Return a joystick from the list
    property Items[const Index: Integer]: TPHXJoystick read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXInputDevices'}

// Pointer a array of input devices
PInputDeviceList = ^TInputDeviceList;
// Array of input devices
TInputDeviceList = array[0..$00FFFFFF] of TPHXInputDevice;

// List of input devices
//------------------------------------------------------------------------------
TPHXInputDevices = class
  private
    FInput: TPHXInput;
    FList : TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPHXInputDevice;
    function GetList: PInputDeviceList;
  public
    // Creates a new input device list
    constructor Create(AInput: TPHXInput);
    // Destroys the input device list
    destructor Destroy; override;

    // Remove and free all devices
    procedure Clear;

    // Add a custom device to the list
    function Add(Device: TPHXInputDevice): TPHXInputDevice; overload;
    // Add a keyboard to the list
    function AddKeyboard: TPHXKeyboard;
    // Add a mouse to the list
    function AddMouse: TPHXMouse;
    // Add a joystick to the list
    function AddJoystick(const Index: TPHXJoystickIndex): TPHXJoystick;

    // Find a device by the name
    function Find(const Name: String; out Device: TPHXInputDevice): Boolean;

    // The owning input class
    property Input: TPHXInput read FInput;
    // Number of input devices in the list
    property Count: Integer read GetCount;
    // Pointer to the internal list for fast access, do not modify
    property List: PInputDeviceList read GetList;
    // Return a input device in the list
    property Items[Index: Integer]: TPHXInputDevice read GetItem;
  end;

{$ENDREGION}

{$REGION 'TPHXInputEvent'}

// The input event is used to add named events to a input
//------------------------------------------------------------------------------
TPHXInputEvent = class
  private
    FInput      : TPHXInput;
    FName       : String;
    FDescription: String;
    FPrimary    : TPHXShortCut;
    FSecondary  : TPHXShortCut;

    FActive : Boolean;
  public
    constructor Create(AInput: TPHXInput);
    destructor Destroy; override;

    // Update the input event
    procedure Update(const Event: TPHXEvent);

    // Name of the input event
    property Name: String read FName write FName;
    // Description of the event
    property Description: String read FDescription write FDescription;
    // Primary shortcut for the event
    property Primary: TPHXShortCut read FPrimary write FPrimary;
    // Secondary shortcut for the event
    property Secondary: TPHXShortCut read FSecondary write FSecondary;

    property Active: Boolean read FActive;
  end;

// Array of input events
TInputEventList = array[0..$00FFFFFF] of TPHXInputEvent;
// Pointer to a array of input events
PInputEventList = ^TInputEventList;

// Contains a list of input events
//------------------------------------------------------------------------------
TPHXInputEvents = class
  private
    FInput: TPHXInput;
    FList : TList;
    function GetCount: Integer;
    function GetList: PInputEventList;
    function GetItem(const Index: Integer): TPHXInputEvent;
    function GetActive(const Name: String): Boolean;
  public
    constructor Create(AInput: TPHXInput);
    destructor Destroy; override;

    // Remove and free all input events
    procedure Clear;

    // Update all input events
    procedure Update(const Event: TPHXEvent);


    function Add(const Name: String; const Description: String): TPHXInputEvent; overload;
    function Add(const Name: String; const Description: String; const Primary: TPHXVirtualKey): TPHXInputEvent; overload;
    function Add(const Name: String; const Description: String; const Primary: TPHXMouseButton): TPHXInputEvent; overload;


    function Find(const Name: String): TPHXInputEvent; overload;

    // Return the number of events in the list
    property Count: Integer read GetCount;
    property List: PInputEventList read GetList;
    property Items[const Index: Integer]: TPHXInputEvent read GetItem;
    // Return the active status of a event by name
    property Events[const Name: String]: Boolean read GetActive; default;
  end;
{$ENDREGION}


{$REGION 'TPHXInput'}

// The input class manages different input devices
//------------------------------------------------------------------------------
TPHXInput = class(TObject)
  private
    FActive  : Boolean;
    FStates  : TPHXInputStates;
    FDevices : TPHXInputDevices;
    FBindings: TPHXInputBindings;
    FEvents  : TPHXInputEvents;

    FKeyboard : TPHXKeyboard;
    FMouse    : TPHXMouse;
    FJoysticks: TPHXJoysticks;

    FDeviceMap: array[TPHXInputSource] of TPHXInputDevice;

    procedure SetActive(const Value: Boolean);
  protected
    // Handles a phoenix event
    procedure EventHandler(Sender: TObject; const Event: TPHXEvent);
  public
    // Creates a new input
    constructor Create;
    // Destroys this input
    destructor Destroy; override;

    // Load the default devices
    procedure DefaultDevices;
    // Load the default key bindings
    procedure DefaultBindings;

    // Update all attached devices and the states
    procedure Update;

    // Load the input configuration from a file
    procedure LoadBindings(const FileName: String); overload;
    // Load the input configuration from a stream
    procedure LoadBindings(Stream: TStream); overload;

    // Save the input configuration to a file
    procedure SaveBindings(const FileName: String); overload;
    // Save the input configuration to a stream
    procedure SaveBindings(Stream: TStream); overload;

    // Get and sets if the input is active
    property Active: Boolean read FActive write SetActive;
    // Get the states of the input device
    property States: TPHXInputStates read FStates write FStates;
    // Get all the attatched devices of this input
    property Devices: TPHXInputDevices read FDevices;
    // Bindings of the input
    property Bindings: TPHXInputBindings read FBindings;
    // List of input events
    property Events: TPHXInputEvents read FEvents;
    // Get the default keyboard
    property Keyboard: TPHXKeyboard read FKeyboard;
    // Get the default mouse
    property Mouse: TPHXMouse read FMouse;
    property Joysticks: TPHXJoysticks read FJoysticks;
  end;

{$ENDREGION}

// Returns the string representation of a input state
function InputStateToString(const Value: TPHXInputState): String;
// Returns the input state for a string
function StringToInputState(const Value: String): TPHXInputState;

// Register a joystick, this is called from the provider during
// initialization.
procedure RegisterJoystick(const Number: TPHXJoystickIndex; const Name: String; const NumAxes, NumButtons: Integer);

implementation

//------------------------------------------------------------------------------
const StateNames: array[TPHXInputState] of String = (
  'isNone',
  'isUp',
  'isDown',
  'isLeft',
  'isRight',
  'isButton1',
  'isButton2',
  'isButton3',
  'isButton4',
  'isButton5',
  'isButton6',
  'isButton7',
  'isButton8',
  'isButton9',
  'isButton10',
  'isButton11',
  'isButton12',
  'isButton13',
  'isButton14',
  'isButton15',
  'isButton16',
  'isButton17',
  'isButton18',
  'isButton19',
  'isButton20',
  'isButton21',
  'isButton22',
  'isButton23',
  'isButton24',
  'isButton25',
  'isButton26',
  'isButton27',
  'isButton28',
  'isButton29',
  'isButton30',
  'isButton31',
  'isButton32',
  'isButton33',
  'isButton34',
  'isButton35',
  'isButton36',
  'isButton37',
  'isButton38',
  'isButton39',
  'isButton40',
  'isButton41',
  'isButton42',
  'isButton43',
  'isButton44',
  'isButton45',
  'isButton46',
  'isButton47',
  'isButton48',
  'isButton49',
  'isButton50'
);

//------------------------------------------------------------------------------
function InputStateToString(const Value: TPHXInputState): String;
begin
  Result:= StateNames[Value];
end;

//------------------------------------------------------------------------------
function StringToInputState(const Value: String): TPHXInputState;
var Index: TPHXInputState;
begin
  for Index:= Low(TPHXInputState) to High(TPHXInputState) do
  begin
    if SameText(StateNames[Index], Value) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= isNone;
end;

//------------------------------------------------------------------------------
var JoystickRegistry : array of record
  // Number of the joystick
  Number: TPHXJoystickIndex;
  // Name of the yoystick
  Name: String;
  // Number of axes for this joystick
  NumAxes: Integer;
  // Number of buttons for this joystick
  NumButtons: Integer;
end;

//------------------------------------------------------------------------------
procedure RegisterJoystick(const Number: TPHXJoystickIndex; const Name: String; const NumAxes, NumButtons: Integer);
var Index: Integer;
begin
  Index:= Length(JoystickRegistry);

  SetLength(JoystickRegistry, Index + 1);

  JoystickRegistry[Index].Number    := Number;
  JoystickRegistry[Index].Name      := Name;
  JoystickRegistry[Index].NumAxes   := NumAxes;
  JoystickRegistry[Index].NumButtons:= NumButtons;
end;



{$REGION 'TPHXShortCut'}

// SizeOf(Result)
// SizeOf(Result.Source)
// SizeOf(Result.Data)
// SizeOf(Result.Keyboard)
// SizeOf(Result.Mouse)
// SizeOf(Result.Joystick)
// SizeOf(Result.Axis)

// TPHXShortCut
//==============================================================================
class function TPHXShortCut.Create: TPHXShortCut;
begin
  Result.Source:= isUnknown;
  Result.Data  := 0;
end;

//------------------------------------------------------------------------------
class function TPHXShortCut.Create(const Text: String): TPHXShortCut;
var Index: Integer;
var Source: String;
var Value : String;
begin
  Result.Source:= isUnknown;
  Result.Data  := 0;

  Index:= Pos(',', Text);

  if Index > 0 then
  begin
    Source:= Trim(Copy(Text, 1, Index-1));
    Value := Trim(Copy(Text, Index + 1, MaxInt));

    Result.Source:= TPHXInputSource(StrToInt(Source));
    case Result.Source of
      isKeyboard : Result.Keyboard:= TPHXVirtualKey(StrToInt(Value));
      isMouse    : Result.Mouse   := TPHXMouseButton(StrToInt(Value));
      isJoystick1: Result.Joystick:= TPHXJoystickButton(StrToInt(Value));
      isJoystick2: Result.Joystick:= TPHXJoystickButton(StrToInt(Value));
      isJoystick3: Result.Joystick:= TPHXJoystickButton(StrToInt(Value));
      isJoystick4: Result.Joystick:= TPHXJoystickButton(StrToInt(Value));
    end;
  end;
end;

//------------------------------------------------------------------------------
class function TPHXShortCut.Create(const Key: TPHXVirtualKey): TPHXShortCut;
begin
  Result.Source := isKeyboard;
  Result.Keyboard:= Key;
end;

//------------------------------------------------------------------------------
class function TPHXShortCut.Create(const Button: TPHXMouseButton): TPHXShortCut;
begin
  Result.Source:= isMouse;
  Result.Mouse := Button;
end;

//------------------------------------------------------------------------------
class function TPHXShortCut.Create(const Button: TPHXJoystickButton; const Index: Integer): TPHXShortCut;
begin
  Assert( (Index >= 0) and (Index < 4));

  Result.Source  := TPHXInputSource(Ord(isJoystick1)+ Index);
  Result.Joystick:= Button;
  Result.Index   := Index;
end;

//------------------------------------------------------------------------------
function TPHXShortCut.ToString: String;
begin
  Result:= '';
  case Source of
    isUnknown  : Result:= IntToStr(Ord(Source));
    isKeyboard : Result:= IntToStr(Ord(Source)) + ',' + IntToStr(Ord(Keyboard));
    isMouse    : Result:= IntToStr(Ord(Source)) + ',' + IntToStr(Ord(Mouse ));
    isJoystick1: Result:= IntToStr(Ord(Source)) + ',' + IntToStr(Ord(Joystick));
    isJoystick2: Result:= IntToStr(Ord(Source)) + ',' + IntToStr(Ord(Joystick));
    isJoystick3: Result:= IntToStr(Ord(Source)) + ',' + IntToStr(Ord(Joystick));
    isJoystick4: Result:= IntToStr(Ord(Source)) + ',' + IntToStr(Ord(Joystick));
  end;
end;

//------------------------------------------------------------------------------
class operator TPHXShortCut.Equal(const ShortCut: TPHXShortCut; const Key: TPHXVirtualKey): Boolean;
begin
  Result:= (ShortCut.Source = isKeyboard) and (ShortCut.Keyboard = Key);
end;

//------------------------------------------------------------------------------
class operator TPHXShortCut.Equal(const ShortCut: TPHXShortCut; const Button: TPHXMouseButton): Boolean;
begin
  Result:= (ShortCut.Source = isMouse) and (ShortCut.Mouse = Button);
end;

//------------------------------------------------------------------------------
class operator TPHXShortCut.Equal(const ShortCut: TPHXShortCut; const Button: TPHXJoystickButton): Boolean;
begin
  Result:= (ShortCut.Source = isJoystick1) and (ShortCut.Joystick = Button);
end;

{$ENDREGION}


{$REGION 'TPHXInputBinding'}

// TPHXInputState=TPHXInputSource,4
// isLeft=2,4

// TPHXInputBinding
//==============================================================================
class function TPHXInputBinding.Create(const Text: String): TPHXInputBinding;
var Index: Integer;
var State: String;
var Shortcut : String;
begin
  Index:= Pos('=', Text);

  Result.State:= isNone;
  Result.Shortcut.Source:= isUnknown;
  Result.Shortcut.Data  := 0;

  if Index > 0 then
  begin
    State   := Trim(Copy(Text, 1, Index-1));
    Shortcut:= Trim(Copy(Text, Index + 1, MaxInt));

    Result.State:= StringToInputState(State);
    Result.Shortcut:= TPHXShortCut.Create(Shortcut);
  end;
end;

//------------------------------------------------------------------------------
class function TPHXInputBinding.Create(const State: TPHXInputState; const Shortcut: TPHXShortCut): TPHXInputBinding;
begin
  Result.State   := State;
  Result.Shortcut:= Shortcut;
end;

//------------------------------------------------------------------------------
class function TPHXInputBinding.Create(const State: TPHXInputState; const Key: TPHXVirtualKey): TPHXInputBinding;
begin
  Result.State   := State;
  Result.Shortcut:= TPHXShortCut.Create(Key);
end;

//------------------------------------------------------------------------------
class function TPHXInputBinding.Create(const State: TPHXInputState; const Button: TPHXMouseButton): TPHXInputBinding;
begin
  Result.State   := State;
  Result.Shortcut:= TPHXShortCut.Create(Button);
end;

//------------------------------------------------------------------------------
class function TPHXInputBinding.Create(const State: TPHXInputState; const Button: TPHXJoystickButton; const Index: Integer): TPHXInputBinding;
begin
  Result.State   := State;
  Result.Shortcut:= TPHXShortCut.Create(Button, Index);
end;

//------------------------------------------------------------------------------
function TPHXInputBinding.ToString: String;
begin
  Result:= InputStateToString(State) + '=' + Shortcut.ToString;
end;



{$ENDREGION}

{$REGION 'TPHXInputBindings'}

// TPHXInputBindings
//==============================================================================
constructor TPHXInputBindings.Create(AInput: TPHXInput);
begin
  FInput   := AInput;
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXInputBindings.Destroy;
begin
  FCount:= 0;

  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Clear;
begin
  FCount:= 0;

  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const Binding: TPHXInputBinding);
begin
  Inc(FCount);

  if FCount > Capacity then Grow;

  FList^[Count - 1]:= Binding;
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const Text: String);
var Binding: TPHXInputBinding;
begin
  Binding:= TPHXInputBinding.Create(Text);

  Add(Binding);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const Joystick: TPHXJoystickIndex; const Button: TPHXJoystickButton);
var Binding: TPHXInputBinding;
begin
  Binding.State  := State;
  Binding.Shortcut:= TPHXShortCut.Create(Button, Ord(Joystick));

  Add(Binding);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const Key: TPHXVirtualKey);
var Binding: TPHXInputBinding;
begin
  Binding.State   := State;
  Binding.Shortcut:= TPHXShortCut.Create(Key);

  Add(Binding);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const KeyA, KeyB: TPHXVirtualKey);
begin
  Add(State, KeyA);
  Add(State, KeyB);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const KeyA, KeyB, KeyC: TPHXVirtualKey);
begin
  Add(State, KeyA);
  Add(State, KeyB);
  Add(State,  KeyC);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const Button: TPHXMouseButton);
var Binding: TPHXInputBinding;
begin
  Binding.State   := State;
  Binding.Shortcut:= TPHXShortCut.Create(Button);

  Add(Binding);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const ButtonA, ButtonB: TPHXMouseButton);
begin
  Add(State, ButtonA);
  Add(State, ButtonB);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Add(const State: TPHXInputState; const ButtonA, ButtonB, ButtonC: TPHXMouseButton);
begin
  Add(State, ButtonA);
  Add(State, ButtonB);
  Add(State, ButtonC);
end;


//------------------------------------------------------------------------------
procedure TPHXInputBindings.Delete(Index: Integer);
begin
  If (Index < 0) or (Index >= FCount) then Exit;

  Dec(FCount);

  System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXInputBinding));
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Remove(State: TPHXInputState);
var Index: Integer;
begin
  Index:= 0;
  while Index < FCount do
  begin

    if FList^[Index].State = State then
    begin
      Dec(FCount);

      System.Move(FList^[Index+1], FList^[Index], (FCount - Index) * SizeOf(TPHXInputBinding));
    end else
    begin
      Inc(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXInputBinding));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXInputBindings.GetItem(Index: Integer): TPHXInputBinding;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXInputBindings.SetItem(Index: Integer; const Value: TPHXInputBinding);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}



{$REGION 'TPHXInputDevice'}

// TPHXInputDevice
//==============================================================================
constructor TPHXInputDevice.Create(AInput: TPHXInput);
begin
  FInput   := AInput;
  FActive  := True;
  FStates  := [];
end;

//------------------------------------------------------------------------------
destructor TPHXInputDevice.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXInputDevice.SetActive(const Value: Boolean);
begin
  FActive:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXInputDevices'}

// TPHXInputDevices
//==============================================================================
constructor TPHXInputDevices.Create(AInput: TPHXInput);
begin
  FInput:= AInput;
  FList := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXInputDevices.Destroy;
begin
  Clear;

  FList.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXInputDevices.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    TPHXInputDevice(FList.List[Index]).Free;
  end;

  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.Add(Device: TPHXInputDevice): TPHXInputDevice;
begin
  Result:= Device;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.AddKeyboard: TPHXKeyboard;
begin
  Result:= TPHXKeyboard.Create(FInput);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.AddMouse: TPHXMouse;
begin
  Result:= TPHXMouse.Create(FInput);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.AddJoystick(const Index: TPHXJoystickIndex): TPHXJoystick;
begin
  Result:= TPHXJoystick.Create(FInput, Index);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.Find(const Name: String; out Device: TPHXInputDevice): Boolean;
var Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Device:= TPHXInputDevice(FList.List[Index]);

    if Device.Name = Name then
    begin
      Exit(True);
    end;
  end;
  Device:= nil;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.GetList: PInputDeviceList;
begin
  Result:= PInputDeviceList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXInputDevices.GetItem(Index: Integer): TPHXInputDevice;
begin
  Assert( (Index >= 0) and (Index < FList.Count), 'List index out of bounds');

  Result:= TPHXInputDevice( FList.List[Index] );
end;

{$ENDREGION}

{$REGION 'TPHXKeyboard'}

// TPHXKeyboard
//==============================================================================
constructor TPHXKeyboard.Create(AInput: TPHXInput);
begin
  inherited Create(AInput);

  FInput.FDeviceMap[isKeyboard]:= Self;
end;

//------------------------------------------------------------------------------
destructor TPHXKeyboard.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TPHXKeyboard.Update;
begin
 // Not used
end;

//------------------------------------------------------------------------------
procedure TPHXKeyboard.Update(const Event: TPHXEvent);
begin
  if not Active then Exit;

  case Event.Event of
    // Activate bindings
    PHX_KEY_PRESSED:
    begin
      // Store the state for the pressed key
      FKeys[Event.Keyboard.Key]:= True;
    end;

    PHX_KEY_RELEASED:
    begin
      // Store the state for the released key
      FKeys[Event.Keyboard.Key]:= False;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXKeyboard.Update(const Event: TPHXEvent; const Binding: TPHXInputBinding);
begin
  if not Active then Exit;

  case Event.Event of
    // Activate bindings
    PHX_KEY_PRESSED:
    begin
      if (Binding.Shortcut = Event.Keyboard.Key) then
      begin
        Include(FStates, Binding.State);

        Include(Input.FStates, Binding.State);
      end;
    end;
    PHX_KEY_RELEASED:
    begin
      if (Binding.Shortcut = Event.Keyboard.Key) then
      begin
        Exclude(FStates, Binding.State);

        Exclude(Input.FStates, Binding.State);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXKeyboard.GetName: String;
begin
  Result:='Keyboard';
end;

//------------------------------------------------------------------------------
function TPHXKeyboard.GetKey(Key: TPHXVirtualKey): Boolean;
begin
  Result:=FKeys[Key];
end;


{$ENDREGION}

{$REGION 'TPHXMouse'}

// TPHXMouse
//==============================================================================
constructor TPHXMouse.Create(AInput: TPHXInput);
begin
  inherited Create(AInput);

  FPosition.X     := 0;
  FPosition.Y     := 0;
  FActive         := True;

  FInput.FDeviceMap[isMouse]:= Self;
end;

//------------------------------------------------------------------------------
destructor TPHXMouse.Destroy;
begin

  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TPHXMouse.Update;
begin
  if not Active then Exit;

  // Calculate the mouse deltas
  FDelta.X:= FPosition.X - FPrevious.X;
  FDelta.Y:= FPosition.Y - FPrevious.Y;

  FPrevious:= FPosition;

  // Update the states
//  if FDelta.X < 0 then Include(FStates, isLeft ) else Exclude(FStates, isLeft);
 // if FDelta.X > 0 then Include(FStates, isRight) else Exclude(FStates, isRight);
 // if FDelta.Y < 0 then Include(FStates, isUp   ) else Exclude(FStates, isUp);
 // if FDelta.Y > 0 then Include(FStates, isDown ) else Exclude(FStates, isDown);
end;

//------------------------------------------------------------------------------
procedure TPHXMouse.Update(const Event: TPHXEvent);
begin
  if not Active then Exit;

  case Event.Event of
    PHX_MOUSE_PRESSED:
    begin
      // Store the state for the button
      FButtons[Event.Mouse.Button]:= True;
    end;
    PHX_MOUSE_RELEASED:
    begin
      // Store the state for the button
      FButtons[Event.Mouse.Button]:= False;
    end;
    PHX_MOUSE_MOVED:
    begin
      FPosition.X:= Event.Mouse.X;
      FPosition.Y:= Event.Mouse.Y;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXMouse.Update(const Event: TPHXEvent; const Binding: TPHXInputBinding);
begin
  if not Active then Exit;

  case Event.Event of
    PHX_MOUSE_PRESSED:
    begin
      if (Binding.Shortcut = Event.Mouse.Button) then
      begin
        Include(FStates, Binding.State);

        Include(Input.FStates, Binding.State);
      end;
    end;
    PHX_MOUSE_RELEASED:
    begin
      if (Binding.Shortcut = Event.Mouse.Button) then
      begin
        Exclude(FStates, Binding.State);

        Exclude(Input.FStates, Binding.State);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXMouse.MouseInRect(Rect: TRecti): Boolean;
begin
  Result:= (X >= Rect.Left  ) and
           (X <= Rect.Right ) and
           (Y >= Rect.Top   ) and
           (Y <= Rect.Bottom);
end;

//------------------------------------------------------------------------------
function TPHXMouse.GetName: String;
begin
  Result:='Mouse';
end;

//------------------------------------------------------------------------------
function TPHXMouse.GetWheel: Integer;
begin
  Result:= 0; //glfwGetMouseWheel;
end;

//------------------------------------------------------------------------------
function TPHXMouse.GetButton(Button: TPHXMouseButton): Boolean;
begin
  Result:= FButtons[Button];
end;


{$ENDREGION}

{$REGION 'TPHXJoystick'}

// TPHXJoystick
//==============================================================================
constructor TPHXJoystick.Create(AInput: TPHXInput; AIndex: TPHXJoystickIndex);
begin
  inherited Create(AInput);
  FIndex:= AIndex;

  FInput.FDeviceMap[  TPHXInputSource( Ord(isJoystick1) + Ord(AIndex))  ]:= Self;
end;

//------------------------------------------------------------------------------
procedure TPHXJoystick.Update;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXJoystick.Update(const Event: TPHXEvent);
begin
  if Event.Joystick.Index <> Self.Index then Exit;

  case Event.Event of
    PHX_JOYSTICK_AXIS:
    begin

      FAxes[Event.Joystick.Axis]:= Event.Joystick.Position;
    end;
    PHX_JOYSTICK_PRESSED:
    begin
      FButtons[Event.Joystick.Button]:= True;
    end;
    PHX_JOYSTICK_RELEASED:
    begin
      FButtons[Event.Joystick.Button]:= False;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXJoystick.Update(const Event: TPHXEvent; const Binding: TPHXInputBinding);
begin
 if Event.Joystick.Index <> Self.Index then Exit;

  if not Active then Exit;

  case Event.Event of
    PHX_JOYSTICK_PRESSED:
    begin
      if (Binding.Shortcut = Event.Joystick.Button) then
      begin
        Include(FStates, Binding.State);

        Include(Input.FStates, Binding.State);
      end;
    end;
    PHX_JOYSTICK_RELEASED:
    begin
      if (Binding.Shortcut = Event.Joystick.Button) then
      begin
        Exclude(FStates, Binding.State);

        Exclude(Input.FStates, Binding.State);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXJoystick.GetName: String;
begin
  Result:='Joystick'+ IntToStr(Ord(FIndex));
end;

//------------------------------------------------------------------------------
function TPHXJoystick.GetButton(const Button: TPHXJoystickButton): Boolean;
begin
  Result:= FButtons[Button];
end;

//------------------------------------------------------------------------------
function TPHXJoystick.GetAxis(const Axis: TPHXJoystickAxis): Integer;
begin
  Result:= FAxes[Axis];
end;

{$ENDREGION}

{$REGION 'TPHXJoysticks'}

// TPHXJoysticks
//==============================================================================
constructor TPHXJoysticks.Create(AInput: TPHXInput);
begin
  FInput:= AInput;
  FList := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXJoysticks.Destroy;
begin
  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXJoysticks.AddRegistered;
var Index: Integer;
begin
  for Index := Low(JoystickRegistry) to High(JoystickRegistry) do
  begin
    Input.Devices.AddJoystick( JoystickRegistry[Index].Number );
  end;
end;

//------------------------------------------------------------------------------
function TPHXJoysticks.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXJoysticks.GetList: PJoystickList;
begin
  Result:= PJoystickList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXJoysticks.GetItem(const Index: Integer): TPHXJoystick;
begin
  Result:= TPHXJoystick(FList.List[Index]);
end;



{$ENDREGION}

{$REGION 'TPHXInputEvent'}

(*



  Events.MoveLeft := Input.Events.Add('left' , 'Move the player left' , VK_A);
  Events.MoveRight:= Input.Events.Add('right', 'Move the player right', VK_D);


  if Input.Events['left'] then
  begin
  end;

  if Events.MoveLeft.Active then
  begin
  end;


  Lua:


  if input.events['left'] then

  end

*)

// TPHXInputEvent
//==============================================================================
constructor TPHXInputEvent.Create(AInput: TPHXInput);
begin
  FInput      := AInput;
  FName       := '';
  FDescription:= '';
  FPrimary    := TPHXShortCut.Create;
  FSecondary  := TPHXShortCut.Create;

  FActive := False;
end;

//------------------------------------------------------------------------------
destructor TPHXInputEvent.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXInputEvent.Update(const Event: TPHXEvent);
begin
  case Event.Event of
    // Activate bindings
    PHX_KEY_PRESSED:
    begin
      if (Primary = Event.Keyboard.Key) or (Secondary = Event.Keyboard.Key) then
      begin
        FActive:= True;
      end;
    end;
    PHX_KEY_RELEASED:
    begin
     if (Primary = Event.Keyboard.Key) or (Secondary = Event.Keyboard.Key) then
      begin
        FActive:= False;
      end;
    end;
    PHX_MOUSE_PRESSED:
    begin
      if (Primary = Event.Mouse.Button) or (Secondary = Event.Mouse.Button) then
      begin
         FActive:= True;
      end;
    end;
    PHX_MOUSE_RELEASED:
    begin
      if (Primary = Event.Mouse.Button) or (Secondary = Event.Mouse.Button) then
      begin
        FActive:= False;
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TPHXInputEvents'}

// TPHXInputEvents
//==============================================================================
constructor TPHXInputEvents.Create(AInput: TPHXInput);
begin
  FInput:= AInput;
  FList := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXInputEvents.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;



//------------------------------------------------------------------------------
procedure TPHXInputEvents.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    TPHXInputEvent(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
procedure TPHXInputEvents.Update(const Event: TPHXEvent);
var Index: Integer;
var Item : TPHXInputEvent;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Item:= TPHXInputEvent(FList.List[Index]);
    Item.Update(Event);
  end;
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.Add(const Name: String; const Description: String): TPHXInputEvent;
begin
  Result:= TPHXInputEvent.Create(FInput);
  Result.Name       := Name;
  Result.Description:= Description;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.Add(const Name: String; const Description: String; const Primary: TPHXVirtualKey): TPHXInputEvent;
begin
  Result:= TPHXInputEvent.Create(FInput);
  Result.Name       := Name;
  Result.Description:= Description;
  Result.Primary    := TPHXShortCut.Create(Primary);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.Add(const Name: String; const Description: String;  const Primary: TPHXMouseButton): TPHXInputEvent;
begin
  Result:= TPHXInputEvent.Create(FInput);
  Result.Name       := Name;
  Result.Description:= Description;
  Result.Primary    := TPHXShortCut.Create(Primary);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.Find(const Name: String): TPHXInputEvent;
var Index: Integer;
var Item : TPHXInputEvent;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Item:= TPHXInputEvent(FList.List[Index]);

    if Item.Name = Name then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.GetActive(const Name: String): Boolean;
var Event: TPHXInputEvent;
begin
  Event:= Find(Name);

  if Assigned(Event) then
  begin
    Result:= Event.Active;
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXInputEvents.GetList: PInputEventList;
begin
  Result:= PInputEventList(FList.List);
end;


//------------------------------------------------------------------------------
function TPHXInputEvents.GetItem(const Index: Integer): TPHXInputEvent;
begin
  Result:= TPHXInputEvent(FList.List[Index]);
end;

{$ENDREGION}

{$REGION 'TPHXInput'}

// TPHXInput
//==============================================================================
constructor TPHXInput.Create;
begin
  FActive   := True;
  FStates   := [];
  FDevices  := TPHXInputDevices.Create(Self);
  FBindings := TPHXInputBindings.Create(Self);
  FEvents   := TPHXInputEvents.Create(Self);
  FJoysticks:= TPHXJoysticks.Create(Self);

  DefaultDevices;
  DefaultBindings;

  TPHXEvents.AddListener(EventHandler);
end;

//------------------------------------------------------------------------------
destructor TPHXInput.Destroy;
begin
  TPHXEvents.RemoveListener(EventHandler);

  FDevices.Free;
  FBindings.Free;
  FEvents.Free;
  FJoysticks.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.EventHandler(Sender: TObject; const Event: TPHXEvent);
var Index : Integer;
var Device: TPHXInputDevice;
var Source: TPHXInputSource;
begin
  if Active then
  begin
    // Update the devices
    for Index:=0 to FDevices.Count-1 do
    begin
      Device:= TPHXInputDevice( FDevices.List^[Index] );

      Device.Update(Event);
    end;

    // Update the bindings
    for Index:= 0 to Bindings.Count - 1 do
    begin
      Source:= Bindings.List^[Index].Shortcut.Source;

      Device:= FDeviceMap[Source];

      if Assigned(Device) then
      begin
        Device.Update(Event, Bindings.List^[Index]);
      end;
    end;

    Events.Update(Event);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.DefaultDevices;
begin
  FDevices.Clear;

  FKeyboard:= FDevices.AddKeyboard;
  FMouse   := FDevices.AddMouse;

  Joysticks.AddRegistered;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.DefaultBindings;
begin
  Bindings.Clear;

  // Add keyboard bindings
  Bindings.Add(isLeft   , VK_LEFT  , VK_A, VK_NUM_4);
  Bindings.Add(isRight  , VK_RIGHT , VK_D, VK_NUM_6);
  Bindings.Add(isUp     , VK_UP    , VK_W, VK_NUM_8);
  Bindings.Add(isDown   , VK_DOWN  , VK_S, VK_NUM_2);
  Bindings.Add(isButton1, VK_SPACE , VK_NUM_0);
  Bindings.Add(isButton2, VK_RETURN, VK_NUM_5);

  // Add mouse button bindings
  Bindings.Add(isButton1, mbLeft);
  Bindings.Add(isButton2, mbRight);
  Bindings.Add(isButton3, mbMiddle);
  (*
  Bindings.Add(isButton4, FMouse, PHX_MOUSE_BUTTON_4);
  Bindings.Add(isButton5, FMouse, PHX_MOUSE_BUTTON_5);
  Bindings.Add(isButton6, FMouse, PHX_MOUSE_BUTTON_6);
  Bindings.Add(isButton7, FMouse, PHX_MOUSE_BUTTON_7);
  Bindings.Add(isButton8, FMouse, PHX_MOUSE_BUTTON_8);
  *)
{
  Result[isShift  ]:= KeyBinding(VK_LSHIFT , VK_RSHIFT);
  Result[isCtrl   ]:= KeyBinding(VK_LCTRL  , VK_RCTRL );
  Result[isAlt    ]:= KeyBinding(VK_LALT   , VK_RALT  );
     }
end;

//------------------------------------------------------------------------------
procedure TPHXInput.Update;
var Index : Integer;
var Device: TPHXInputDevice;
begin
  // Update all devices
  for Index:=0 to FDevices.Count-1 do
  begin
    Device:= TPHXInputDevice( FDevices.List^[Index] );
    // Update the device
    Device.Update;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.LoadBindings(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckText, fmOpenRead or fmShareDenyNone);
  try
    LoadBindings(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXInput.LoadBindings(Stream: TStream);
var Lines  : TStringList;
var Index  : Integer;
var Line   : String;
var Binding: TPHXInputBinding;
begin
  Lines:= TStringList.Create;
  try
    Lines.LoadFromStream(Stream);

    Bindings.Clear;
    for Index:=0 to Lines.Count-1 do
    begin
      Line:= Lines[Index];
      if Line <> '' then
      begin
        Binding:= TPHXInputBinding.Create(Line);

        Bindings.Add(Binding);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.SaveBindings(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckText, fmCreate);
  try
    SaveBindings(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.SaveBindings(Stream: TStream);
var Lines  : TStringList;
var Index  : Integer;
var Binding: TPHXInputBinding;
begin
  Lines:= TStringList.Create;
  try
    for Index:=0 to FBindings.Count-1 do
    begin
      Binding:= FBindings.List^[Index];

      Lines.Add(Binding.ToString);
    end;

    Lines.SaveToStream(Stream);
  finally
    Lines.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXInput.SetActive(const Value: Boolean);
var Index : Integer;
var Device: TPHXInputDevice;
begin
  FActive := Value;

  // Inactivate all devices
  for Index:=0 to FDevices.Count-1 do
  begin
    Device:= TPHXInputDevice(FDevices.List^[Index]);
    Device.SetActive(FActive);
  end;
end;

{$ENDREGION}










end.
