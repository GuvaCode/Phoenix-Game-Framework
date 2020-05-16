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
unit phxScript;
//< Scripting support using Lua

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes, Variants,

  Lua,

  phxTypes,
  phxTimer,
  phxInput,
  phxAudio;

const
// Name for the timer namespace
LIBRARY_TIMER = 'timer';
// Name for the input namespace
LIBRARY_INPUT = 'input';
// Name for the audio namespace
LIBRARY_AUDIO = 'audio';

type

// Forward
TPHXScript = class;
TPHXScriptParams = class;
TPHXScriptResults = class;

// Lua state
TLuaState = PLuaState;
// Lua object function
TLuaFunction = function(State: TLuaState): Integer of object;
// Function for pushing a object onto the lua stack
TLuaPushFunction = procedure(State: TLuaState; Instance: TObject);


// Exception for lua errors
EPHXScriptError = class(Exception);
// Event for script errors
TPHXScriptErrorEvent = procedure(Engine: TPHXScript; const Error: String) of object;
// Event that is called by lua print function
TPHXScriptPrintEvent = procedure(Engine: TPHXScript; const Text: String) of object;

// A function that can be registered with a script
TPHXScriptFunction = procedure(Params: TPHXScriptParams; Results: TPHXScriptResults) of object;


{$REGION 'TPHXScriptParam'}

// Helper class for reading a value from the lua stack
//------------------------------------------------------------------------------
TPHXScriptParam = class
  private
    function GetBoolean: Boolean;
    function GetInteger: Integer;
    function GetNumber: Double;
    function GetString: String;
  protected
    // The owning lua state
    State: PLuaState;
    // The index of the parameter on the stack
    Index: Integer;
  public
    // Create the script parameter
    constructor Create;
    // Destroy this script parameter
    destructor Destroy; override;

    // Return a parameter as string
    property AsBoolean: Boolean read GetBoolean;
    // Return a parameter as string
    property AsInteger: Integer read GetInteger;
    // Return a parameter as string
    property AsNumber: Double read GetNumber;
    // Return a parameter as string
    property AsString: String read GetString;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptParams'}

// Helper class for reading parameters to a scriptable function
//------------------------------------------------------------------------------
TPHXScriptParams = class
  private
    FState: PLuaState;
    FCount: Integer;
    FParam: TPHXScriptParam;

    function GetParam(const Index: Integer): TPHXScriptParam;

    procedure SetState(const Value: PLuaState);
  public
    // Create the script params
    constructor Create(const AState: PLuaState);
    // Destroy this script parameters
    destructor Destroy; override;

    // The owning lua state
    property State: PLuaState read FState write SetState;
    // Number of parameters
    property Count: Integer read FCount;
    // Return a parameter from the lua stack
    property Params[const Index: Integer]: TPHXScriptParam read GetParam; default;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptResults'}

// Helper class for setting results from a scriptable function
//------------------------------------------------------------------------------
TPHXScriptResults = class
  private
    FState: PLuaState;
    FCount: Integer;
    procedure SetState(const Value: PLuaState);
  public
    // Create the script results
    constructor Create(const AState: PLuaState);

    // Add a boolean value to the result
    procedure Add(const Value: Boolean); overload;
    // Add a integer value to the result
    procedure Add(const Value: Integer); overload;
    // Add a float value to the result
    procedure Add(const Value: Double); overload;
    // Add a string value to the result
    procedure Add(const Value: String); overload;

    // Add a array of boolean values as a lua table to the results
    procedure Add(const Values: array of Boolean); overload;
    // Add a array of integer values as a lua table to the results
    procedure Add(const Values: array of Integer); overload;
    // Add a array of double values as a lua table to the results
    procedure Add(const Values: array of Double); overload;
    // Add a array of string values as a lua table to the results
    procedure Add(const Values: array of String); overload;

    // The owning lua state
    property State: PLuaState read FState write SetState;
    // Number of returned results
    property Count: Integer read FCount;
  end;

{$ENDREGION}


{$REGION 'TPHXScript'}

// Class for implement scripting support via lua
//------------------------------------------------------------------------------
TPHXScript = class(TObject)
  private
    // Name of the script engine
    FName: String;
    // Source code of the script
    FSource: AnsiString;
    // The lua state of the script engine
    FState: TLuaState;
    //

    // Event for script errors
    FOnError: TPHXScriptErrorEvent;
    // Messages from the lua print function
    FOnPrint: TPHXScriptPrintEvent;

    // Check the status of a lua function and generate a error if needed
    procedure CheckStatus(Status: Integer);

    function GetVariable(const Name: AnsiString): Variant;
  protected
    procedure Print(const Text: String);
  public
    // Create a new plugin list
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    // Register a timer for scripting
    // @param(Timer the timer to register)
    // @param(Name name of the lua table)
    procedure Register(const Timer: TPHXTimer; const Name: AnsiString = LIBRARY_TIMER); overload;
    // Register a input for scripting
    // @param(Input the input to register)
    // @param(Name name of the lua table)
    procedure Register(const Input: TPHXInput; const Name: AnsiString = LIBRARY_INPUT); overload;
    // Register a audio engine for scripting
    // @param(Audio the audio engine to register)
    // @param(Name name of the lua table)
    procedure Register(const Audio: TPHXAudioEngine; const Name: AnsiString = LIBRARY_AUDIO); overload;
    // Register a TPersistent instance for scripting using RTTI
    // @param(Instance the instance to register)
    // @param(Name name of the lua table)
    procedure Register(const Instance: TPersistent; const Name: AnsiString); overload;
    // Register a script function with lua
    // @param(Name name of the function in lua)
    // @param(Event the method to call)
    procedure Register(const Event: TPHXScriptFunction; const Name: AnsiString); overload;
    // Register a object function with lua
    procedure Register(const Event: TLuaFunction; const Name: AnsiString); overload;
    // Register a static function with lia
    procedure Register(const Event: lua_CFunction; const Name: AnsiString); overload;


    // Load the script source from a file
    procedure LoadFromFile(const FileName: String);
    // Load the script source from a stram
    procedure LoadFromStream(const Stream: TStream);
    // Load the script source from a string
    procedure LoadFromString(const Source: String);

    // Compile the script source
    procedure Compile; overload;
    // Compile the script with a new source
    procedure Compile(const Source: AnsiString); overload;

    // Execute a script method
    procedure Run(const Method: AnsiString); overload;
    // Execute a script method with a integer parameter
    procedure Run(const Method: AnsiString; const Param: Integer); overload;
    // Execute a script method with a number parameter
    procedure Run(const Method: AnsiString; const Param: Double); overload;
    // Execute a script method with a boolean parameter
    procedure Run(const Method: AnsiString; const Param: Boolean); overload;
    // Execute a script method with a string parameter
    procedure Run(const Method: AnsiString; const Param: AnsiString); overload;

    // List all the functions in the script
    procedure ListFunctions(List: TStrings);
    // List all global variables
    procedure ListGlobals(List: TStrings);

    // Name of the script engine
    property Name: String read FName write FName;
    // Source code of the script
    property Source: AnsiString read FSource write FSource;
    // The lua state of the script engine
    property State: TLuaState read FState;

    // Event for script errors
    property OnError: TPHXScriptErrorEvent read FOnError write FOnError;
    // Messages from the lua print function
    property OnPrint: TPHXScriptPrintEvent read FOnPrint write FOnPrint;

    // Read a variable from the lua state
    property Variables[const Name: AnsiString]: Variant read GetVariable;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptFiler'}

// Property information about a lua object
//------------------------------------------------------------------------------
TPHXScriptProp = record
  Hash: Cardinal;
  Name: String;
end;

//------------------------------------------------------------------------------
TPHXScriptFiler = class
  private
    FState: PLua_State;
    FTop  : Integer;
	  FIndex: Integer;

    FPropName: String;
    FPropHash: Cardinal;
  public
	  constructor Create(AState: PLua_State; AIndex: Integer);

    // Calculates the number of elements added or removed from the lua stack
    function GetStackDelta: Integer;

    // The lua state of the property
    property State: Plua_State read FState;
    // Index in the stack to read the value
    property Index: Integer read FIndex;
    // The top position in the stack
    property Top: Integer read FTop;

    // Name of the property
    property PropName: String read FPropName write FPropName;
    // Hash code of the property
    property PropHash: Cardinal read FPropHash write FPropHash;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptReader'}

// Helper class for reading and writing properties to a script
//------------------------------------------------------------------------------
TPHXScriptReader = class(TPHXScriptFiler)
  public
    // Read a integer value
    function ReadInteger: Integer;
    // Read a double value
    function ReadNumber: Double;
    // Read a string value
    function ReadString: String;
    // Read a boolean value
    function ReadBoolean: Boolean;
    // Read a pointer value
    function ReadPointer: Pointer;
    // Read a color
    function ReadColor: TColor32;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptWriter'}

//------------------------------------------------------------------------------
TPHXScriptWriter = class(TPHXScriptFiler)
  public
     // Push a null value onto the stack
    procedure WriteNull;
    // Push a double onto the stack
    procedure WriteNumber(const Value: Double);
    // Push a integer onto the stack
    procedure WriteInteger(const Value: Integer);
    // Push a string onto the stack
    procedure WriteString(const Value: String);
    // Push a boolean onto the stack
    procedure WriteBoolean(const Value: Boolean);
    // Push a pointer onto the stack
    procedure WritePointer(const Value: Pointer);
    // Push a method callback onto the stack
    procedure WriteMethod(const Value: TLuaFunction); overload;
    // Push a method callback onto the stack
    procedure WriteMethod(const Value: lua_CFunction); overload;
    // Push a color onto the stack
    procedure WriteColor(const Value: TColor32);
    // Push a TStrings object onto the stack
    procedure WriteStrings(const Value: TStrings);
  end;

{$ENDREGION}

//------------------------------------------------------------------------------
TPHXScriptValueKind = (
  // Integer
  vkInteger,
  // String
  vkString,
  // Double
  vkNumber,
  // Boolean
  vkBoolean
);

{$REGION 'TPHXScriptConstant'}

// Registers a constant in a script
//------------------------------------------------------------------------------
TPHXScriptConstant = record
  Name: String;
  Hash: Cardinal;
  Kind: TPHXScriptValueKind;
  // Pointer to the value
  Value: Pointer;
end;

PPHXScriptConstant = ^TPHXScriptConstant;

PScriptConstantList = ^TScriptConstantList;
TScriptConstantList = array[0..$00FFFFFF] of TPHXScriptConstant;

//------------------------------------------------------------------------------
TPHXScriptConstants = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PScriptConstantList;

    procedure Grow;

    function GetItem(const Index: Integer): TPHXScriptConstant;

    procedure SetCapacity(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(const Name: String; const Kind: TPHXScriptValueKind; Value: Pointer);

    function Find(const Name: String): PPHXScriptConstant;

    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property List: PScriptConstantList read FList;
    property Items[const Index: Integer]: TPHXScriptConstant read GetItem; default;
  end;


{$ENDREGION}

{$REGION 'TPHXScriptProperty'}

//------------------------------------------------------------------------------
TPHXScriptProperty = record
  // Name of the property
  Name: String;
  // Hash code of the property name
  Hash: Cardinal;
  // The data type of the property
  Kind: TPHXScriptValueKind;
  // Pointer to the reader method
  Reader: Pointer;
  // Pointer to the writer method
  Writer: Pointer;
end;

PPHXScriptProperty = ^TPHXScriptProperty;

PScriptPropertyList = ^TScriptPropertyList;
TScriptPropertyList = array[0..$00FFFFFF] of TPHXScriptProperty;

// List of script properties
//------------------------------------------------------------------------------
TPHXScriptProperties = class
  private
    FList: PScriptPropertyList;
    FCount: Integer;
    FCapacity: Integer;

    procedure Grow;

    function GetItem(const Index: Integer): TPHXScriptProperty;

    procedure SetCapacity(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(const Name: String; const Kind: TPHXScriptValueKind; Reader: Pointer; Writer: Pointer);

    function Find(const Name: String): PPHXScriptProperty; overload;
    function Find(const Name: String; out Prop: PPHXScriptProperty): Boolean; overload;

    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property List: PScriptPropertyList read FList;
    property Items[const Index: Integer]: TPHXScriptProperty read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptMethod'}

//------------------------------------------------------------------------------
TPHXScriptMethod = record
  Name: String;
  Hash: Cardinal;
  // TPHXScriptMethod
  Method: Pointer;
end;

PPHXScriptMethod = ^TPHXScriptMethod;

PScriptMethodList = ^TScriptMethodList;
TScriptMethodList = array[0..$00FFFFFF] of TPHXScriptMethod;

// List of script methods
//------------------------------------------------------------------------------
TPHXScriptMethods = class
  private
    FCount   : Integer;
    FCapacity: Integer;
    FList    : PScriptMethodList;

    procedure Grow;

    function GetItem(const Index: Integer): TPHXScriptMethod;

    procedure SetCapacity(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(const Name: String; const Method: Pointer);

    function Find(const Name: String): PPHXScriptMethod; overload;
    function Find(const Name: String; out Method: PPHXScriptMethod): Boolean; overload;

    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property List: PScriptMethodList read FList;
    property Items[const Index: Integer]: TPHXScriptMethod read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptClass'}

//------------------------------------------------------------------------------
TPHXScriptClass = class
  private
    FName: String;
    FHash: Cardinal;
    FProperties: TPHXScriptProperties;
    FMethods: TPHXScriptMethods;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddProperty(const Name: String; const Kind: TPHXScriptValueKind; Reader: Pointer; Writer: Pointer);
    // Add a method to the script class
    procedure AddMethod(const Name: String; Method: Pointer);

    // Name of the class (Should be the classname)
    property Name: String read FName write FName;
    // Hash code of the name
    property Hash: Cardinal read FHash write FHash;
    // Properties for the script class
    property Properties: TPHXScriptProperties read FProperties;
    // Methods for the script class
    property Methods: TPHXScriptMethods read FMethods;
  end;

//------------------------------------------------------------------------------
TPHXScriptClasses = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const Name: String): TPHXScriptClass;

    function Find(const Name: String): TPHXScriptClass;
  end;

{$ENDREGION}

{$REGION 'TPHXScriptRegistry'}

//------------------------------------------------------------------------------
TPHXScriptRegistry = class
  private
    FConstants: TPHXScriptConstants;
    FMethods  : TPHXScriptMethods;
    FClasses  : TPHXScriptClasses;
  public
    constructor Create;
    destructor Destroy; override;

    function AddClass(const Name: String): TPHXScriptClass;
    // Add a method to the script class
    procedure AddMethod(const Name: String; Method: Pointer);
    // Add a method to the script class
    procedure AddConstant(const Name: String; const Kind: TPHXScriptValueKind; Value: Pointer);

    // Public constants
    property Constants: TPHXScriptConstants read FConstants;
    // Public methods
    property Methods: TPHXScriptMethods read FMethods;
    // All registered classes
    property Classes: TPHXScriptClasses read FClasses;
  end;
{$ENDREGION}

//------------------------------------------------------------------------------
TPHXScriptEngine = class
  private
    FName: String;
    FSource: String;

    FOnPrint: TPHXScriptPrintEvent;
    FOnError: TPHXScriptErrorEvent;
  protected
    procedure DoPrint(const Text: String);
    procedure DoError(const Error: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Compile the script
    procedure Compile; virtual; abstract;

    procedure SetVariable(const Name: String; const Value: Integer); overload; virtual; abstract;
    procedure SetVariable(const Name: String; const Value: String); overload; virtual; abstract;
    procedure SetVariable(const Name: String; const Value: Double); overload; virtual; abstract;
    procedure SetVariable(const Name: String; const Value: Boolean); overload; virtual; abstract;
    procedure SetVariable(const Name: String; const Value: TObject); overload; virtual; abstract;

    property Name: String read FName write FName;
    // The script source
    property Source: String read FSource write FSource;

    // Event for script errors
    property OnError: TPHXScriptErrorEvent read FOnError write FOnError;
    // Messages from the lua print function
    property OnPrint: TPHXScriptPrintEvent read FOnPrint write FOnPrint;
  end;


var ScriptRegistry: TPHXScriptRegistry;

// Push a delphi string on the a lua stack
procedure lua_pushstring(L: PLuaState; const Value: WideString); overload;
// Push a delphi string on the a lua stack
procedure lua_pushstring(L: PLuaState; const Value: AnsiString); overload;
// Push a delphi object function on the lua stack
procedure lua_pushfunction(L: PLuaState; const Funct: TLuaFunction);
// Push a TStrings object on the lua stack
procedure lua_pushStrings(L: PLua_State; Strings: TStrings);

function lua_getErrorText(L: Plua_State; Status: Integer): String;

implementation

uses phxScriptSupport;

var ScriptParams : TPHXScriptParams;
var ScriptResults: TPHXScriptResults;

var LibraryRegistry : array of record
  Name: String;
  Funct: TLuaPushFunction;
end;

procedure RegisterScriptable(const Name: String; const Funct: TLuaPushFunction);
var Index: Integer;
begin
  Index:= Length(LibraryRegistry);

  SetLength(LibraryRegistry, Index+1);

  LibraryRegistry[Index].Name := Name;
  LibraryRegistry[Index].Funct:= Funct;
end;
      (*
procedure Lua_pushScriptable(L: PLuaState; const Instance: TObject);
var Name : String;
var Index: Integer;
//var ObjectIf: IPHXScriptObject;
begin
  Name:= Instance.ClassName;

  for Index := Low(LibraryRegistry) to High(LibraryRegistry) do
  begin
    if LibraryRegistry[Index].Name = Name then
    begin
      LibraryRegistry[Index].Funct(L, Instance);

      Exit;
    end;
  end;

  raise EPHXScriptError.CreateFmt('The class %s is not registered as a scriptable object.', [Name]);

 // if not Supports(Instance, IPHXScriptObject, ObjectIf) then
  //begin
 //   raise EPHXScriptError.Create('The instance doesnt support the ILuaObject interface.');
  //end;
end;

procedure Test;
var Input: TPHXInput;
var L: PLuaState;
begin
//  RegisterScriptable('TStrings'   , lua_pushStrings);
 // RegisterScriptable('TStringList', lua_pushStrings);

  // Register the input a s a scriptable object
  RegisterScriptable('TPHXInput', lua_pushInput);

  Lua_pushScriptable(L, Input);
end;
      *)



{$REGION 'Lua utils'}

// Returns the error message on the top of the lua stack
//------------------------------------------------------------------------------
function lua_getErrorText(L: Plua_State; Status: Integer): String;
begin
  if Status > 0 then
  begin
    // Get the error message from the stack...
    Result:= String( lua_tostring(L, -1) );
    // ... pop the message
    lua_pop(L, 1);
  end else
  begin
    Result:= '';
  end;
end;

// Wrapper for calling object methods
//------------------------------------------------------------------------------
function lua_functioncall(L: Plua_State): Integer; cdecl;
var Method: TMethod;
begin
  Method.Code:= lua_topointer(L, lua_upvalueindex(1) );
  Method.Data:= lua_topointer(L, lua_upvalueindex(2) );

  Result:= TLuaFunction(Method)(L);
end;

// Wrapper for calling TPHXScriptFunction events from a lua script
//------------------------------------------------------------------------------
function lua_ScriptFunctionCall(L: Plua_State): Integer; cdecl;
var Method: TMethod;
begin
  // Get the method callback
  Method.Code:= lua_topointer(L, lua_upvalueindex(1) );
  Method.Data:= lua_topointer(L, lua_upvalueindex(2) );

  ScriptParams .SetState(L);
  ScriptResults.SetState(L);

  TPHXScriptFunction(Method)(ScriptParams, ScriptResults);

  Result:= ScriptResults.Count;
end;

// Push a delphi string on the a lua stack
//------------------------------------------------------------------------------
procedure lua_pushstring(L: PLuaState; const Value: AnsiString);
begin
  lua.lua_pushstring(L, PAnsiChar(Value));
end;

// Push a delphi string on the a lua stack
//------------------------------------------------------------------------------
procedure lua_pushstring(L: PLuaState; const Value: WideString);
begin
  lua.lua_pushstring(L, PAnsiChar(AnsiString(Value)));
end;

//------------------------------------------------------------------------------
procedure lua_pushfunction(L: PLuaState; const Funct: TLuaFunction);
begin
  lua_pushlightuserdata(L, TMethod(Funct).Code);
  lua_pushlightuserdata(L, TMethod(Funct).Data);

  lua_pushcclosure(L, @lua_functioncall, 2);
end;




{$ENDREGION}

{$REGION 'luaBase'}

// Print all the parameters
//------------------------------------------------------------------------------
function luaBase_Print(LuaState: Plua_State): Integer; cdecl;
var Engine : TPHXScript;
var LuaType: Integer;
var i, n   : Integer;
begin
  Engine:= TPHXScript(lua_topointer(LuaState, lua_upvalueindex(1) ));

  n:= lua_gettop(LuaState);

  for I:= 1 to n do
  begin
    LuaType:= lua_type(LuaState, I);

    case LuaType of
      LUA_TNONE:
      begin
        Engine.Print('none');
      end;
      LUA_TNIL:
      begin
        Engine.Print('nil');
      end;
      LUA_TBOOLEAN:
      begin
        if lua_toboolean(LuaState, n) then
        begin
          Engine.Print('true');
        end else
        begin
          Engine.Print('false');
        end;
      end;
      // The type is a table
      LUA_TTABLE:
      begin
     //   Engine.DoPrint(LuaState, '{' +#13 + LuaTableToString(LuaState, I, '  ' ) + '}' );
      end;
      LUA_TSTRING:
      begin
        Engine.Print(String(lua_tostring(LuaState, I)));
      end;
      LUA_TNUMBER:
      begin
        Engine.Print(FloatToStr(lua_tonumber(LuaState, I)) );
      end;
      LUA_TLIGHTUSERDATA:
      begin
        //Engine.DoPrint('[LIGHTUSERDATA]');
      end;
      LUA_TFUNCTION:
      begin
        //Engine.DoPrint('[FUNCTION]');
      end;
      LUA_TUSERDATA:
      begin
        //Engine.DoPrint('[USERDATA]');
      end;
      LUA_TTHREAD:
      begin
        //Engine.DoPrint('[THREAD]');
      end;
    end;
  end;
  Result:=0;
end;

//------------------------------------------------------------------------------
function luaBase_Format(LuaState: Plua_State): Integer; cdecl;
var Format: AnsiString;
var Output: AnsiString;
var Text  : AnsiString;
var Curr  : PAnsiChar;
var Start : PAnsiChar;
var Kind  : AnsiChar;
var Index : Integer;
begin
  Format:= lua_tostring(LuaState, 1);
  Output:= '';

  // Scan the format string
  Index:= 2;
  Curr := @Format[1];
  while Curr^ <> #0 do
  begin
    // Start of a format character
    if Curr^ = '%' then
    begin
      Start:= Curr;

      // Skip the format character
      Inc(Curr);

      // Escaped %
      if Curr^ = '%' then
      begin
        Output:= Output + Curr^;
        Inc(Curr);
        Continue;
      end;

      Kind:= #0;
      // Extract the format string
      while (Curr^ <> #0) and (Curr^ in ['0'..'9', '.', '-']) do
      begin
        Inc(Curr);

        // Store the current character
        if (Curr^ in ['d', 'e', 'f', 'g', 'm', 'n', 'p', 's', 'u', 'x']) then
        begin
          Kind:= Curr^;

          // Skip format character
          Inc(Curr);

          Break;
        end;

       end;
      // Extract the format string
      SetString(Text, Start, Curr - Start);

      case Kind of
        // Decimal (integer)
        'd': Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tointeger(LuaState, Index)]));
        // Scientific
        'e':  Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tonumber(LuaState, Index)]));
        // Fixed
        'f': Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tonumber(LuaState, Index)]));
        // General
        'g': Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tonumber(LuaState, Index)]));
        // Money
        'm': Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tonumber(LuaState, Index)]));
        //  Number (floating)
        'n':  Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tonumber(LuaState, Index)]));
        // Pointer
        'p':  Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tointeger(LuaState, Index)]));
        // String
        's':  Output:= Output + AnsiString(SysUtils.Format(String(Text), [String(lua_tostring(LuaState, Index))]));
        // Unsigned decimal
        'u': Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tointeger(LuaState, Index)]));
        // Hexadecimal
        'x': Output:= Output + AnsiString(SysUtils.Format(String(Text), [lua_tointeger(LuaState, Index)]));
      end;
      Inc(Index);
    end;

    Output:= Output + Curr^;

    Inc(Curr);
  end;

  Lua.lua_pushstring(LuaState, PAnsiChar(AnsiString(Output)));

  Result:= 1;
end;

{$ENDREGION}

{$REGION 'luaStrings'}

function lua_StringsRead   (L: PLua_State): Integer; cdecl; forward;
function lua_StringsWrite  (L: PLua_State): Integer; cdecl; forward;

function lua_StringsClear  (L: PLua_State): Integer; cdecl; forward;
function lua_StringsAdd    (L: PLua_State): Integer; cdecl; forward;
function lua_StringsInsert (L: PLua_State): Integer; cdecl; forward;
function lua_StringsDelete (L: PLua_State): Integer; cdecl; forward;
function lua_StringsIndexOf(L: PLua_State): Integer; cdecl; forward;

//------------------------------------------------------------------------------
const luaStrings_meta : array[0..2] of luaL_Reg = (
  (Name : '__index'   ; func: lua_StringsRead ),
  (Name : '__newindex'; func: lua_StringsWrite ),
  (Name : nil         ; func: nil)
);
//------------------------------------------------------------------------------
const luaStrings_method : array[0..5] of luaL_Reg = (
  (Name : 'Clear'    ; func: lua_StringsClear ),
  (Name : 'Add'      ; func: lua_StringsAdd),
  (Name : 'Insert'   ; func: lua_StringsInsert),
  (Name : 'Delete'   ; func: lua_StringsDelete),
  (Name : 'IndexOf'  ; func: lua_StringsIndexOf),
  (Name : nil        ; func: nil)
);

//-----------------------------------------------------------------------------
procedure lua_pushStrings(L: PLua_State; Strings: TStrings);
begin
  lua_newtable(L); // object table
    luaL_register(L, nil, @luaStrings_method);

  lua_newtable(L); // Meta table
    // push the owner
    lua_pushstring(L, 'this');
    lua_pushlightuserdata(L, Strings);
    lua_rawset(L, -3);

    luaL_register(L, nil, @luaStrings_meta);

  // Set the metatable for the object table
  lua_setmetatable(L, -2);
end;

// TODO: lua_checkudata
//------------------------------------------------------------------------------
function Lua_StringsFromTable(L: PLua_State; Index: Integer): TStrings; cdecl;
begin
  if lua_istable(L, index) then
  begin
    lua_getmetatable(L, Index);
    // we got the meta table
    if lua_istable(L, -1) then
    begin
      lua_pushstring(L, 'this');
      lua_rawget(L, -2);

      Result:= TStrings(lua_touserdata(L, -1));
      // pop the control value
      lua_pop(L, 1);
    end else
    begin
      Result:= nil;
    end;
    // Pop the meta table
    lua_pop(L,1);
  end else
  begin
    Result:= nil;
  end;
end;

//-----------------------------------------------------------------------------
function lua_StringsRead(L: PLua_State): Integer; cdecl;
var Strings: TStrings;
var Index   : Integer;
var Prop    : String;
begin
  Strings:= Lua_StringsFromTable(L, 1);

  if not Assigned(Strings) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStrings.');
    lua_error(L);
  end;

  // 1 = object
  // 2 = name
  // 3 = value

  // Indexing a element
  if lua_isnumber(L, 2) then
  begin

    Index:= lua_tointeger(L, 2);

    if (Index >= 0) and (Index < Strings.Count) then
    begin
      lua_pushstring(L, Strings[Index]);
    end else
    begin
      lua_pushnil(L);
    end;
    Result:= 1;

  end else
  // Reading a property
  begin
    Prop:= String(lua_tostring(L, 2));

    // Strings.Count
    if SameText(Prop, 'Count') then
    begin
      lua_pushinteger(L, Strings.Count);

      Result:= 1;
    end else
    begin
      Result:=0;
    end;
  end
end;

//-----------------------------------------------------------------------------
function lua_StringsWrite(L: PLua_State): Integer; cdecl;
begin
  // Readonly table
  Result:=0;
end;

//-----------------------------------------------------------------------------
function lua_StringsClear(L: PLua_State): Integer; cdecl;
var Strings: TStrings;
begin
  // Get the control from the first upvalue of the c closure
  Strings:= Lua_StringsFromTable(L, 1);

  if not Assigned(Strings) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStrings.');
    lua_error(L);
  end;

  Strings.Clear;

  Result:=0;
end;

//-----------------------------------------------------------------------------
function lua_StringsAdd(L: PLua_State): Integer; cdecl;
var Strings: TStrings;
var i, n   : Integer;
var Value  : String;
begin
  // Get the control from the first upvalue of the c closure
  Strings:= Lua_StringsFromTable(L, 1);

  if not Assigned(Strings) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStrings.');
    lua_error(L);
  end;

  // 1 =table
  // 2..n = values

  // get the number of items to add
  n:= lua_gettop(L);
  // Add them all to the strings
  for I:=2 to n do
  begin
    Value:= String(lua_tostring(L, i));

    Strings.Add( Value );
  end;

  Result:=0;
end;

//-----------------------------------------------------------------------------
function lua_StringsInsert(L: PLua_State): Integer; cdecl;
var Strings: TStrings;
var Index  : Integer;
var Value  : String;
begin
  // Get the control from the first upvalue of the c closure
  Strings:= Lua_StringsFromTable(L, 1);

  if not Assigned(Strings) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStrings.');
    lua_error(L);
  end;

  // get the number of items to add
  Index:=         luaL_checkinteger(L, 1);
  Value:=  String(luaL_checkstring (L, 2));

  Strings.Insert( Index, Value);
  Result:=0;
end;

//-----------------------------------------------------------------------------
function lua_StringsDelete(L: PLua_State): Integer; cdecl;
var Strings: TStrings;
var Index  : Integer;
var i, n   : Integer;
begin
  // Get the control from the first upvalue of the c closure
  Strings:= Lua_StringsFromTable(L, 1);

  if not Assigned(Strings) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStrings.');
    lua_error(L);
  end;

  // get the number of items to add
  n:= lua_gettop(L);

  // Add them all to the strings
  for I:=1 to n do
  begin
    Index:= luaL_checkinteger(L, I);

    if Index >= 0 then Strings.Delete( Index );
  end;
  Result:=0;
end;

//-----------------------------------------------------------------------------
function lua_StringsIndexOf(L: PLua_State): Integer; cdecl;
var Strings: TStrings;
var i, n   : Integer;
var Value  : String;
var Index  : Integer;
begin
  // Get the control from the first upvalue of the c closure
  Strings:= Lua_StringsFromTable(L, 1);

  if not Assigned(Strings) then
  begin
    lua_pushstring(L, 'Malformed meta table for TLuaStrings.');
    lua_error(L);
  end;

  n:= lua_gettop(L);

  // Find the value for each of the string parameters
  for I:=1 to n do
  begin
    Value:=  String(lua_tostring(L, I));

    Index:= Strings.IndexOf( Value );

    lua_pushnumber(L, Index );
  end;
  Result:= n;
end;

{$ENDREGION}



{$REGION 'TPHXScriptParam'}

// TPHXScriptParams
//==============================================================================
constructor TPHXScriptParam.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXScriptParam.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXScriptParam.GetBoolean: Boolean;
begin
  Result:= lua_toboolean(State, Index);
end;

//------------------------------------------------------------------------------
function TPHXScriptParam.GetInteger: Integer;
begin
  Result:= lua_tointeger(State, Index);
end;

//------------------------------------------------------------------------------
function TPHXScriptParam.GetNumber: Double;
begin
  Result:= lua_tonumber(State, Index);
end;

//------------------------------------------------------------------------------
function TPHXScriptParam.GetString: String;
begin
  Result:= String(lua_tostring(State, Index));
end;
{$ENDREGION}

{$REGION 'TPHXScriptParams'}

// TPHXScriptParams
//==============================================================================
constructor TPHXScriptParams.Create(const AState: PLuaState);
begin
  FState:= AState;
  FParam:= TPHXScriptParam.Create;

  if Assigned(FState) then
  begin
    FCount:= lua_gettop(FState);
  end;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptParams.Destroy;
begin
  FParam.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TPHXScriptParams.GetParam(const Index: Integer): TPHXScriptParam;
begin
  FParam.Index:= Index+1;
  FParam.State:= State;

  Result:= FParam;
end;


//------------------------------------------------------------------------------
procedure TPHXScriptParams.SetState(const Value: PLuaState);
begin
  FState:= Value;
  FCount:= lua_gettop(FState);
end;


{$ENDREGION}

{$REGION 'TPHXScriptResults'}

// TPHXScriptResults
//==============================================================================
constructor TPHXScriptResults.Create(const AState: PLuaState);
begin
  FState:= AState;
  FCount:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Value: Boolean);
begin
  lua_pushboolean(FState, Value);

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Value: Integer);
begin
  lua_pushinteger(FState, Value);

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Value: Double);
begin
  lua_pushnumber(FState, Value);

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Value: String);
begin
  lua.lua_pushstring(FState, PAnsiChar(AnsiString(Value)));

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Values: array of Boolean);
var Index: Integer;
begin
  lua_newtable(FState);

  for Index := Low(Values) to High(Values) do
  begin
    lua_pushboolean(FState, Values[Index]);

    lua_rawseti(FState, -2, Index + 1);
  end;

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Values: array of Integer);
var Index: Integer;
begin
  lua_newtable(FState);

  for Index := Low(Values) to High(Values) do
  begin
    lua_pushinteger(FState, Values[Index]);

    lua_rawseti(FState, -2, Index + 1);
  end;

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Values: array of Double);
var Index: Integer;
begin
  lua_newtable(FState);

  for Index := Low(Values) to High(Values) do
  begin
    lua_pushnumber(FState, Values[Index]);

    lua_rawseti(FState, -2, Index + 1);
  end;

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.Add(const Values: array of String);
var Index: Integer;
begin
  lua_newtable(FState);

  for Index := Low(Values) to High(Values) do
  begin
    lua_pushstring(FState, PAnsiChar(AnsiString(Values[Index])));

    lua_rawseti(FState, -2, Index + 1);
  end;

  Inc(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptResults.SetState(const Value: PLuaState);
begin
  FState:= Value;
  FCount:= 0;
end;

{$ENDREGION}

{$REGION 'TPHXScript'}

// TPHXScript
//==============================================================================
constructor TPHXScript.Create;
begin
  FName:= 'Script';

  {$IFNDEF LUA_STATIC_LINK}
  if not LuaLibLoaded then
  begin
     if not LoadLuaLib then
     begin
       raise Exception.Create('Failed to load lua5.1.dll');
     end;
  end;
  {$ENDIF}

  // Create the lua state
  FState:= lua_open();

  // Register the print function
  lua_pushlightuserdata(FState, Self);
  lua_pushcclosure(FState, @luaBase_Print, 1);
  lua_setglobal(FState, 'print');

  // Register the format function
  lua_pushcfunction(FState, @luaBase_Format);
  lua_setglobal(FState, 'format');
end;

//------------------------------------------------------------------------------
destructor TPHXScript.Destroy;
begin
  lua_close(FState);

  FState:= nil;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Print(const Text: String);
begin
  if Assigned(OnPrint) then OnPrint(Self, Text);
end;

//----------------------------------------------------------------------------
procedure TPHXScript.CheckStatus(Status: Integer);
var Error : String;
begin
  if Status <> 0 then
  begin
    // Get the error message from the stack...
    Error:= String(lua_tostring(FState, -1) );
    // ... pop the message
    lua_pop(FState, 1);

    if Assigned(OnError) then
    begin
      OnError(Self, Error);
    end else
    begin
      raise EPHXScriptError.CreateFmt('Script error:'#13, [Error]);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Timer: TPHXTimer; const Name: AnsiString = LIBRARY_TIMER);
begin
  luaTimer(FState, Timer);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Input: TPHXInput; const Name: AnsiString = LIBRARY_INPUT);
begin
  luaInput(FState, Input);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Audio: TPHXAudioEngine; const Name: AnsiString = LIBRARY_AUDIO);
begin
  luaAudio(FState, Audio);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Instance: TPersistent; const Name: AnsiString);
begin
  lua_PushPersistent(FState, Instance);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Event: TPHXScriptFunction; const Name: AnsiString);
begin
  lua_pushlightuserdata(FState, TMethod(Event).Code);
  lua_pushlightuserdata(FState, TMethod(Event).Data);

  lua_pushcclosure(FState, @lua_ScriptFunctionCall, 2);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Event: TLuaFunction; const Name: AnsiString);
begin
  lua_pushlightuserdata(FState, TMethod(Event).Code);
  lua_pushlightuserdata(FState, TMethod(Event).Data);

  lua_pushcclosure(FState, @lua_functioncall, 2);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//------------------------------------------------------------------------------
procedure TPHXScript.Register(const Event: lua_CFunction; const Name: AnsiString);
begin
  lua_pushcfunction(FState, Event);

  lua_setglobal(FState, PAnsiChar(Name));
end;

//----------------------------------------------------------------------------
procedure TPHXScript.LoadFromFile(const FileName: String);
var Lines: TStrings;
begin
  Lines:= TStringList.Create;
  try
    Lines.LoadFromFile(FileName);

    FSource:= AnsiString(Lines.Text);
  finally
    Lines.Free;
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.LoadFromStream(const Stream: TStream);
var Lines: TStrings;
begin
  Lines:= TStringList.Create;
  try
    Lines.LoadFromStream(Stream);

    FSource:= AnsiString(Lines.Text);
  finally
    Lines.Free;
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.LoadFromString(const Source: String);
begin
  FSource:= AnsiString(Source);
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Compile;
var Status: Integer;
begin
  // Empty string
  if Length(Source) = 0 then Exit;

  Status:= luaL_loadbuffer(FState, PAnsiChar(Source), Length(Source), PAnsiChar(AnsiString(Name)));

  // If the chunk is loaded ok run the script to register the globals
  if Status = 0 then
  begin
    // lua_pcall pops the chunk from the stack and executes it
    Status:= lua_pcall(FState, 0, 0, 0);
  end;

  CheckStatus(Status);
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Compile(const Source: AnsiString);
begin
  FSource:= Source;

  Compile;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Run(const Method: AnsiString);
var Status: Integer;
begin
  lua_getglobal(FState, PAnsiChar(Method));

  if lua_isfunction(FState, -1) then
  begin
    Status:= lua_pcall(FState, 0, 0, 0);

    CheckStatus(Status);
  end else
  begin
    lua_pop(FState, 1);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Run(const Method: AnsiString; const Param: Integer);
var Status: Integer;
begin
  lua_getglobal(FState, PAnsiChar(Method));

  if lua_isfunction(FState, -1) then
  begin
    lua_pushinteger(FState, Param);

    Status:= lua_pcall(FState, 1, 0, 0);

    CheckStatus(Status);
  end else
  begin
    lua_pop(FState, 1);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Run(const Method: AnsiString; const Param: Double);
var Status: Integer;
begin
  lua_getglobal(FState, PAnsiChar(Method));

  if lua_isfunction(FState, -1) then
  begin
    lua_pushnumber(FState, Param);

    Status:= lua_pcall(FState, 1, 0, 0);

    CheckStatus(Status);
  end else
  begin
    lua_pop(FState, 1);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Run(const Method: AnsiString; const Param: Boolean);
var Status: Integer;
begin
  lua_getglobal(FState, PAnsiChar(Method));

  if lua_isfunction(FState, -1) then
  begin
    lua_pushboolean(FState, Param);

    Status:= lua_pcall(FState, 1, 0, 0);

    CheckStatus(Status);
  end else
  begin
    lua_pop(FState, 1);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXScript.Run(const Method: AnsiString; const Param: AnsiString);
var Status: Integer;
begin
  lua_getglobal(FState, PAnsiChar(Method));

  if lua_isfunction(FState, -1) then
  begin
    lua.lua_pushstring(FState, PAnsiChar(Param));

    Status:= lua_pcall(FState, 1, 0, 0);

    CheckStatus(Status);
  end else
  begin
    lua_pop(FState, 1);
  end;
end;

//----------------------------------------------------------------------------
function TPHXScript.GetVariable(const Name: AnsiString): Variant;
begin
  lua_getglobal(FState, PAnsiChar(Name));

  case lua_type(FState, -1) of
    LUA_TSTRING : Result:= String(lua_tostring(FState, -1));
    LUA_TNUMBER : Result:= lua_tonumber(FState, -1);
    LUA_TBOOLEAN: Result:= lua_toboolean(FState, -1);
    else          Result:= Null;

  end;
end;



(*
//----------------------------------------------------------------------------
function TPHXScript.RunString(const Script: String): Boolean;
var Status: Integer;
var Error : String;
begin
  Result:= False;

  // Empty string
  if Trim(Script) = '' then Exit;

  Status:= luaL_loadbuffer(FState, PAnsiChar( AnsiString(Script) ), Length(Script), PAnsiChar(AnsiString(Name)));

  // If the chunk is loaded ok run the script to register the globals
  if Status = 0 then
  begin
    // lua_pcall pops the chunk from the stack and executes it
    Status:= lua_pcall(FState, 0, 0, 0);
  end;

  if Status <> 0 then
  begin
    Error:= lua_getErrorText(FState, Status);

    DoError(Error);

    Result:= False;
  end else
  begin
    Result:= True;
  end;
end;

//------------------------------------------------------------------------------
function TPHXScript.RunFile(const FileName: String): Boolean;
var Status: Integer;
var Error : String;
begin
  Status:= luaL_loadfile(FState, PAnsiChar( AnsiString(FileName) ) );

  // If the chunk is loaded ok run the script to register the globals
  if Status = 0 then
  begin
    // lua_pcall pops the chunk from the stack and executes it
    Status:= lua_pcall(FState, 0, 0, 0);
  end;

  // Show error messages
  if Status <> 0 then
  begin
    Error:= lua_getErrorText(FState, Status);

    DoError(Error);

    Result:= False;
  end else
  begin
    Result:= True;
  end;
end;
*)

//------------------------------------------------------------------------------
procedure TPHXScript.ListFunctions(List: TStrings);
begin
  List.Clear;

  // Push the start iterator key
  lua_pushnil(FState);

  // Iterate over the globals in the script
  while lua_next(FState, LUA_GLOBALSINDEX) <> 0 do
  begin
    // Test if the current value is a function
    if lua_isfunction(FState, -1) then
    begin
      // ... add the name of the function to the list
      List.Add( String( lua_tostring(FState, -2) ) );
    end;
    // removes the value from the stack
    lua_pop(FState, 1);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXScript.ListGlobals(List: TStrings);
begin
  List.Clear;

  // Push the start iterator key
  lua_pushnil(FState);

  // Iterate over the globals in the script
  while lua_next(FState, LUA_GLOBALSINDEX) <> 0 do
  begin
    // Test if the current value is a function
    if not lua_isfunction(FState, -1) then
    begin
      // ... add the name of the variable to the list
      List.Add( String( lua_tostring(FState, -2) ) );
    end;
    // removes the value from the stack
    lua_pop(FState, 1);
  end;
end;



{$ENDREGION}

{$REGION 'TPHXScriptFiler'}

// TPHXScriptFiler
//==============================================================================
constructor TPHXScriptFiler.Create(AState: PLua_State; AIndex: Integer);
begin
  FState:= AState;
  FIndex:= AIndex;
  FTop  := lua_gettop(FState);
end;

//------------------------------------------------------------------------------
function TPHXScriptFiler.GetStackDelta: Integer;
begin
  Result:= lua_gettop(FState) - FTop;
end;
{$ENDREGION}

{$REGION 'TPHXScriptReader'}

// TPHXScriptReader
//==============================================================================
function TPHXScriptReader.ReadNumber: Double;
begin
  Result:= lua_tonumber(FState, FIndex);
end;

//------------------------------------------------------------------------------
function TPHXScriptReader.ReadInteger: Integer;
begin
  Result:= lua_tointeger(FState, FIndex);
end;

//------------------------------------------------------------------------------
function TPHXScriptReader.ReadString: String;
begin
  Result:=  String( lua_tostring(FState, FIndex) );
end;

//------------------------------------------------------------------------------
function TPHXScriptReader.ReadBoolean: Boolean;
begin
  Result:= lua_toboolean(FState, FIndex);
end;

//------------------------------------------------------------------------------
function TPHXScriptReader.ReadPointer: Pointer;
begin
  Result:= lua_topointer(FState, FIndex);
end;

//------------------------------------------------------------------------------
function TPHXScriptReader.ReadColor: TColor32;
begin
  Result:= lua_tointeger(FState, FIndex);
end;

{$ENDREGION}

{$REGION 'TPHXScriptWriter'}

//==============================================================================
procedure TPHXScriptWriter.WriteNull;
begin
	lua_pushnil(FState);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteNumber(const Value: Double);
begin
  lua_pushnumber(FState, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteInteger(const Value: Integer);
begin
  lua_pushinteger(FState, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteString(const Value: String);
begin
  lua.lua_pushstring(FState, PAnsiChar(AnsiString(Value)));
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteBoolean(const Value: Boolean);
begin
  lua_pushboolean(FState, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WritePointer(const Value: Pointer);
begin
  lua_pushlightuserdata(FState, Value);
end;


//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteMethod(const Value: TLuaFunction);
begin
  lua_pushlightuserdata(FState, TMethod(Value).Code);
  lua_pushlightuserdata(FState, TMethod(Value).Data);

  lua_pushcclosure(FState, @lua_functioncall, 2);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteMethod(const Value: lua_CFunction);
begin
  lua_pushcfunction(FState, lua_CFunction(Value));
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteColor(const Value: TColor32);
begin
  lua_pushinteger(FState, Value);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptWriter.WriteStrings(const Value: TStrings);
begin
  lua_pushStrings(FState, Value);
end;

{$ENDREGION}



//------------------------------------------------------------------------------
function FastHash(const s: String): Cardinal;
var I: Integer;
begin
  Result := 0;
  for I := 1 to Length(s) do
  begin
    Result := ((Result shl 7) or (Result shr 25)) + Ord(s[I]);
  end;
end;


{$REGION 'TPHXScriptConstants'}

// TPHXScriptConstants
//==============================================================================
constructor TPHXScriptConstants.Create;
begin
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptConstants.Destroy;
begin
  FCount := 0;
  SetCapacity(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXScriptConstants.Clear;
begin
  FCount := 0;

  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptConstants.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;
  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptConstants.Add(const Name: String; const Kind: TPHXScriptValueKind; Value: Pointer);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  Pointer(FList[FCount-1].Name):= nil;

  FList[FCount-1].Name := Name;
  FList[FCount-1].Hash := FastHash(Name);
  FList[FCount-1].Kind := Kind;
  FList[FCount-1].Value:= Value;
end;

//------------------------------------------------------------------------------
function TPHXScriptConstants.Find(const Name: String): PPHXScriptConstant;
var Hash: Cardinal;
var Index: Integer;
begin
  Hash:= FastHash(Name);

  for Index := 0 to FCount-1 do
  begin
    if (FList^[Index].Hash = Hash) and (FList^[Index].Name = Name) then
    begin
      Result:= @FList^[Index];
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXScriptConstants.GetItem(const Index: Integer): TPHXScriptConstant;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXScriptConstants.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXScriptConstant));
  end;
end;

{$ENDREGION}

{$REGION 'TPHXScriptProperties'}

// TPHXScriptProperties
//==============================================================================
constructor TPHXScriptProperties.Create;
begin
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptProperties.Destroy;
begin
  FCount := 0;
  SetCapacity(0);

  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXScriptProperties.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptProperties.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;
  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptProperties.Add(const Name: String; const Kind: TPHXScriptValueKind; Reader: Pointer; Writer: Pointer);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  Pointer(FList[FCount-1].Name):= nil;

  FList[FCount-1].Name  := Name;
  FList[FCount-1].Hash  := FastHash(Name);
  FList[FCount-1].Kind  := Kind;
  FList[FCount-1].Reader:= Reader;
  FList[FCount-1].Writer:= Writer;
end;

//------------------------------------------------------------------------------
function TPHXScriptProperties.Find(const Name: String): PPHXScriptProperty;
var Hash: Cardinal;
var Index: Integer;
begin
  Hash:= FastHash(Name);

  for Index := 0 to FCount-1 do
  begin
    if (FList^[Index].Hash = Hash) and (FList^[Index].Name = Name) then
    begin
      Result:= @FList^[Index];
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXScriptProperties.Find(const Name: String; out Prop: PPHXScriptProperty): Boolean;
var Hash: Cardinal;
var Index: Integer;
begin
  Hash:= FastHash(Name);

  for Index := 0 to FCount-1 do
  begin
    if (FList^[Index].Hash = Hash) and (FList^[Index].Name = Name) then
    begin
      Prop  := @FList^[Index];
      Result:= True;

      Exit;
    end;
  end;
  Prop  := nil;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXScriptProperties.GetItem(const Index: Integer): TPHXScriptProperty;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXScriptProperties.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXScriptProperty));
  end;
end;

{$ENDREGION}

{$REGION 'TPHXScriptMethod'}

// TPHXScriptMethods
//==============================================================================
constructor TPHXScriptMethods.Create;
begin
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptMethods.Destroy;
begin
  FCount := 0;
  SetCapacity(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXScriptMethods.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptMethods.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptMethods.Add(const Name: String; const Method: Pointer);
begin
  Inc(FCount);

  if Count > Capacity then Grow;

  Pointer(FList[FCount-1].Name):= nil;

  FList[FCount-1].Name  := Name;
  FList[FCount-1].Hash  := FastHash(Name);
  FList[FCount-1].Method:= Method;
end;

//------------------------------------------------------------------------------
function TPHXScriptMethods.Find(const Name: String): PPHXScriptMethod;
var Hash: Cardinal;
var Index: Integer;
begin
  Hash:= FastHash(Name);

  for Index := 0 to FCount-1 do
  begin
    if (FList^[Index].Hash = Hash) and (FList^[Index].Name = Name) then
    begin
      Result:= @FList^[Index];
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXScriptMethods.Find(const Name: String; out Method: PPHXScriptMethod): Boolean;
var Hash: Cardinal;
var Index: Integer;
begin
  Hash:= FastHash(Name);

  for Index := 0 to FCount-1 do
  begin
    if (FList^[Index].Hash = Hash) and (FList^[Index].Name = Name) then
    begin
      Method:= @FList^[Index];
      Result:= True;
      Exit;
    end;
  end;
  Method:= nil;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TPHXScriptMethods.GetItem(const Index: Integer): TPHXScriptMethod;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXScriptMethods.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXScriptMethod));
  end;
end;

{$ENDREGION}

{$REGION 'TPHXScriptClass'}

// TPHXScriptClass
//==============================================================================
constructor TPHXScriptClass.Create;
begin
  FMethods   := TPHXScriptMethods.Create;
  FProperties:= TPHXScriptProperties.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptClass.Destroy;
begin
  FMethods.Free;
  FProperties.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXScriptClass.AddProperty(const Name: String; const Kind: TPHXScriptValueKind; Reader, Writer: Pointer);
begin
  FProperties.Add(Name, Kind, Reader, Writer);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptClass.AddMethod(const Name: String; Method: Pointer);
begin
  FMethods.Add(Name, Method);
end;

{$ENDREGION}

{$REGION 'TPHXScriptClasses'}

// TPHXScriptClasses
//==============================================================================
constructor TPHXScriptClasses.Create;
begin
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptClasses.Destroy;
begin
  FList.Free;

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXScriptClasses.Add(const Name: String): TPHXScriptClass;
begin
  Result:= TPHXScriptClass.Create;
  Result.Name:= Name;
  Result.Hash:= FastHash(Name);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXScriptClasses.Find(const Name: String): TPHXScriptClass;
var Hash : Cardinal;
var Index: Integer;
var Item : TPHXScriptClass;
begin
  Hash:= FastHash(Name);

  for Index := 0 to FList.Count-1 do
  begin
    Item:= TPHXScriptClass(FList.List[Index]);

    if (Item.Hash = Hash) and (Item.Name = Name) then
    begin
      Result:= Item;
      Exit;
    end;
  end;
  Result:= nil;
end;

{$ENDREGION}

{$REGION 'TPHXScriptRegistry'}

// TPHXScriptRegistry
//==============================================================================
constructor TPHXScriptRegistry.Create;
begin
  FConstants:= TPHXScriptConstants.Create;
  FMethods  := TPHXScriptMethods.Create;
  FClasses  := TPHXScriptClasses.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXScriptRegistry.Destroy;
begin
  FConstants.Free;
  FMethods.Free;
  FClasses.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TPHXScriptRegistry.AddClass(const Name: String): TPHXScriptClass;
begin
  Result:= FClasses.Add(Name);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptRegistry.AddMethod(const Name: String; Method: Pointer);
begin
  FMethods.Add(Name, Method);
end;

//------------------------------------------------------------------------------
procedure TPHXScriptRegistry.AddConstant(const Name: String; const Kind: TPHXScriptValueKind; Value: Pointer);
begin
  FConstants.Add(Name, Kind, Value);
end;

{$ENDREGION}


{$REGION 'TPHXScriptEngine'}

// TPHXScriptEngine
//==============================================================================
constructor TPHXScriptEngine.Create;
begin
  FName:= 'Script';
  FSource:= '';
end;

//------------------------------------------------------------------------------
destructor TPHXScriptEngine.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXScriptEngine.DoError(const Error: String);
begin
  if Assigned(OnError) then
  begin
    OnError(nil, Error);
  //  OnError(Self, Error);
  end else
  begin
     raise EPHXScriptError.CreateFmt('Script error: %s'#13, [Error]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXScriptEngine.DoPrint(const Text: String);
begin
  if Assigned(OnPrint) then
  begin
    OnPrint(nil, Text);
  end;
end;

{$ENDREGION}



initialization
  ScriptRegistry:= TPHXScriptRegistry.Create;

  ScriptParams := TPHXScriptParams.Create(nil);
  ScriptResults:= TPHXScriptResults.Create(nil);
finalization
  ScriptRegistry.Free;

  ScriptParams.Free;
  ScriptResults.Free;
end.
