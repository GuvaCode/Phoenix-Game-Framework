unit phxScript_Lua;
//< Scripting support using Lua

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes, Variants,

  Lua,

  phxTypes,
  phxScript,

  phxTimer,
  phxInput,
  phxAudio;

type

//------------------------------------------------------------------------------
TLuaScriptEngine = class(TPHXScriptEngine)
  private
    FState: PLuaState;

    procedure RegisterConstants;
    procedure RegisterMethods;

    procedure CheckStatus(Status: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Compile; override;

    procedure SetVariable(const Name: String; const Value: Integer); override;
    procedure SetVariable(const Name: String; const Value: String); override;
    procedure SetVariable(const Name: String; const Value: Double); override;
    procedure SetVariable(const Name: String; const Value: Boolean); override;
    procedure SetVariable(const Name: String; const Value: TObject); override;
  end;

implementation

var ScriptParams : TPHXScriptParams;
var ScriptResults: TPHXScriptResults;

{$REGION 'lua_method'}

type

TMethodProc = procedure(Params: TPHXScriptParams; Results: TPHXScriptResults);

// procedure(Params: TPHXScriptParams; Results: TPHXScriptResults)
//------------------------------------------------------------------------------
function lua_method_call(LuaState: PLuaState): Integer; cdecl;
var Method: Pointer;
begin
  Method:= lua_topointer(LuaState, lua_upvalueindex(1) );

  ScriptParams .State:= LuaState;
  ScriptResults.State:= LuaState;

  TMethodProc(Method)(ScriptParams, ScriptResults);

  Result:= ScriptResults.Count;
end;

{$ENDREGION}

{$REGION 'lua_class'}

type

TClassMethodProc = procedure(Instance: Pointer; Params: TPHXScriptParams; Results: TPHXScriptResults);

// procedure(Instance: Pointer; Params: TPHXScriptParams; Results: TPHXScriptResults)
//------------------------------------------------------------------------------
function lua_class_method(LuaState: PLuaState): Integer; cdecl;
var Instance: Pointer;
var Method  : Pointer;
begin
  Instance:= lua_topointer(LuaState, lua_upvalueindex(1) );
  Method  := lua_topointer(LuaState, lua_upvalueindex(2) );

  ScriptParams .State:= LuaState;
  ScriptResults.State:= LuaState;

  TClassMethodProc(Method)(Instance, ScriptParams, ScriptResults);

  Result:= ScriptResults.Count;
end;

type

TPropReader_I = procedure(Instance: Pointer; out Value: Integer);
TPropReader_S = procedure(Instance: Pointer; out Value: String);
TPropReader_N = procedure(Instance: Pointer; out Value: Double);
TPropReader_B = procedure(Instance: Pointer; out Value: Boolean);

//------------------------------------------------------------------------------
function lua_class_read(LuaState: PLuaState): Integer; cdecl;
var Info    : TPHXScriptClass;
var Instance: Pointer;

var Name  : String;
var Prop  : PPHXScriptProperty;
var Method: PPHXScriptMethod;

var valInteger: Integer;
var valString : String;
var valNumber : Double;
var valBoolean: Boolean;
begin
  Info    := TPHXScriptClass(lua_topointer(LuaState, lua_upvalueindex(1)));
  Instance:= lua_topointer(LuaState, lua_upvalueindex(2));

  Name:= String(lua_tostring(LuaState, 2));


  // Read a property
  if Info.Properties.Find(Name, Prop) then
  begin
    if Prop^.Reader = nil then
    begin
      raise EPHXScriptError.CreateFmt('Property %s is not readable.', [Name]);
    end;

    case Prop^.Kind of
      vkInteger:
      begin
        TPropReader_I(Prop^.Reader)(Instance, valInteger);

        lua_pushinteger(LuaState, valInteger);
      end;
      vkString:
      begin
        valString:= String(luaL_checkstring(LuaState, 3));

        lua_pushstring(LuaState, PAnsiChar(AnsiString(valString)));
      end;
      vkNumber:
      begin
        TPropReader_N(Prop^.Reader)(Instance, valNumber);

        lua_pushnumber(LuaState, valNumber);
      end;
      vkBoolean:
      begin
        TPropReader_B(Prop^.Reader)(Instance, valBoolean);

        lua_pushboolean(LuaState, valBoolean);
      end;
    end;

    Result:= 1;
  end else
  // Call a method
  if Info.Methods.Find(Name, Method) then
  begin
    lua_pushlightuserdata(LuaState, Instance);
    lua_pushlightuserdata(LuaState, Method.Method);

    lua_pushcclosure(LuaState, @lua_class_method, 2);

    Result:= 1;
  end else
  begin
    raise EPHXScriptError.CreateFmt('Property %s is not scriptable.', [Name]);

    Result:= 0;
  end;
end;

type

TPropWriter_I = procedure(Instance: Pointer; const Value: Integer);
TPropWriter_S = procedure(Instance: Pointer; const Value: String);
TPropWriter_N = procedure(Instance: Pointer; const Value: Double);
TPropWriter_B = procedure(Instance: Pointer; const Value: Boolean);

//------------------------------------------------------------------------------
function lua_class_write(LuaState: PLuaState): Integer; cdecl;
var Info    : TPHXScriptClass;
var Instance: Pointer;

var Name  : String;
var Prop  : PPHXScriptProperty;

var valInteger: Integer;
var valString : String;
var valNumber : Double;
var valBoolean: Boolean;
begin
  Info    := TPHXScriptClass(lua_topointer(LuaState, lua_upvalueindex(1)));
  Instance:= lua_topointer(LuaState, lua_upvalueindex(2));

  Name:= String(lua_tostring(LuaState, 2));

  // Write a property
  if Info.Properties.Find(Name, Prop) then
  begin

    if Prop^.Writer = nil then
    begin
      raise EPHXScriptError.CreateFmt('Property %s is not writeable.', [Name]);
    end;

    case Prop^.Kind of
      vkInteger:
      begin
        valInteger:= luaL_checkinteger(LuaState, 3);

        TPropWriter_I(Prop^.Writer)(Instance, valInteger);
      end;
      vkString:
      begin
        valString:= String(luaL_checkstring(LuaState, 3));

        TPropWriter_S(Prop^.Writer)(Instance, valString);
      end;
      vkNumber:
      begin
        valNumber:= luaL_checknumber(LuaState, 3);

        TPropWriter_N(Prop^.Writer)(Instance, valNumber);
      end;
      vkBoolean:
      begin
        valBoolean:= lua_toboolean(LuaState, 3);

        TPropWriter_B(Prop^.Writer)(Instance, valBoolean);
      end;
    end;
    Result:= 0;
  end else
  begin
    raise EPHXScriptError.CreateFmt('Property %s is not scriptable.', [Name]);
    Result:= 0;
  end;
end;


//------------------------------------------------------------------------------
procedure lua_pushclass(LuaState: PLuaState; SClass: TPHXScriptClass; Instance: TObject); overload;
begin
  // create the instance table
  lua_newtable(LuaState);
    // create the meta table for the keyboard
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, SClass);
    lua_pushlightuserdata(LuaState, Instance);
    lua_pushcclosure(LuaState, @lua_class_read, 2);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, '__newindex');
    lua_pushlightuserdata(LuaState, SClass);
    lua_pushlightuserdata(LuaState, Instance);
    lua_pushcclosure(LuaState, @lua_class_write, 2);
    lua_rawset(LuaState, -3);

  // Set the meta table for the instance
  lua_setmetatable(LuaState, -2);
end;

//------------------------------------------------------------------------------
procedure lua_pushclass(LuaState: PLuaState; Instance: TObject); overload;
var AClass: TClass;
var SClass: TPHXScriptClass;
begin
  Assert(Assigned(Instance));

  AClass:= Instance.ClassType;
  repeat
    SClass:= ScriptRegistry.Classes.Find(AClass.ClassName);

    AClass:= AClass.ClassParent;
  until Assigned(SClass) or (AClass = nil);

  if Assigned(SClass) then
  begin
    lua_pushclass(LuaState, SClass, Instance);
  end else
  begin
    raise EPHXScriptError.CreateFmt('The class %s is not registered for scripting.', [Instance.ClassName]);
  end;
end;


{$ENDREGION}


{$REGION 'TLuaScriptEngine'}

// TLuaScriptEngine
//==============================================================================
constructor TLuaScriptEngine.Create;
begin
  inherited Create;

  {$IFNDEF LUA_STATIC_LINK}
  if not LuaLibLoaded then
  begin
    if not LoadLuaLib then
    begin
      raise Exception.Create('Failed to load lua5.1.dll');
    end;
  end;
  {$ENDIF}

  FState:= lua_open;
end;

//------------------------------------------------------------------------------
destructor TLuaScriptEngine.Destroy;
begin
  lua_close(FState);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.RegisterConstants;
var Index: Integer;
var Item : PPHXScriptConstant;

var valInteger: Integer;
var valString : String;
var valNumber : Double;
var valBoolean: Boolean;
begin
  for Index := 0 to ScriptRegistry.Constants.Count-1 do
  begin
    Item:= @ScriptRegistry.Constants.List^[Index];

    case Item^.Kind of
      vkInteger:
      begin
        valInteger:= PInteger(Item^.Value)^;

        lua_pushinteger(FState, valInteger);
      end;
      vkString :
      begin
        valString:= PString(Item^.Value)^;

        lua_pushstring(FState, PAnsiChar( AnsiString( valString )));
      end;
      vkNumber :
      begin
        valNumber:= PDouble(Item^.Value)^;

        lua_pushnumber(FState, valNumber );
      end;
      vkBoolean:
      begin
        valBoolean:= PBoolean(Item^.Value)^ ;

        lua_pushboolean(FState, valBoolean);
      end;
    end;

    lua_setglobal(FState, PAnsiChar(AnsiString(Item^.Name)));
  end;
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.RegisterMethods;
var Index: Integer;
var Item : PPHXScriptMethod;
begin
  for Index := 0 to ScriptRegistry.Methods.Count-1 do
  begin
    Item:= @ScriptRegistry.Methods.List^[Index];

    lua_pushlightuserdata(FState, Item.Method);

    lua_pushcclosure(FState, @lua_method_call, 1);

    lua_setglobal(FState, PAnsiChar(AnsiString(Item^.Name)));
  end;
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.CheckStatus(Status: Integer);
var Error : String;
begin
  if Status <> 0 then
  begin
    // Get the error message from the stack...
    Error:= String(lua_tostring(FState, -1) );
    // ... pop the message
    lua_pop(FState, 1);

    DoError(Error);
  end;
end;


//------------------------------------------------------------------------------
procedure TLuaScriptEngine.Compile;
var Status: Integer;
begin
  RegisterConstants;
  RegisterMethods;

  // Empty string
  if Length(Source) = 0 then Exit;

  Status:= luaL_loadbuffer(FState, PAnsiChar(AnsiString(Source)), Length(Source), PAnsiChar(AnsiString(Name)));

  // If the chunk is loaded ok run the script to register the globals
  if Status = 0 then
  begin
    // lua_pcall pops the chunk from the stack and executes it
    Status:= lua_pcall(FState, 0, 0, 0);
  end;

  CheckStatus(Status);
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.SetVariable(const Name: String; const Value: Integer);
begin
  lua_pushinteger(FState, Value);

  lua_setglobal(FState, PAnsiChar(AnsiString(Name)));
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.SetVariable(const Name: String; const Value: String);
begin
  lua_pushstring(FState, PAnsiChar(AnsiString(Value)));

  lua_setglobal(FState, PAnsiChar(AnsiString(Name)));
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.SetVariable(const Name: String; const Value: Boolean);
begin
  lua_pushboolean(FState, Value);

  lua_setglobal(FState, PAnsiChar(AnsiString(Name)));
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.SetVariable(const Name: String; const Value: Double);
begin
  lua_pushnumber(FState, Value);

  lua_setglobal(FState, PAnsiChar(AnsiString(Name)));
end;

//------------------------------------------------------------------------------
procedure TLuaScriptEngine.SetVariable(const Name: String; const Value: TObject);
begin
  lua_pushclass(FState, Value);

  lua_setglobal(FState, PAnsiChar(AnsiString(Name)));
end;

{$ENDREGION}



initialization
  ScriptParams := TPHXScriptParams.Create(nil);
  ScriptResults:= TPHXScriptResults.Create(nil);
finalization
  ScriptParams.Free;
  ScriptResults.Free;
end.
