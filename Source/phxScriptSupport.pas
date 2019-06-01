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
unit phxScriptSupport;
//< Scripting support for the phoenix components

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  Lua,

  phxTypes,
  phxTimer,
  phxInput,
  phxAudio;

// Push a persistent onto the lua stack using RTTI
// Note that all published methods must be of the TPHXScriptMethod type
// @param(LuaState The lua state)
// @param(Instance The instance to register)
// @param(RegisterMethods Register published methods of the instance)
procedure lua_PushPersistent(LuaState: PLuaState; Instance: TPersistent; RegisterMethods: Boolean = True);

// Push a timer onto a lua stack
// @seealso(TLuaTimer)
procedure luaTimer(LuaState: PLuaState; Timer: TPHXTimer);
// push a mouse onto a lua stack
// @seealso(TLuaMouse)
procedure luaMouse(LuaState: PLuaState; Mouse: TPHXMouse);
// push a keyboard onto a lua stack
// @seealso(TLuaKeyboard)
procedure luaKeyboard(LuaState: PLuaState; Keyboard: TPHXKeyboard);
// Push a input onto a lua stack
// @seealso(TLuaInput)
procedure luaInput(LuaState: PLuaState; Input: TPHXInput);
procedure lua_pushInput(LuaState: PLuaState; Input: TObject);




// Push a audio engine onto a lua stack
// @seealso(TLuaAudio)
procedure luaAudio(LuaState: PLuaState; Audio: TPHXAudioEngine);

implementation

uses phxScript, TypInfo;

const
  SInvalidParameters: AnsiString = 'Invalid parameters';

// maybe use luaL_newmetatable for creating meta tables?
// https://code.google.com/p/bitfighter/source/browse/zap/teleporter.cpp


{$REGION 'luaPersistent'}

type TMethodEntry = packed record
  Size   : word;
  Address: Pointer;
  Name   : Shortstring;
end;

type TMethodTable = packed record
  Count: Word;
  First: TMethodEntry;
end;

//------------------------------------------------------------------------------
function luaPersistent_instance(LuaState: PLuaState): TPersistent;
begin
  Result:= TPersistent(lua_topointer(LuaState, lua_upvalueindex(1)));
end;


// Read a property from a TPersistent using rtti
//------------------------------------------------------------------------------
function luaPersistent_read(LuaState: PLuaState): Integer; cdecl;
var Instance: TPersistent;
var PropName: String;
var PropInfo: PPropInfo;
var PropKind: TTypeKind;
var valInteger: Integer;
var valNumber : Double;
var valString : String;
begin
  Instance:= luaPersistent_instance(LuaState);
  PropName:= String(lua_tostring(LuaState, 2));

  PropInfo:= GetPropInfo(Instance, PropName);

  if Assigned(PropInfo) then
  begin
    PropKind:= PropInfo^.PropType^^.Kind;

    case PropKind of
      // Read a integer property from the instance
      tkInteger, tkInt64, tkEnumeration:
      begin
        valInteger:= GetOrdProp(Instance, PropInfo);

        if (PropKind = tkEnumeration) and (PropInfo^.PropType^^.Name = 'Boolean') then
        begin
          if valInteger = 0 then
          begin
            lua_pushboolean(LuaState, False);
          end else
          begin
            lua_pushboolean(LuaState, true);
          end;
        end else
        begin
          lua_pushinteger(LuaState, valInteger);
        end;
      end;
      // Read a number property from the instance
      tkFloat:
      begin
        valNumber:= GetFloatProp(Instance, PropInfo);

        lua_pushnumber(LuaState, valNumber);
      end;
      // Read a string property from the instance
      tkString, tkLString, tkWString, tkUString:
      begin
        valString:= GetStrProp(Instance, PropInfo);

        lua_pushstring(LuaState, PAnsiChar(AnsiString(valString)));
      end;
      // Unsupported property
      else
        raise EPHXScriptError.CreateFmt('Unsupported data type %s for property %s.', [PropInfo^.PropType^^.Name , PropName]);
    end;
    Result:= 1;

  end else
  // The property was not found
  begin
    raise EPHXScriptError.CreateFmt('The property %s is not scriptable', [PropName]);
    Result:= 0;
  end;
end;

// Write a property from a TPersistent using rtti
//------------------------------------------------------------------------------
function luaPersistent_write(LuaState: PLuaState): Integer; cdecl;
var Instance: TPersistent;
var PropName: String;
var PropInfo: PPropInfo;
var PropKind: TTypeKind;
var valInteger: Integer;
var valNumber : Double;
var valString : String;
begin
  Instance:= luaPersistent_instance(LuaState);
  PropName:= String(lua_tostring(LuaState, 2));

  PropInfo:= GetPropInfo(Instance, PropName);

  if Assigned(PropInfo) then
  begin
    PropKind:= PropInfo^.PropType^^.Kind;

    case PropKind of
      // Write a integer property to the instance
      tkInteger, tkInt64, tkEnumeration:
      begin
        valInteger:= lua_tointeger(LuaState, 3);

        SetOrdProp(Instance, PropInfo, valInteger);
      end;
     // Write a number property to the instance
     tkFloat:
      begin
        valNumber:= lua_tonumber(LuaState, 3);

        SetFloatProp(Instance, PropInfo, valNumber);
      end;
      // Write string number property to the instance
      tkString, tkLString, tkWString, tkUString:
      begin
        valString:= String(lua_tostring(LuaState, 3));

        SetStrProp(Instance, PropInfo, valString);
      end;
      // Unsupported property
      else
        raise EPHXScriptError.CreateFmt('Unsupported data type %s for property %s.', [PropInfo^.PropType^^.Name , PropName]);
    end;

  end;

  Result:= 0;
end;

// Call a method of a TPersistent
//------------------------------------------------------------------------------
function luaPersistent_method(LuaState: PLuaState): Integer; cdecl;
var Method : TMethod;
var Params : TPHXScriptParams;
var Results: TPHXScriptResults;
begin
  Method.Code:= lua_topointer(LuaState, lua_upvalueindex(1) );
  Method.Data:= lua_topointer(LuaState, lua_upvalueindex(2) );

  Params := TPHXScriptParams .Create(LuaState);
  Results:= TPHXScriptResults.Create(LuaState);
  try
    TPHXScriptFunction(Method)(Params, Results);

    Result:= Results.Count;
  finally
    Params.Free;
    Results.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure lua_PushPersistent(LuaState: PLuaState;  Instance: TPersistent; RegisterMethods: Boolean = True);
var Index      : Integer;
var MethodTable: ^TMethodTable;
var MethodEntry: ^TMethodEntry;
begin
  // create the instance table
  lua_newtable(LuaState);
    // create the meta table for the keyboard
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, Instance);
    lua_pushcclosure(LuaState, @luaPersistent_read, 1);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, '__newindex');
    lua_pushlightuserdata(LuaState, Instance);
    lua_pushcclosure(LuaState, @luaPersistent_write, 1);
    lua_rawset(LuaState, -3);

  // Set the meta table for the instance
  lua_setmetatable(LuaState, -2);

  // Register published methods
  if RegisterMethods then
  begin
    // Get the method table from the class
    MethodTable:= PPointer(Integer(Instance.ClassType) + vmtMethodTable)^;

    if MethodTable <> nil then
    begin
      MethodEntry:= @MethodTable^.First;

      for Index:= 1 to MethodTable^.Count do
      begin
        lua_pushstring(LuaState, PAnsiChar( AnsiString(MethodEntry.Name)));

        // TMethod.Code
        lua_pushlightuserdata(LuaState, MethodEntry.Address);
        // TMethod.Data
        lua_pushlightuserdata(LuaState, Instance);

        lua_pushcclosure(LuaState, @luaPersistent_method, 2);

        // Add the method to the lua table for the instance
        lua_rawset(LuaState, -3);

        // Move to the next method
        MethodEntry:= Pointer( Integer(MethodEntry) + MethodEntry^.Size);
      end;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'luaTimer'}

//------------------------------------------------------------------------------
function luaTimer_get(LuaState: PLuaState): TPHXTimer;
begin
  Result:= TPHXTimer(lua_topointer(LuaState, lua_upvalueindex(1)));
end;

//------------------------------------------------------------------------------
function luaTimer_index(LuaState: PLuaState): Integer; cdecl;
var Timer: TPHXTimer;
var Prop : String;
begin
  Timer:= luaTimer_get(LuaState);
  Prop := String(lua_tostring(LuaState, 2));

  // timer.ElapsedTime
  if SameText(Prop, 'ElapsedTime') then
  begin
    lua_pushnumber(LuaState, Timer.ElapsedTime);

    Result:= 1;
  end else
  // timer.FrameTime
  if SameText(Prop, 'FrameTime') then
  begin
    lua_pushnumber(LuaState, Timer.FrameTime);

    Result:= 1;
  end else
  // timer.FrameRate
  if SameText(Prop, 'FrameRate') then
  begin
    lua_pushnumber(LuaState, Timer.FrameTime);

    Result:= 1;
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure luaTimer(LuaState: PLuaState; Timer: TPHXTimer);
begin
  // create the keyboard table
  lua_newtable(LuaState);
    // create the meta table for the keyboard
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, Timer);
    lua_pushcclosure(LuaState, @luaTimer_index, 1);
    lua_rawset(LuaState, -3);
  // meta = table
  lua_setmetatable(LuaState, -2);
end;

{$ENDREGION}

{$REGION 'luaKeyboard'}

//------------------------------------------------------------------------------
function luaKeyboard_get(LuaState: PLuaState): TPHXKeyboard;
begin
  Result:= TPHXKeyboard(lua_topointer(LuaState, lua_upvalueindex(1)));
end;

//------------------------------------------------------------------------------
function luaKeyboard_index(LuaState: PLuaState): Integer; cdecl;
//var Keyboard: TPHXKeyboard;
//var Prop : String;
begin
 (* Keyboard:= luaKeyboard_get(LuaState);
  Prop    := String(lua_tostring(LuaState, 2));

  // mouse.x
  if SameText(Prop, 'x') then
  begin
    lua_pushnumber(LuaState, Mouse.X);

    Result:= 1;
  end else
  // mouse.y
  if SameText(Prop, 'y') then
  begin
    lua_pushnumber(LuaState, Mouse.Y);

    Result:= 1;
  end else
  begin
    Result:= 0;
  end; *)
  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaKeyboard_keys(LuaState: PLuaState): Integer; cdecl;
var Keyboard: TPHXKeyboard;
var Key: TPHXVirtualKey;
begin
  Keyboard:= luaKeyboard_get(LuaState);

  Key:= TPHXVirtualKey(luaL_checkinteger(LuaState, 2));

  lua_pushboolean(LuaState, Keyboard.Keys[Key]);

  Result:= 1;
end;

//------------------------------------------------------------------------------
procedure luaKeyboard(LuaState: PLuaState; Keyboard: TPHXKeyboard);
begin
  // create the keyboard table
  lua_newtable(LuaState);
    // create the meta table for the keyboard
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, Keyboard);
    lua_pushcclosure(LuaState, @luaKeyboard_index, 1);
    lua_rawset(LuaState, -3);
  // keyboard.meta.__index = table
  lua_setmetatable(LuaState, -2);

  // mouse.buttons
  lua_pushstring(LuaState, 'keys');
  // create the table for the states
  lua_newtable(LuaState);
      // create the meta table for the states
      lua_newtable(LuaState);

        lua_pushstring(LuaState, '__index');
        lua_pushlightuserdata(LuaState, Keyboard);
        lua_pushcclosure(LuaState, @luaKeyboard_keys, 1);
        lua_rawset(LuaState, -3);

      lua_setmetatable(LuaState, -2);
  // buttons = table
  lua_rawset(LuaState, -3);
end;

{$ENDREGION}

{$REGION 'luaMouse'}

//------------------------------------------------------------------------------
function luaMouse_get(LuaState: PLuaState): TPHXMouse;
begin
  Result:= TPHXMouse(lua_topointer(LuaState, lua_upvalueindex(1)));
end;

//------------------------------------------------------------------------------
function luaMouse_index(LuaState: PLuaState): Integer; cdecl;
var Mouse: TPHXMouse;
var Prop : String;
begin
  Mouse:= luaMouse_get(LuaState);
  Prop := String(lua_tostring(LuaState, 2));

  // mouse.x
  if SameText(Prop, 'x') then
  begin
    lua_pushnumber(LuaState, Mouse.X);

    Result:= 1;
  end else
  // mouse.y
  if SameText(Prop, 'y') then
  begin
    lua_pushnumber(LuaState, Mouse.Y);

    Result:= 1;
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
function luaMouse_buttons(LuaState: PLuaState): Integer; cdecl;
var Mouse : TPHXMouse;
var Button: TPHXMouseButton;
begin
  Mouse:= luaMouse_get(LuaState);

  Button:= TPHXMouseButton(luaL_checkinteger(LuaState, 2));

  lua_pushboolean(LuaState, Mouse.Buttons[Button]);

  Result:= 1;
end;

//------------------------------------------------------------------------------
procedure luaMouse(LuaState: PLuaState; Mouse: TPHXMouse);
begin
  // create the mouse table
  lua_newtable(LuaState);
    // create the meta table for the mouse
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, Mouse);
    lua_pushcclosure(LuaState, @luaMouse_index, 1);
    lua_rawset(LuaState, -3);
  // mouse.meta.__index = table
  lua_setmetatable(LuaState, -2);

  // mouse.buttons
  lua_pushstring(LuaState, 'buttons');
  // create the table for the states
  lua_newtable(LuaState);
      // create the meta table for the states
      lua_newtable(LuaState);

        lua_pushstring(LuaState, '__index');
        lua_pushlightuserdata(LuaState, Mouse);
        lua_pushcclosure(LuaState, @luaMouse_buttons, 1);
        lua_rawset(LuaState, -3);

      lua_setmetatable(LuaState, -2);
  // buttons = table
  lua_rawset(LuaState, -3);

end;

{$ENDREGION}

{$REGION 'luaInput'}

function luaInput_get(LuaState: PLuaState): Integer; cdecl; forward;
function luaInput_state_get(LuaState: PLuaState): Integer; cdecl; forward;
function luaInput_state_set(LuaState: PLuaState): Integer; cdecl; forward;

// input.mouse
// input.keyboard
// input.getState(state)
//------------------------------------------------------------------------------
procedure luaInput(LuaState: PLuaState; Input: TPHXInput);
var State: TPHXInputState;
begin
  // Register all input state names
  for State:= Low(TPHXInputState) to High(TPHXInputState) do
  begin
    lua_pushinteger(LuaState, Ord(State));
    lua_setglobal(LuaState, PAnsiChar(AnsiString(InputStateToString(State))));
  end;

  // create the input table
  lua_newtable(LuaState);
    // create the meta table
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, Input);
    lua_pushcclosure(LuaState, @luaInput_get, 1);
    lua_rawset(LuaState, -3);

  lua_setmetatable(LuaState, -2);

    // input.states
    lua_pushstring(LuaState, 'states');
    // create the table for the states
    lua_newtable(LuaState);
        // create the meta table for the states
        lua_newtable(LuaState);

          lua_pushstring(LuaState, '__index');
          lua_pushlightuserdata(LuaState, Input);
          lua_pushcclosure(LuaState, @luaInput_state_get, 1);
          lua_rawset(LuaState, -3);

          lua_pushstring(LuaState, '__newindex');
          lua_pushlightuserdata(LuaState, Input);
          lua_pushcclosure(LuaState, @luaInput_state_set, 1);
          lua_rawset(LuaState, -3);
        lua_setmetatable(LuaState, -2);
    // states = table
    lua_rawset(LuaState, -3);


  // Create the mouse table
  lua_pushstring(LuaState, 'mouse');
    luaMouse(LuaState, Input.Mouse);
  lua_rawset(LuaState, -3);

  // Create the keyboard table
  lua_pushstring(LuaState, 'keyboard');
    luaKeyboard(LuaState, Input.Keyboard);
  lua_rawset(LuaState, -3);
end;

procedure lua_pushInput(LuaState: PLuaState; Input: TObject);
begin
  luaInput(LuaState, TPHXInput(Input));
end;

//------------------------------------------------------------------------------
function GetInput(LuaState: PLuaState): TPHXInput; inline;
begin
  Result:= TPHXInput(lua_topointer(LuaState, lua_upvalueindex(1)));
end;

//------------------------------------------------------------------------------
function luaInput_get(LuaState: PLuaState): Integer; cdecl;
//var Input: TPHXInput;
//var Prop : String;
begin
//  Input:= GetInput(LuaState);
//  Prop := String(lua_tostring(LuaState, 2));
   {
  // Get a state from the input
  if SameText(Prop, 'states') then
  begin    (*
    lua_newtable(LuaState);
      lua_pushstring(LuaState, '__index');
      lua_pushlightuserdata(LuaState, Input);
      lua_pushcclosure(LuaState, @lua_input_state, 1);
      lua_rawset(LuaState, -3);
    lua_setmetatable(LuaState, -2);
   // lua_pushboolean(LuaState, Sprite.Visible);
         *)
    Result:= 1;
  end else
  begin
    Result:= 0;
  end;  }

  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaInput_state_get(LuaState: PLuaState): Integer; cdecl;
var Input: TPHXInput;
var State: TPHXInputState;
begin
  Input := GetInput(LuaState);

  State:= TPHXInputState(luaL_checkinteger(LuaState, 2));

  lua_pushboolean(LuaState, (State in Input.States));

  Result:= 1;
end;

//------------------------------------------------------------------------------
function luaInput_state_set(LuaState: PLuaState): Integer; cdecl;
var Input: TPHXInput;
var State: TPHXInputState;
var Value: Boolean;
begin
  Input := GetInput(LuaState);

  State:= TPHXInputState(luaL_checkinteger(LuaState, 2));
  Value:=                    lua_toboolean(LuaState, 3);

  if Value then
  begin
    Input.States:= Input.States + [State];
  end else
  begin
    Input.States:= Input.States - [State];
  end;

  Result:= 0;
end;



{$ENDREGION}

{$REGION 'luaAudio'}

function luaAudio_Get(LuaState: PLuaState): Integer; cdecl; forward;
function luaAudio_Set(LuaState: PLuaState): Integer; cdecl; forward;
function luaAudio_load(LuaState: PLuaState): Integer; cdecl; forward;
function luaAudio_play(LuaState: PLuaState): Integer; cdecl; forward;
function luaAudio_stop(LuaState: PLuaState): Integer; cdecl; forward;
function luaAudio_pause(LuaState: PLuaState): Integer; cdecl; forward;
function luaAudio_resume(LuaState: PLuaState): Integer; cdecl; forward;

//------------------------------------------------------------------------------
procedure luaAudio(LuaState: PLuaState; Audio: TPHXAudioEngine);
begin
  // Create audio table
  lua_newtable(LuaState);
    // create the meta table
    lua_newtable(LuaState);

    lua_pushstring(LuaState, '__index');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_Get, 1);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, '__newindex');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_Set, 1);
    lua_rawset(LuaState, -3);

    lua_setmetatable(LuaState, -2);

    lua_pushstring(LuaState, 'load');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_load, 1);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, 'play');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_play, 1);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, 'stop');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_stop, 1);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, 'pause');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_pause, 1);
    lua_rawset(LuaState, -3);

    lua_pushstring(LuaState, 'resume');
    lua_pushlightuserdata(LuaState, Audio);
    lua_pushcclosure(LuaState, @luaAudio_resume, 1);
    lua_rawset(LuaState, -3);
end;

//------------------------------------------------------------------------------
function getAudio(LuaState: PLuaState): TPHXAudioEngine; inline;
begin
  Result:= TPHXAudioEngine(lua_topointer(LuaState, lua_upvalueindex(1)));
end;

//------------------------------------------------------------------------------
function luaAudio_Get(LuaState: PLuaState): Integer; cdecl;
var Audio : TPHXAudioEngine;
var Prop : String;
begin
  Audio:= getAudio(LuaState);
  Prop := String(lua_tostring(LuaState, 2));

  // Get the master volume
  if SameText(Prop, 'volume') then
  begin
    lua_pushnumber(LuaState, Audio.Volume);

    Result:= 1;
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
function luaAudio_Set(LuaState: PLuaState): Integer; cdecl;
var Audio : TPHXAudioEngine;
var Prop : String;
begin
  Audio:= getAudio(LuaState);
  Prop := String(lua_tostring(LuaState, 2));

  // Set the master volume
  if SameText(Prop, 'volume') then
  begin
    Audio.Volume:= lua_tonumber(LuaState, 3);
  end else
  begin
  end;

  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaAudio_load(LuaState: PLuaState): Integer; cdecl;
var Audio : TPHXAudioEngine;
var Sample: String;
var Name  : String;
begin
  Audio:= getAudio(LuaState);

  // sample.load(filename)
  if lua_gettop(LuaState) = 1 then
  begin
    Sample:= String(luaL_checkstring(LuaState, 1));

    Audio.LoadSample(Sample)
  end else
  // sample.load(filename, name)
  if lua_gettop(LuaState) = 2then
  begin
    Sample:= String(luaL_checkstring(LuaState, 1));
    Name  := String(luaL_checkstring(LuaState, 2));

    Audio.LoadSample(Sample, Name);
  end else
  begin
    lua_pushstring(LuaState, PAnsiChar(SInvalidParameters));
    lua_error(LuaState);
  end;

  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaAudio_play(LuaState: PLuaState): Integer; cdecl;
var Audio : TPHXAudioEngine;
var Sample: String;
var Volume: Single;
begin
  Audio:= getAudio(LuaState);

  // audio.play(sample)
  if lua_gettop(LuaState) = 1 then
  begin
    Sample:= String(luaL_checkstring(LuaState, 1));

    Audio.Play(Sample);
  end else
  // audio.play(sample, volume)
  if lua_gettop(LuaState) = 2 then
  begin
    Sample:= String(luaL_checkstring(LuaState, 1));
    Volume:=        luaL_checknumber(LuaState, 2);

    Audio.Play(Sample, Volume);
  end else
  begin
    lua_pushstring(LuaState, PAnsiChar(SInvalidParameters));
    lua_error(LuaState);
  end;

  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaAudio_stop(LuaState: PLuaState): Integer; cdecl;
var Audio: TPHXAudioEngine;
begin
  Audio:= getAudio(LuaState);

  Audio.Stop;

  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaAudio_pause(LuaState: PLuaState): Integer; cdecl;
var Audio: TPHXAudioEngine;
begin
  Audio:= getAudio(LuaState);

  Audio.Pause;

  Result:= 0;
end;

//------------------------------------------------------------------------------
function luaAudio_resume(LuaState: PLuaState): Integer; cdecl;
var Audio: TPHXAudioEngine;
begin
  Audio:= getAudio(LuaState);

  Audio.Resume;

  Result:= 0;
end;

{$ENDREGION}


(*

  //Script.LoadScript(ContentPath + 'gui.lua');
  //Script.Compile;
  //loadGui(Script.State, 'gui');
  //Halt(0);

var Output: TStrings;

procedure createbutton(LuaState: PLuaState);
var Prop   : AnsiString;
var Value  : AnsiString;
begin

  // Push the start iterator key
  lua_pushnil(LuaState);
  while lua_next(LuaState, -2) <> 0 do
  begin
    Prop := lua_tostring(LuaState, -2);
    Value:= lua_tostring(LuaState, -1);

    Output.Add('button.'+ Prop +'=' + Value );

    lua_pop(LuaState, 1);
  end;
  Output.Add('');
end;

procedure loadGui(LuaState: PLuaState; const Name: AnsiString);
var Control: AnsiString;
var Prop   : AnsiString;
var Value  : AnsiString;
begin
  Output:= TStringList.Create;
  lua_getglobal(LuaState, PAnsiChar(Name));

  if lua_istable(LuaState, 1) then
  begin
    // Push the start iterator key
    lua_pushnil(LuaState);

    while lua_next(LuaState, -2) <> 0 do
    begin
      Prop:= lua_tostring(LuaState, -2);

      // set the name of the guo
      if Prop = 'name' then
      begin
        Value:= lua_tostring(LuaState, -1);

        Output.Add('gui.name=' + value)
      end else
      // add a button to the gui
      if Prop = 'button' then
      begin
        createbutton(LuaState);
      end;

      lua_pop(LuaState, 1);
    end;


    lua_pop(LuaState, 1);
  end else
  begin
    lua_pop(LuaState, 1);

    raise Exception.CreateFmt('The script variable %s does not contains a lua table', [Name]);
  end;

  ShowMessage(Output.Text);

end;

*)



end.
