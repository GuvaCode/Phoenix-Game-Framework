{
  @abstract(Lua scripting interface)

  This page documents the lua scripting interface.

  @italic(This unit or the classes in it doesn't exist in the source but is avaiable from lua scripts.)

  @seealso(TPHXScript)
}
unit scripting;

interface

uses Lua,

  phxFramework,
  phxInput,
  phxAudio;

type

{ @exclude }
TLuaTable = class(TluaTable);

{$REGION 'TLuaTimer'}

// @abstract(Scripting support for the timer)
//------------------------------------------------------------------------------
TLuaTimer = class(TLuaTable)
  private
  public
    // @abstract(Returns the elapsed time since the start of the application)
    property ElapsedTime: number read FElapsedTime;
    // @abstract(Returns the time since the last frame)
    property FrameTime: number read FFrameTime;
    // @abstract(Returns the current frame rate)
    property FrameRate: integer read FFrameRate;
  end;

{$ENDREGION}

{$REGION 'TLuaMouse'}

// @abstract(Scripting support for mouse)
//------------------------------------------------------------------------------
TLuaMouse = class(TLuaTable)
  private
    FX: integer;
    FY: integer;
    function GetButton(index: integer): Boolean;
  public
    // @abstract(Returns the current X position of the mouse)
    property X: integer read FX;
    // @abstract(Returns the current Y position of the mouse)
    property Y: integer read FY;
    // @abstract(Returns the state for a mouse button)
    // @param(button The button to query)
    // @returns(True if the button is pressed)
    property Buttons[button: integer]: Boolean read GetButton;
  end;

{$ENDREGION}

{$REGION 'TLuaKeyboard'}

// @abstract(Scripting support for keyboard)
//------------------------------------------------------------------------------
TLuaKeyboard = class(TLuaTable)
  private
    function GetKey(key: integer): Boolean;
    // @abstract(Returns the state for a keyboard key)
    property Buttons[key: integer]: Boolean read GetKey;
  end;

{$ENDREGION}

{$REGION 'TLuaInput'}

//  @abstract(Scripting support for input)
//
//  This is a wrapper class for the @link(TPHXInput) component.
//
//  @seealso(TPHXInput)
//------------------------------------------------------------------------------
TLuaInput = class(TLuaTable)
  private
    Fmouse: TLuaMouse;
    function GetState(state: TPHXInputState): Boolean;
  public
    // @abstract(Return the status of a input state)
    property states[state: TPHXInputState]: Boolean read GetState;
    // @abstract(Return the mouse from the input)
    property mouse: TLuaMouse read Fmouse;
    // @abstract(Return the keyboard from the input)
    property keyboard: TLuaKeyboard read Fmouse;
  end;

{$ENDREGION}

{$REGION 'TLuaAudio'}

//  @abstract(Scripting support for audio)
//
//  This is a wrapper class for the @link(TPHXAudioEngine) component.
//
//  @seealso(TPHXAudioEngine)
//------------------------------------------------------------------------------
TLuaAudio = class(TLuaTable)
  public
    // @abstract(Load a audio sample)
    //
    // @param(filename Filename of the audio sample to load)
    //
    // @seealso(TPHXAudioEngine.LoadSample)
    procedure load(filename: string); overload;
    // @abstract(Load and rename audio sample)
    //
    // @param(filename Filename of the audio sample to load)
    // @param(Name The name to give the sample)
    //
    // @seealso(TPHXAudioEngine.LoadSample)
    procedure load(filename: string; name: string); overload;

    //  @abstract(Plays a audio sample)
    //
    //  @param(sample Name of the audio sample to play)
    //
    //  @seealso(TPHXAudioEngine.Play)
    procedure play(sample: string); overload;
    //  @abstract(Play a audio sample by name with a volume)
    //
    //  $param(sample Name of the audio sample to play)
    //  @param(volume The volume of the sample from 0.0 (silent) to 1.0 (full))
    procedure play(sample: string; volume: Single); overload;
    // @abstract(Stops all playing audio channels)
    //
    //  @code(
    //    audio.stop
    //  )
    //
    // @seealso(TPHXAudioEngine.Stop)
    procedure stop;
    // @seealso(TPHXAudioEngine.Pause)
    procedure pause;
    // @seealso(TPHXAudioEngine.Resume)
    procedure resume;
  end;

{$ENDREGION}

{$REGION 'Functions'}


// @abstract(Prints text for debuging)
// @param(values The text or variable to print)
//
// The print function prints all the texts and variables that are passed to the function
//
// @bold(Sample)
//
// @longcode(#
//   print("hello", "world")
//   print(x)
// #)
// @seealso(TPHXScript.OnPrint)
//------------------------------------------------------------------------------procedure print(values: array of variant);

// @abstract(Formats a value)
// @param(format The format string)
// @param(values The values to format)
//
//------------------------------------------------------------------------------
procedure format(format: string; values: array of variant);

{$ENDREGION}

{$REGION 'Variables'}

// @abstract(the timer namespace)
var timer: TLuaTimer;

// @abstract(the input namespace)
//
// @bold(Sample)
//
// @longcode(
//   if(input.states[isButton1]) then
//      fire(2)
//   end
// )
//------------------------------------------------------------------------------
var input: TLuaInput;

// @abstract(the audio namespace)
//
// @bold(Sample)
// @longcode(
//   -- Load a sample from disk
//   audio.load("Boom.ogg")
//   -- Play the sample
//   audio.play("Boom", 0.5)
// )
//------------------------------------------------------------------------------
var audio: TLuaAudio;

{$ENDREGION}

implementation
{

// @returns(@true on success, @false otherwise)

 Creates a lua table with the name audio with the following exported functions

 @bold(Exported functions)
 @definitionList(
   @itemLabel(audio.play(sample: string))
   @item(Plays a @link(TPHXAudioSample) by name)

   @itemLabel(audio.stop())
   @item(Stops all playing audio channels)

   @itemLabel(audio.pause())
   @item(Pauses all playing audio channels)

   @itemLabel(audio.resume())
   @item(Resumes all paused audio channels)
 )
 @bold(Exported variables)
 @definitionList(
   @itemLabel(audio.volume: number)
   @item(Gets or sets the output master volume, from 0.0 (silent) to 1.0 (full))
 )

}




{ TLuaMouse }

function TLuaMouse.GetButton(index: integer): Boolean;
begin

end;

{ TLuaKeyboard }

function TLuaKeyboard.GetKey(key: integer): Boolean;
begin

end;


{ TLuaInput }

function TLuaInput.GetState(state: TPHXInputState): Boolean;
begin

end;

end.