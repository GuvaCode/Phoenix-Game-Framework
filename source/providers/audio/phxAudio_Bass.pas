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
unit phxAudio_Bass;
//< Audio engine using BASS
//< http://www.un4seen.com/bass.html

interface

{$I ../source/phxConfig.inc}

uses
  SysUtils, Classes,

  {$IFDEF FPC}
  lazdynamic_bass
  {$ELSE}
  bass
  {$ENDIF},

  phxClasses,
  phxAudio;

type

TPHXBassDevice = class;

// Provider for Bass
// See: @link(TPHXAudioProvider)
//------------------------------------------------------------------------------
TPHXBassProvider = class(TPHXAudioProvider)
  protected
    // Return the name of the provider
    function GetName: String; override;
  public
    constructor Create; override;
    // Load the provider
    function CreateDevice(const Flags: Cardinal): IPHXAudioDevice; override;
  end;

// Audio sample for Bass
// See: @link(TPHXAudioSample)
//------------------------------------------------------------------------------
TPHXBassSample = class(TPHXAudioSample)
  private
    FDevice: TPHXBassDevice;
    FHandle: HSAMPLE;
    FInfo  : BASS_SAMPLE;
  protected
    procedure LoadSample(Stream: TStream); override;

    procedure SetVolume(const Volume: Single); override;
  public
    constructor Create(ADevice: TPHXBassDevice);
    destructor Destroy; override;

    // Play this sample
    procedure Play; override;
    // Play this sample with a given volume
    procedure Play(const Volume: Single); override;
     // Stops all instances of this sample.
    procedure Stop; override;

    // The bass handle
    property Device: TPHXBassDevice read FDevice;
    // The bass handle
    property Handle: HSAMPLE read FHandle write FHandle;
  end;

// Audio stream for Bass
// See: @link(TPHXAudioStream)
//------------------------------------------------------------------------------
TPHXBassStream = class(TPHXAudioStream)
  private
    FDevice: TPHXBassDevice;
    FHandle: HSTREAM;
  protected
    procedure LoadStream(const Name: String); override;

    function GetDuration: Double; override;
    function GetPosition: Double; override;

    procedure SetVolume(const Value: Single); override;
  public
    constructor Create(ADevice: TPHXBassDevice);
    destructor Destroy; override;

    procedure Play; override;
    procedure Stop; override;
    procedure Restart; override;


    // The bass handle
    property Device: TPHXBassDevice read FDevice;
    // The bass handle
    property Handle: HSTREAM read FHandle write FHandle;
  end;

// Audio device for Bass
// See: @link(IPHXAudioDevice)
//------------------------------------------------------------------------------
TPHXBassDevice = class(TInterfacedObject, IPHXAudioDevice)
  private
    // Retrieves the current master volume level.
    function GetVolume: Double;
    // Sets the output master volume.
    procedure SetVolume(const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    // Enumerate all supported devices
    procedure EnumerateDevices(List: TPHXSoundDeviceList);

    // Initialize the audio engine using the default device
    procedure Initialize; overload;
    // Initialize the audio engine using a proviced device
    procedure Initialize(const Device: TPHXSoundDevice); overload;

    // Create a audio sample
    function CreateSample: TPHXAudioSample;
    // Create a audio stream
    function CreateStream: TPHXAudioStream;

    // Update the audio engine
    procedure Update;
    // Stops all channels
    procedure Stop;
    // Pause all channels
    procedure Pause;
    // Resume all active channels
    procedure Resume;
  end;

implementation

//------------------------------------------------------------------------------
function GetBassError: String;
var Error: Integer;
begin
  Error:= BASS_ErrorGetCode();
  Result:= '';

  case Error of
    BASS_ERROR_INIT      : Result:= 'BASS_Init has not been successfully called.';
    BASS_ERROR_NOTAVAIL  : Result:= 'Sample functions are not available when using the "no sound" device.';
    BASS_ERROR_ILLPARAM  : Result:= 'max and/or length is invalid. The length must be specified when loading from memory.';
    BASS_ERROR_FILEOPEN  : Result:= 'The file could not be opened.';
    BASS_ERROR_FILEFORM  : Result:= 'The files format is not recognised/supported.';
    BASS_ERROR_CODEC     : Result:= 'The file uses a codec that''s not available/supported. This can apply to WAV and AIFF files, and also MP3 files when using the "MP3-free" BASS version.';
    BASS_ERROR_FORMAT    : Result:= 'The sample format is not supported by the device/drivers. If the sample is more than stereo or the BASS_SAMPLE_FLOAT flag is used, it could be that they are not supported.';
    BASS_ERROR_MEM       : Result:= 'There is insufficient memory.';
    BASS_ERROR_NO3D      : Result:= 'Could not initialize 3D support.';
    BASS_ERROR_UNKNOWN   : Result:= 'Some other mystery problem!';
    else                    Result:= IntToStr(Error);
  end;
end;

//------------------------------------------------------------------------------
procedure Bass_CloseProc(user: Pointer); stdcall;
var Stream: TStream;
begin
  Stream:= TStream(User);
  Stream.Free;
end;

//------------------------------------------------------------------------------
function Bass_SizeProc(user: Pointer): QWORD; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(User);

  Result:= Stream.Size;
end;

//------------------------------------------------------------------------------
function Bass_ReadProc(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(User);

  Result:= Stream.Read(Buffer^, Length);
end;

//------------------------------------------------------------------------------
function Bass_SeekProc(offset: QWORD; user: Pointer): BOOL; stdcall;
var Stream: TStream;
begin
  Stream:= TStream(User);

  Result:= Stream.Seek(offset, soFromBeginning) > 0;
end;

var FileProcs: BASS_FILEPROCS;

{$REGION 'TPHXBassProvider'}

// TPHXBassProvider
//==============================================================================
constructor TPHXBassProvider.Create;
begin
  inherited;

end;

//------------------------------------------------------------------------------
function TPHXBassProvider.GetName: String;
begin
  Result:= 'Bass';
end;

//------------------------------------------------------------------------------
function TPHXBassProvider.CreateDevice(const Flags: Cardinal): IPHXAudioDevice;
begin
  Result:= TPHXBassDevice.Create;
end;

{$ENDREGION}

{$REGION 'TPHXBassSample'}

//function BASS_SampleLoad(mem: BOOL; f: Pointer; offset: QWORD; length, max, flags: DWORD): HSAMPLE; stdcall; external bassdll;
//function BASS_SampleCreate(length, freq, chans, max, flags: DWORD): HSAMPLE; stdcall; external bassdll;
//function BASS_SampleFree(handle: HSAMPLE): BOOL; stdcall; external bassdll;
//function BASS_SampleSetData(handle: HSAMPLE; buffer: Pointer): BOOL; stdcall; external bassdll;
//function BASS_SampleGetData(handle: HSAMPLE; buffer: Pointer): BOOL; stdcall; external bassdll;
//function BASS_SampleGetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; stdcall; external bassdll;
//function BASS_SampleSetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; stdcall; external bassdll;
//function BASS_SampleGetChannel(handle: HSAMPLE; onlynew: BOOL): HCHANNEL; stdcall; external bassdll;
//function BASS_SampleGetChannels(handle: HSAMPLE; channels: Pointer): DWORD; stdcall; external bassdll;
//function BASS_SampleStop(handle: HSAMPLE): BOOL; stdcall; external bassdll;


// http://www.un4seen.com/forum/?topic=7962.msg54257#msg54257
//==============================================================================
constructor TPHXBassSample.Create(ADevice: TPHXBassDevice);
begin
  FDevice:= ADevice;
  FHandle:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXBassSample.Destroy;
begin
  if Handle <> 0 then
  begin
    BASS_SampleFree(Handle);
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXBassSample.LoadSample(Stream: TStream);
var Flags : DWORD;
var Size  : Cardinal;
var Buffer: PByte;
begin
  Flags:= BASS_SAMPLE_OVER_POS;
  Size := Stream.Size;

  GetMem(Buffer, Size);
  try
    Stream.Read(Buffer^, Size);

    // http://www.un4seen.com/doc/#bass/BASS_SampleLoad.html
    FHandle:= BASS_SampleLoad(TRUE, Buffer, 0, Size, 3, Flags);

    if Handle = 0 then
    begin
      raise Exception.CreateFmt('Bass error: %s', [GetBassError]);
    end;

    BASS_SampleGetInfo(FHandle, FInfo);

  finally
    FreeMem(Buffer, Size);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBassSample.Play;
var Channel: HCHANNEL;
begin
  Channel:= BASS_SampleGetChannel( FHandle, False);

  if not BASS_ChannelPlay(Channel, False) then
  begin
    raise Exception.CreateFmt('Bass error: %s', [GetBassError]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBassSample.Play(const Volume: Single);
var Channel: HCHANNEL;
begin
  Channel:= BASS_SampleGetChannel(FHandle, False);

  BASS_ChannelSetAttribute(Channel, BASS_ATTRIB_VOL, Volume);

  if not BASS_ChannelPlay(Channel, False) then
  begin
    raise Exception.CreateFmt('Bass error: %s', [GetBassError]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBassSample.Stop;
begin
  BASS_SampleStop(FHandle);
end;
(*
BASS_SAMPLE info;
HCHANNEL *channels;
DWORD a, count;
BASS_SampleGetInfo(sample, &info); // get sample info for "max" value
channels=malloc(info.max*sizeof(HCHANNEL)); // allocate channels array
count=BASS_SampleGetChannels(sample, channels); // get the channels
for (a=0; a<count; a++) // go through them all and...
    BASS_ChannelSetAttribute(channels[a], BASS_ATTRIB_FREQ, 10000); // set the sample rate to 10000
free(channels); // free the channels array
*)

type
  PCHANNELList =^HCHANNELList;
  HCHANNELList = array[0..$00FFFFFF] of HCHANNEL;

//------------------------------------------------------------------------------
procedure TPHXBassSample.SetVolume(const Volume: Single);
var Count: Integer;
var Index: Integer;
var Channels: PCHANNELList;
begin
  inherited SetVolume(Volume);

  Count:= BASS_SampleGetChannels(FHandle, nil);

  GetMem(Channels, Count * SizeOf(HCHANNEL));
  try
    BASS_SampleGetChannels(FHandle, Channels);

    for Index := 0 to Count-1 do
    begin
      BASS_ChannelSetAttribute(Channels^[Index], BASS_ATTRIB_VOL, Volume);
    end;
  finally
    FreeMem(Channels);
  end;
end;

{$ENDREGION}

{$REGION 'TPHXBassStream'}

// TPHXBassStream
//==============================================================================
constructor TPHXBassStream.Create(ADevice: TPHXBassDevice);
begin
  inherited Create;
  FDevice:= ADevice;
end;

//------------------------------------------------------------------------------
destructor TPHXBassStream.Destroy;
begin
  if FHandle <> 0 then
  begin
    BASS_StreamFree(FHandle);
  end;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXBassStream.LoadStream(const Name: String);
var FileName: String;
var f: PChar;
var Flags: DWORD;
begin
  FileName:= TPHXContentManager.ExpandFileName(Name, ckAudio);

  f:= PChar( FileName);


  Flags:= BASS_SAMPLE_OVER_POS {$IFDEF UNICODE} or BASS_UNICODE {$ENDIF};

  FHandle:= BASS_StreamCreateFile(FALSE, f, 0, 0, Flags);

  if Handle = 0 then
  begin
    raise Exception.CreateFmt('Bass error: %s', [GetBassError]);
  end;
end;

  // http://www.un4seen.com/doc/#bass/BASS_ChannelFlags.html
//------------------------------------------------------------------------------
procedure TPHXBassStream.Play;
begin
  BASS_ChannelSetAttribute(FHandle, BASS_ATTRIB_VOL, Volume);

  if Looped then
  begin
    BASS_ChannelFlags(FHandle, BASS_SAMPLE_LOOP, BASS_SAMPLE_LOOP);
  end else
  begin
    BASS_ChannelFlags(FHandle, 0, BASS_SAMPLE_LOOP);
  end;

  if not BASS_ChannelPlay(Handle, True) then
  begin
    raise Exception.CreateFmt('Bass error: %s', [GetBassError]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBassStream.Restart;
begin
  if Looped then
  begin
    BASS_ChannelSetAttribute( Handle, BASS_SAMPLE_LOOP, BASS_SAMPLE_LOOP);
  end else
  begin
    BASS_ChannelSetAttribute( Handle, 0, BASS_SAMPLE_LOOP);
  end;

  BASS_ChannelSetAttribute(FHandle, BASS_ATTRIB_VOL, Volume);

  if not BASS_ChannelPlay(Handle, True) then
  begin
    raise Exception.CreateFmt('Bass error: %s', [GetBassError]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBassStream.Stop;
begin
  BASS_ChannelStop(FHandle);
end;

//------------------------------------------------------------------------------
function TPHXBassStream.GetDuration: Double;
var Position: QWORD;
begin
  Position:= BASS_ChannelGetLength(FHandle, BASS_POS_BYTE);

  Result:= BASS_ChannelBytes2Seconds(FHandle, Position);
end;

//------------------------------------------------------------------------------
function TPHXBassStream.GetPosition: Double;
var Position: QWORD;
begin
  Position:= BASS_ChannelGetPosition(FHandle, BASS_POS_BYTE);

  Result:= BASS_ChannelBytes2Seconds(FHandle, Position);
end;

//------------------------------------------------------------------------------
procedure TPHXBassStream.SetVolume(const Value: Single);
begin
  inherited;
  BASS_ChannelSetAttribute(FHandle, BASS_ATTRIB_VOL, Volume);
end;

{$ENDREGION}

{$REGION 'TPHXBassDevice'}

// TPHXBassDevice
//==============================================================================
constructor TPHXBassDevice.Create;
begin
  {$IFDEF FPC}
  Load_BASSDLL(BASS_name);
  {$ENDIF}
  FileProcs.close := {$IFDEF FPC}@{$ENDIF}Bass_CloseProc;
  FileProcs.length:= {$IFDEF FPC}@{$ENDIF}Bass_SizeProc;
  FileProcs.read  := {$IFDEF FPC}@{$ENDIF}Bass_ReadProc;
  FileProcs.seek  := {$IFDEF FPC}@{$ENDIF}Bass_SeekProc;
end;

//------------------------------------------------------------------------------
destructor TPHXBassDevice.Destroy;
begin
  BASS_Free();
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXBassDevice.EnumerateDevices(List: TPHXSoundDeviceList);
var Device : BASS_DEVICEINFO;
var Index: DWORD;
begin
  List.Clear;
  FillChar(Device, SizeOf(Device), 0);
  Index:=0 ;
  while BASS_GetDeviceInfo(Index, Device) do
  begin
    List.Add(Index, String(Device.Name));
    Inc(Index);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBassDevice.Initialize;
begin
  // check the correct BASS was loaded
  if (BASS_GetVersion) shr 16 <> BASSVERSION then
  begin
    raise Exception.CreateFmt('Failed to intialize bass, incorrect version: %d', [BASS_GetVersion() ] );
  end;

  // Initialize audio - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(-1, 44100, 0, 0, nil) then
  begin
    raise Exception.CreateFmt('Failed to intialize bass: %s', [GetBassError] );
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXBassDevice.Initialize(const Device: TPHXSoundDevice);
begin
  // check the correct BASS was loaded
  if (BASS_GetVersion) shr 16 <> BASSVERSION then
  begin
    raise Exception.CreateFmt('Failed to intialize bass, incorrect version: %d', [BASS_GetVersion() ] );
  end;

  // Initialize audio - default device, 44100hz, stereo, 16 bits
  if not BASS_Init(Device.Ident, 44100, 0, 0, nil) then
  begin
    raise Exception.CreateFmt('Failed to intialize bass: %s', [GetBassError] );
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXBassDevice.Update;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXBassDevice.Pause;
begin
  BASS_Pause;
end;

//------------------------------------------------------------------------------
procedure TPHXBassDevice.Resume;
begin
  BASS_Start;
end;

//------------------------------------------------------------------------------
procedure TPHXBassDevice.Stop;
begin
  BASS_Stop();
end;

//------------------------------------------------------------------------------
function TPHXBassDevice.CreateStream: TPHXAudioStream;
begin
  Result:= TPHXBassStream.Create(Self);
end;

//------------------------------------------------------------------------------
function TPHXBassDevice.CreateSample: TPHXAudioSample;
begin
  Result:= TPHXBassSample.Create(Self);
end;

//------------------------------------------------------------------------------
function TPHXBassDevice.GetVolume: Double;
begin
  Result:= BASS_GetVolume();
end;

//------------------------------------------------------------------------------
procedure TPHXBassDevice.SetVolume(const Value: Double);
begin
  BASS_SetVolume(Value);
end;

{$ENDREGION}



initialization
  RegisterAudioProvider('Bass', TPHXBassProvider);
end.
