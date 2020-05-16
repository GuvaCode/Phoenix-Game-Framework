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
unit phxAudio;
//< Audio engine

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxTypes,
  phxClasses;

type

// Forward declarations
TPHXAudioEngine = class;
TPHXAudioSample = class;
IPHXAudioDevice = interface;

{$REGION 'TPHXSoundDevice'}

// Information about a sound device
//------------------------------------------------------------------------------
TPHXSoundDevice = record
  // Name of the sound device
  Name: String[128];
  // The identifier of the deviced, used by the sound provider to select a device
  Ident: Cardinal;
end;

PSoundDeviceList = ^TSoundDeviceList;
TSoundDeviceList = array[0..$001FFFFF] of TPHXSoundDevice;

// List container for floats
//------------------------------------------------------------------------------
TPHXSoundDeviceList = class
  private
    FCount   : Integer;
    FCapacity: Integer;

    FList: PSoundDeviceList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXSoundDevice;
    procedure SetItem(Index: Integer; const Value: TPHXSoundDevice);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Creates a new sound device list
    constructor Create;
    // Destroys this list
    destructor Destroy; override;

    // Removes all devices from the list
    procedure Clear;

    // Add a devices to the list
    procedure Add(const Value: TPHXSoundDevice); overload;
    // Add a devices to the list
    procedure Add(const Ident: Cardinal; const Name: String); overload;

    // Find a sound device by name, returns true if the device exists
    function Find(const Name: String; out Device: TPHXSoundDevice): Boolean;

    // The current number of items in the list
    property Count: Integer read FCount write SetCount;
    // The current capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PSoundDeviceList read FList;
    // Gets and sets items in the list
    property Items[Index: Integer]: TPHXSoundDevice read GetItem Write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXAudioProvider'}

TPHXAudioProviderClass = class of TPHXAudioProvider;

// The audio provider is the factory function for creating audio devices
//------------------------------------------------------------------------------
TPHXAudioProvider = class
  protected
    // Return the name of the provider
    function GetName: String; virtual;
  public
    constructor Create; virtual;

    // Load the provider
    function CreateDevice(const Flags: Cardinal): IPHXAudioDevice; virtual;

    // The name of the provider
    property Name: String read GetName;
  end;

{$ENDREGION}

{$REGION 'TPHXAudioSource'}

// Abstract audio source
//------------------------------------------------------------------------------
TPHXAudioSource = class
  private
    FName: String;
  public
    // Name of the audio source
    property Name: String read FName write FName;
  end;

{$ENDREGION}

{$REGION 'TPHXAudioSample'}

// A Audio source that is loaded into memory before playing
//------------------------------------------------------------------------------
TPHXAudioSample = class(TPHXAudioSource)
  private
    FVolume: Single;
  protected
    // Load the sample from a stream
    procedure LoadSample(Stream: TStream); virtual; abstract;

    procedure SetVolume(const Volume: Single); virtual;
  public
    // Creates a new audio sample
    constructor Create;

    // Load the audio sample from a file
    procedure LoadFromFile(const FileName: String);
    // Load the audio sample from a steam
    procedure LoadFromStream(Stream: TStream);

    // Play this sample
    procedure Play; overload; virtual; abstract;
    // Play this sample with a given volume
    procedure Play(const Volume: Single); overload; virtual; abstract;
    // Stops all instances of this sample.
    procedure Stop; virtual; abstract;

    // The volume level, from 0.0 (silent) to 1.0 (full)
    property Volume: Single read FVolume write SetVolume;
  end;

PAudioSampleList = ^TAudioSampleList;
TAudioSampleList = array[0..$00FFFFFF] of TPHXAudioSample;

// List of audio samples
//------------------------------------------------------------------------------
TPHXAudioSamples = class
  private
    FEngine: TPHXAudioEngine;
    FList: TList ;

    function GetCount: Integer;
    function GetList: PAudioSampleList;
    function GetItem(Index: Integer): TPHXAudioSample;
  public
    // Create the audio sample list
    constructor Create(AEngine: TPHXAudioEngine);
    // Free the list
    destructor Destroy; override;

    // Remove and free all sounds
    procedure Clear;

    // Load the audio sample from a file
    function LoadSample(const FileName: String): TPHXAudioSample; overload;
    // Load the audio sample from a steam
    function LoadSample(Stream: TStream; const Name: String): TPHXAudioSample; overload;

    // Find a sample by name
    function Find(const Name: String): TPHXAudioSample; overload;
    // Find a sample by name
    function Find(const Name: String; out Sample: TPHXAudioSample): Boolean; overload;

    // The owning audio engine
    property Engine: TPHXAudioEngine read FEngine;
    // Number of loaded sounds
    property Count: Integer read GetCount;
    // Return a pointer to the internal list
    property List: PAudioSampleList read GetList;
    // The sounds in the list
    property Items[Index: Integer]: TPHXAudioSample read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXAudioStream'}

// A audio source that is streamed from disk when playing
// Use this for music
//------------------------------------------------------------------------------
TPHXAudioStream = class(TPHXAudioSource)
  private
    FLooped: Boolean;
    FVolume: Single;
  protected
    // Load the stream from a filename
    procedure LoadStream(const Name: String); virtual; abstract;

    // Returns the length of the stream in seconds
    function GetDuration: Double; virtual; abstract;
    // Returns the current position of the stream in seconds
    function GetPosition: Double; virtual; abstract;

    // Set the volume level, from 0.0 (silent) to 1.0 (full)
    procedure SetVolume(const Value: Single); virtual;
    // Set if the stream should be looped
    procedure SetLooped(const Value: Boolean); virtual;
  public
    // Creates a new audio stream
    constructor Create;

    // Play the stream
    procedure Play; virtual; abstract;
    // Stop the stream
    procedure Stop; virtual; abstract;
    // Restarts the stream
    procedure Restart; virtual; abstract;

    // The volume level, from 0.0 (silent) to 1.0 (full)
    property Volume: Single read FVolume write SetVolume;
    // If the stream should be looped
    property Looped: Boolean read FLooped write SetLooped;
    // Returns the length of the stream in seconds
    property Duration: Double read GetDuration;
    // Returns the current position of the stream in seconds
    property Position: Double read GetPosition;
  end;

PAudioStreamList = ^TAudioStreamList;
TAudioStreamList = array[0..$00FFFFFF] of TPHXAudioStream;

// List of audio samples
//------------------------------------------------------------------------------
TPHXAudioStreams = class
  private
    FEngine: TPHXAudioEngine;
    FList  : TList ;

    function GetCount: Integer;
    function GetList: PAudioStreamList;
    function GetItem(Index: Integer): TPHXAudioStream;
  public
    // Create the audio sample list
    constructor Create(AEngine: TPHXAudioEngine);
    // Free the list
    destructor Destroy; override;

    // Remove and free all sounds
    procedure Clear;

    // Load the audio sample from a file
    function LoadStream(const FileName: String): TPHXAudioStream;

    // Find a stream by name
    function Find(const Name: String): TPHXAudioStream; overload;
    // Find a stream by name
    function Find(const Name: String; out Stream: TPHXAudioStream): Boolean; overload;

    // The owning audio engine
    property Engine: TPHXAudioEngine read FEngine;
    // Number of loaded sounds
    property Count: Integer read GetCount;
    // Return a pointer to the internal list
    property List: PAudioStreamList read GetList;
    // The sounds in the list
    property Items[Index: Integer]: TPHXAudioStream read GetItem; default;
  end;

{$ENDREGION}

{$REGION 'IPHXAudioDevice'}

// Interface for a audio device
//------------------------------------------------------------------------------
IPHXAudioDevice = interface
  ['{F11793C4-5DFF-41AD-AE14-57EC65307374}']

  // Enumerate all supported devices
  procedure EnumerateDevices(List: TPHXSoundDeviceList);

  // Initialize the audio engine using the default device
  procedure Initialize; overload;
  // Initialize the audio engine using a proviced device
  procedure Initialize(const Device: TPHXSoundDevice); overload;

  // Create a audio sample
  function CreateSample: TPHXAudioSample;
  // Create a audio channel
//  function CreateChannel: TPHXAudioChannel;
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
  // Retrieves the current master volume level.
  function GetVolume: Double;
  // Sets the output master volume.
  procedure SetVolume(const Value: Double);
end;

 {$ENDREGION}

// The audio engine is used to play audio using a audio provider
//------------------------------------------------------------------------------
TPHXAudioEngine = class
  private
    FInitialized: Boolean;
    FProvider   : TPHXAudioProvider;
    FDevices    : TPHXSoundDeviceList;
    FDevice     : IPHXAudioDevice;
    FSamples    : TPHXAudioSamples;
    FStreams    : TPHXAudioStreams;

    function GetVolume: Double;

    procedure SetVolume(const Value: Double);
  public
    // Create the audio engine using the default provicer
    constructor Create; overload;
    // Create the audio engine using a custom provider
    constructor Create(AProvider: TPHXAudioProvider); overload;
    // Free the audio engine
    destructor Destroy; override;

    // Initialize the audio engine using the default device
    procedure Initialize; overload;
    // Initialize the audio engine using a provided device
    procedure Initialize(const Device: TPHXSoundDevice); overload;
    // Initialize the audio engine using the device at a given index
    procedure Initialize(const DeviceIndex: Integer); overload;
    // Initialize the audio engine using a device by name
    procedure Initialize(const DeviceName: string); overload;

    // Load a audio sample
    function LoadSample(const FileName: String): TPHXAudioSample; overload;
    // Load a audio sample and rename it
    function LoadSample(const FileName: String; const Name: String): TPHXAudioSample; overload;
    // Load a audio stream
    function LoadStream(const FileName: String): TPHXAudioStream; overload;
    // Load a audio stream and rename it
    function LoadStream(const FileName: String; const Name: String): TPHXAudioStream; overload;

    // Update the audio engine
    procedure Update;

    // Play a audio sample by name
    procedure Play(const Sample: String); overload;
    // Play a audio sample by name with a volume from 0.0 (silent) to 1.0 (full)
    procedure Play(const Sample: String; const Volume: Single); overload;

    // Stops all channels
    procedure Stop;
    // Pause all channels
    procedure Pause;
    // Resume all active channels
    procedure Resume;

    // List of known audio devices
    property Devices: TPHXSoundDeviceList read FDevices;
    // List of loaded samples
    property Samples: TPHXAudioSamples read FSamples;
    // List of loaded samples
    property Streams: TPHXAudioStreams read FStreams;
    // Gets or sets the output master volume, from 0.0 (silent) to 1.0 (full)
    property Volume: Double read GetVolume write SetVolume;
    // The current audio device interface
    property Device: IPHXAudioDevice read FDevice;
  end;


  // Register a audio provider
procedure RegisterAudioProvider(const Name: String; const Provider: TPHXAudioProviderClass);

implementation

resourcestring
  SSampleMissing = 'The audio sample %s was not found';



// The default provider
var DefaultProvider:  TPHXAudioProviderClass;

//------------------------------------------------------------------------------
var ProviderRegistry : array of record
  Name: String;
  Provider: TPHXAudioProviderClass;
end;

//------------------------------------------------------------------------------
procedure RegisterAudioProvider(const Name: String; const Provider: TPHXAudioProviderClass);
var Index: Integer;
begin
  Index:= Length(ProviderRegistry);

  SetLength(ProviderRegistry, Index + 1);

  ProviderRegistry[Index].Name    := Name;
  ProviderRegistry[Index].Provider:= Provider;

  // If this is the first registered provider, set this as the default one
  if DefaultProvider = nil then
  begin
    DefaultProvider:= Provider;
  end;
end;


{$REGION 'TPHXSoundDeviceInfoList'}

// TPHXSoundDeviceList
//==============================================================================
constructor TPHXSoundDeviceList.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXSoundDeviceList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
    begin
      Delta := 16
    end else
    begin
      Delta := 4;
    end;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.Add(const Value: TPHXSoundDevice);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.Add(const Ident: Cardinal; const Name: String);
begin
  Inc(FCount);

  if(FCount > FCapacity) then Grow;

  FList^[Count - 1].Ident := Ident;
  FList^[Count - 1].Name  := ShortString(Name);
end;

//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.Clear;
begin
  SetCount(0);
end;

//------------------------------------------------------------------------------
function TPHXSoundDeviceList.Find(const Name: String; out Device: TPHXSoundDevice): Boolean;
var Index: Integer;
begin
  for Index := 0 to FCount-1 do
  begin
    if String(FList^[Index].Name) = Name then
    begin
      Device:= FList^[Index];
      Result:= True;

      Exit;
    end;
  end;
  Device.Ident := 0;
  Device.Name  := '';

  Result:= False;
end;


//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXSoundDevice));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if(FCount > FCapacity) then SetCapacity(FCapacity);
end;

//------------------------------------------------------------------------------
function TPHXSoundDeviceList.GetItem(Index: Integer): TPHXSoundDevice;
begin
  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXSoundDeviceList.SetItem(Index: Integer; const Value: TPHXSoundDevice);
begin
  FList^[Index]:= Value;
end;


{$ENDREGION}

{$REGION 'TPHXAudioProvider'}

// TPHXAudioProvider
//==============================================================================
constructor TPHXAudioProvider.Create;
begin

end;

//------------------------------------------------------------------------------
function TPHXAudioProvider.GetName: String;
begin
  Result:= ClassName;
end;

//------------------------------------------------------------------------------
function TPHXAudioProvider.CreateDevice(const Flags: Cardinal): IPHXAudioDevice;
begin
  raise Exception.Create('The audio provider did not return a valid device.');
end;

{$ENDREGION}



{$REGION 'TPHXAudioSample'}

// TPHXAudioSample
//==============================================================================
constructor TPHXAudioSample.Create;
begin
  FVolume:= 1.0;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioSample.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckAudio, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioSample.LoadFromStream(Stream: TStream);
begin
  LoadSample(Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXAudioSample.SetVolume(const Volume: Single);
begin
  FVolume:= Volume;
end;

{$ENDREGION}

{$REGION 'TPHXAudioSamples'}

// TPHXAudioSamples
//==============================================================================
constructor TPHXAudioSamples.Create(AEngine: TPHXAudioEngine);
begin
  FEngine:= AEngine;
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXAudioSamples.Destroy;
begin
  Clear;

  FList.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioSamples.Clear;
var Index: Integer;
begin
  For Index:= 0 to FList.Count-1 do
  begin
     TPHXAudioSample(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.LoadSample(const FileName: String): TPHXAudioSample;
begin
  Result:= Engine.Device.CreateSample;
  Result.Name:= ExtractFileName(FileName);
  Result.LoadFromFile(FileName);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.LoadSample(Stream: TStream; const Name: String): TPHXAudioSample;
begin
  Result:= Engine.Device.CreateSample;
  Result.Name:= Name;
  Result.LoadFromStream(Stream);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.Find(const Name: String): TPHXAudioSample;
var Index: Integer;
var Item : TPHXAudioSample;
begin
  for Index:= 0 to FList.Count-1 do
  begin
     Item:= TPHXAudioSample( FList.List[Index]);

     if SameText(Item.Name, Name) then
     begin
       Result:= Item;
       Exit;
     end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.Find(const Name: String; out Sample: TPHXAudioSample): Boolean;
var Index: Integer;
var Item : TPHXAudioSample;
begin
  for Index:= 0 to FList.Count-1 do
  begin
     Item:= TPHXAudioSample( FList.List[Index]);

     if SameText(Item.Name, Name) then
     begin
       Result:= True;
       Sample:= Item;

       Exit;
     end;
  end;
  Result:= False;
  Sample:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.GetList: PAudioSampleList;
begin
  Result:= PAudioSampleList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXAudioSamples.GetItem(Index: Integer): TPHXAudioSample;
begin
  Result:= TPHXAudioSample(FList.List[Index]);
end;

{$ENDREGION}

{$REGION 'TPHXAudioStream'}

// TPHXAudioStream
//==============================================================================
constructor TPHXAudioStream.Create;
begin
  FLooped:= False;
  FVolume:= 1.0;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioStream.SetLooped(const Value: Boolean);
begin
  FLooped:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioStream.SetVolume(const Value: Single);
begin
  FVolume:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXAudioStreams'}

// TPHXAudioStreams
//==============================================================================
constructor TPHXAudioStreams.Create(AEngine: TPHXAudioEngine);
begin
  FEngine:= AEngine;
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXAudioStreams.Destroy;
begin
  Clear;

  FList.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioStreams.Clear;
var Index: Integer;
begin
  For Index:= 0 to FList.Count-1 do
  begin
     TPHXAudioSample(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXAudioStreams.LoadStream(const FileName: String): TPHXAudioStream;
begin
  Result:= Engine.Device.CreateStream;
  Result.Name:= ExtractFileName(FileName);

  Result.LoadStream(FileName);

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAudioStreams.Find(const Name: String): TPHXAudioStream;
var Index: Integer;
var Stream: TPHXAudioStream;
begin
  for Index:= 0 to FList.Count-1 do
  begin
     Stream:= TPHXAudioStream( FList.List[Index]);

     if SameText(Stream.Name, Name) then
     begin
       Result:= Stream;
       Exit;
     end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAudioStreams.Find(const Name: String; out Stream: TPHXAudioStream): Boolean;
var Index: Integer;
begin
  for Index:= 0 to FList.Count-1 do
  begin
     Stream:= TPHXAudioStream( FList.List[Index]);

     if SameText(Stream.Name, Name) then
     begin
       Result:= True;

       Exit;
     end;
  end;
  Result:= False;
  Stream:= nil;
end;


//------------------------------------------------------------------------------
function TPHXAudioStreams.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXAudioStreams.GetList: PAudioStreamList;
begin
  Result:= PAudioStreamList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXAudioStreams.GetItem(Index: Integer): TPHXAudioStream;
begin
  Result:= TPHXAudioStream(FList.List[Index]);
end;

{$ENDREGION}


{$REGION 'TPHXAudioEngine'}

// TPHXAudioEngine
//==============================================================================
constructor TPHXAudioEngine.Create;
begin
  // Check so a default provider is avaiable
  if DefaultProvider = nil then
  begin
    raise Exception.Create('No default provider is avaiable.');
  end;

  Create(DefaultProvider.Create);
end;

//------------------------------------------------------------------------------
constructor TPHXAudioEngine.Create(AProvider: TPHXAudioProvider);
begin
  FInitialized:= False;
  FSamples    := TPHXAudioSamples.Create(Self);
  FStreams    := TPHXAudioStreams.Create(Self);
  FDevices    := TPHXSoundDeviceList.Create;
  FProvider   := AProvider;
  FDevice     := AProvider.CreateDevice(0);

  // Enumerate all avaiable devices
  FDevice.EnumerateDevices(FDevices);
end;

//------------------------------------------------------------------------------
destructor TPHXAudioEngine.Destroy;
begin
  FDevice:= nil;

  FSamples.Free;
  FStreams.Free;

  FProvider.Free;
  FDevices.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Initialize;
begin
  Assert(FInitialized = False);

  FDevice.Initialize;

  FInitialized:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Initialize(const Device: TPHXSoundDevice);
begin
  Assert(FInitialized = False);

  FDevice.Initialize(Device);

  FInitialized:= True;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Initialize(const DeviceName: String);
var Device: TPHXSoundDevice;
begin
  if FDevices.Find(DeviceName, Device) then
  begin
    Initialize(Device);
  end else
  begin
    raise Exception.CreateFmt('The device %s was not found', [DeviceName]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Initialize(const DeviceIndex: Integer);
var Device: TPHXSoundDevice;
begin
  if (DeviceIndex >= 0) and (DeviceIndex < FDevices.Count) then
  begin
    Device:= Devices.List^[DeviceIndex];

    Initialize(Device);
  end else
  begin
    raise Exception.CreateFmt('Invalid device index: %d', [DeviceIndex]);
  end;
end;

//------------------------------------------------------------------------------
function TPHXAudioEngine.LoadSample(const FileName: String): TPHXAudioSample;
begin
  Result:= FSamples.LoadSample(FileName)
end;

//------------------------------------------------------------------------------
function TPHXAudioEngine.LoadSample(const FileName: String; const Name: String): TPHXAudioSample;
begin
  Result:= FSamples.LoadSample(FileName);
  Result.Name:= Name;
end;

//------------------------------------------------------------------------------
function TPHXAudioEngine.LoadStream(const FileName: String): TPHXAudioStream;
begin
  Result:= FStreams.LoadStream(FileName);
end;

//------------------------------------------------------------------------------
function TPHXAudioEngine.LoadStream(const FileName: String; const Name: String): TPHXAudioStream;
begin
  Result:= FStreams.LoadStream(FileName);
  Result.Name:= Name;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Update;
begin
  FDevice.Update;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Play(const Sample: String);
var ASample: TPHXAudioSample;
begin
  if Samples.Find(Sample, ASample) then
  begin
    ASample.Play;
  end else
  begin
    raise Exception.CreateFmt(SSampleMissing, [Sample]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Play(const Sample: String; const Volume: Single);
var ASample: TPHXAudioSample;
begin
  if Samples.Find(Sample, ASample) then
  begin
    ASample.Play(Volume);
  end else
  begin
    raise Exception.CreateFmt(SSampleMissing, [Sample]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Pause;
begin
  FDevice.Pause;
end;


//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Resume;
begin
  FDevice.Resume;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.Stop;
begin
  FDevice.Stop;
end;

//------------------------------------------------------------------------------
function TPHXAudioEngine.GetVolume: Double;
begin
  Result:= FDevice.GetVolume;
end;

//------------------------------------------------------------------------------
procedure TPHXAudioEngine.SetVolume(const Value: Double);
begin
  FDevice.SetVolume(Value);
end;



{$ENDREGION}







end.
