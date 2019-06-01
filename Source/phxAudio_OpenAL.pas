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
unit phxAudio_OpenAL;
{<
  @abstract(OpenAL audio provider)

  http://www.noeska.com/doal/
}
interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  openal,

  phxClasses,
  phxAudio;

const

// Buffer size for streaming wave files
BUFFER_SIZE = 4096;

type

TPHXOpenALDevice = class;

// Provider for OpenAL
// See: @link(TPHXAudioProvider)
//------------------------------------------------------------------------------
TPHXOpenALProvider = class(TPHXAudioProvider)
  protected
    // Return the name of the provider
    function GetName: String; override;
  public
    constructor Create; override;

    // Load the provider
    function CreateDevice(const Flags: Cardinal): IPHXAudioDevice; override;
  end;

// Audio sample for OpenAL
// See: @link(TPHXAudioSample)
//------------------------------------------------------------------------------
TPHXOpenALSample = class(TPHXAudioSample)
  private
    FDevice: TPHXOpenALDevice;
    FBuffer: TALuint;
  protected
    procedure LoadSample(Stream: TStream); override;
  public
    constructor Create(ADevice: TPHXOpenALDevice);
    destructor Destroy; override;

    // Play this sample
    procedure Play; override;
    // Play this sample with a given volume
    procedure Play(const Volume: Single); override;
    // Stops all instances of this sample.
    procedure Stop; override;

    // The OpenAL handle
    property Device: TPHXOpenALDevice read FDevice;
    // The OpenAL buffer handle
    property Buffer: TALuint read FBuffer write FBuffer;
  end;

// Audio stream for OpenAL
// See: @link(TPHXAudioStream)
//------------------------------------------------------------------------------
TPHXOpenALStream = class(TPHXAudioStream)
  private
    FDevice : TPHXOpenALDevice;
    FStream : TStream;
    FSource : TALuint;
    FBuffers: array[0..3] of TALuint;

    Format: TALEnum;
    Freq  : TALSizei;
    Start : Int64;

    function LoadWave(var Format: TALenum; var Freq: TALsizei): Boolean;
    function ReadWave(var Buffer; const Size: Int64): Integer;
  protected
    procedure LoadStream(const Name: String); override;

    function GetDuration: Double; override;
    function GetPosition: Double; override;

    procedure SetVolume(const Value: Single); override;
  public
    constructor Create(ADevice: TPHXOpenALDevice);
    destructor Destroy; override;

    // Update the audio stream
    procedure Update;
    // Play the stream
    procedure Play; override;
    // Stop the stream
    procedure Stop; override;
    // Restarts the stream
    procedure Restart; override;

    // The OpenAL handle
    property Device: TPHXOpenALDevice read FDevice;
    // The file stream using for reading wave data
    property Stream: TStream read FStream;
    // The OpenAL source
    property Source: TALuint read FSource write FSource;
  end;

// Audio device for OpenAL
// See: @link(IPHXAudioDevice)
//------------------------------------------------------------------------------
TPHXOpenALDevice = class(TInterfacedObject, IPHXAudioDevice)
  private
   FDevice : TALCdevice;
   FContext: TALCcontext;
   FStreams: TList;

   oggext: Boolean;
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

    property Streams: TList read FStreams;
  end;

implementation

type
  //WAV file header
  TWAVHeader = record
    RIFFHeader: array [1..4] of AnsiChar;
    FileSize: Integer;
    WAVEHeader: array [1..4] of AnsiChar;
    FormatHeader: array [1..4] of AnsiChar;
    FormatHeaderSize: Integer;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BytesPerSample: Word;
    BitsPerSample: Word;
  end;
  TWAVChunk = record
    Name: array[1..4] of AnsiChar;
    Size: Integer
  end;

const
  WAV_STANDARD  = $0001;
  WAV_IMA_ADPCM = $0011;
  WAV_MP3       = $0055;

// http://kcat.strangesoft.net/openal-tutorial.html

{$REGION 'TPHXOpenALProvider'}

// TPHXOpenALProvider
//==============================================================================
constructor TPHXOpenALProvider.Create;
begin
  inherited;

end;

//------------------------------------------------------------------------------
function TPHXOpenALProvider.GetName: String;
begin
  Result:= 'OpenAL';
end;

//------------------------------------------------------------------------------
function TPHXOpenALProvider.CreateDevice(const Flags: Cardinal): IPHXAudioDevice;
begin
  Result:= TPHXOpenALDevice.Create;
end;

{$ENDREGION}

{$REGION 'TPHXOpenALSample'}

// TPHXOpenALSample
//==============================================================================
constructor TPHXOpenALSample.Create(ADevice: TPHXOpenALDevice);
begin
  FDevice:= ADevice;
  FBuffer:= 0;
end;

//------------------------------------------------------------------------------
destructor TPHXOpenALSample.Destroy;
begin
  if FBuffer <> 0 then
  begin
    alDeleteBuffers(1, @FBuffer);
  end;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALSample.LoadSample(Stream: TStream);
var format: TALEnum;
var size: TALSizei;
var freq: TALSizei;
var loop: TALInt;
var data: TALVoid;
begin
  AlGenBuffers(1, @FBuffer);
    LoadWavStream(Stream, format, data, size, freq, loop);
  alBufferData(FBuffer, format, data, size, freq);
  alutUnloadWAV(format, data, size, freq);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALSample.Play;
var Channel: TALuint;
begin
  AlGenSources(1, @Channel);

  AlSourcei(Channel, AL_BUFFER, FBuffer);
  //   AlSourcei ( source, AL_LOOPING, AL_TRUE );
  AlSourcePlay(Channel);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALSample.Play(const Volume: Single);
var Channel: TALuint;
begin
  AlGenSources(1, @Channel);

  AlSourcei(Channel, AL_BUFFER, FBuffer);
  //   AlSourcei ( source, AL_LOOPING, AL_TRUE );
  AlSourcePlay(Channel);
end;


{$ENDREGION}

{$REGION 'TPHXOpenALStream'}

// TPHXOpenALStream
//==============================================================================
constructor TPHXOpenALStream.Create(ADevice: TPHXOpenALDevice);
begin
  FDevice := ADevice;
  FStream := nil;

  FDevice.Streams.Add(Self);
end;

//------------------------------------------------------------------------------
destructor TPHXOpenALStream.Destroy;
begin
  FDevice.Streams.Remove(Self);

  if FBuffers[0] <> 0 then
  begin
    alDeleteBuffers(3, @FBuffers);
  end;

  if Assigned(FStream) then
  begin
    FStream.Free;
  end;

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXOpenALStream.LoadWave(var format: TALenum; var freq: TALsizei): Boolean;
var Header: TWavHeader;
var Chunk : TWAVChunk;
begin
  //Read wav header
  stream.Read(Header, sizeof(TWavHeader));

  //Determine SampleRate
  freq:= Header.SampleRate;

  //Detemine waveformat
  if Header.ChannelNumber = 1 then
  begin
    case Header.BitsPerSample of
      8 : Format := AL_FORMAT_MONO8;
      16: Format := AL_FORMAT_MONO16;
    end;
  end;
  if Header.ChannelNumber = 2 then
  begin
    case Header.BitsPerSample of
      8 : Format := AL_FORMAT_STEREO8;
      16: Format := AL_FORMAT_STEREO16;
    end;
  end;
  //Decode wave data if needed
  if Header.FormatCode=WAV_IMA_ADPCM then
  begin
    //TODO: add code to decompress IMA ADPCM data
  end;
  if Header.FormatCode=WAV_MP3 then
  begin
    //TODO: add code to decompress MP3 data
  end;

  Stream.Read(Chunk.Name, 4);
  Stream.Read(Chunk.Size, 4);

  if Chunk.Name = 'data' then
  begin
    Start:= Stream.Position;

    Result:= True;
  end else
  begin
    Result:= False;
  end;
end;

//------------------------------------------------------------------------------
function TPHXOpenALStream.ReadWave(var Buffer; const Size: Int64): Integer;
begin
  if Stream.Position < Stream.Size then
  begin
    Result:= Stream.Read(Buffer, Size);
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALStream.LoadStream(const Name: String);
var Data  : array[0..BUFFER_SIZE-1] of Byte;
var Size  : Integer;
begin
  // Free the current stream
  if Assigned(FStream) then FreeAndNil(FStream);

  alGenSources(1, @FSource);
  alGenBuffers(3, @FBuffers);

  if SameText(ExtractFileExt(Name), '.wav') then
  begin
    FStream:= TPHXContentManager.CreateStream(Name, ckAudio, fmOpenRead or fmShareDenyNone);

    if LoadWave(format, freq) then
    begin
      Size:= ReadWave(Data, BUFFER_SIZE);
      alBufferData(FBuffers[0], Format, @Data, Size, Freq);

      Size:= ReadWave(Data, BUFFER_SIZE);
      alBufferData(FBuffers[1], Format, @Data, Size, Freq);

      Size:= ReadWave(Data, BUFFER_SIZE);
      alBufferData(FBuffers[2], Format, @Data, Size, Freq);

      alSourceQueueBuffers(Source, 3, @FBuffers);

      //if Looped Then
      //  alSourcei( Source, AL_LOOPING, AL_TRUE )
     // else
      //  alSourcei( Source, AL_LOOPING, AL_FALSE );

    end else
    begin
      raise Exception.CreateFmt('Unsupported audio file: %s', [Name]);
    end;

  end else
  begin
    raise Exception.CreateFmt('Unsupported audio format: %s', [Name]);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALStream.Update;
var Processed: TALint ;
var Buffer   : TALuint;
var Data     : array[0..BUFFER_SIZE-1] of Byte;
var Size     : Integer;
//var State   : TALint ;
begin
  alGetSourcei(Source, AL_BUFFERS_PROCESSED, @Processed);

  if Processed > 0 then
  repeat
    alSourceUnqueueBuffers(Source, 1, @Buffer);

    // Restart if looped
    if Looped and (Stream.Position >= Stream.Size) then
    begin
      Stream.Position:= Start;
    end;

    Size:= ReadWave(Data, BUFFER_SIZE);

    if Size <> 0 then
    begin
      alBufferData(Buffer, Format, @Data, Size, Freq);

      alSourceQueueBuffers(Source, 1, @Buffer);
    end;

    Dec(Processed);
  until Processed <= 0;

//  alGetSourcei(Source, AL_SOURCE_STATE, @State);

//  if Looped and (State <> AL_PLAYING) then
//  begin
 //   alSourcePlay(source);
 // end;

end;


//------------------------------------------------------------------------------
procedure TPHXOpenALStream.Play;
begin
  alSourcePlay(Source);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALStream.Stop;
begin
  alSourceStop(Source);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALStream.Restart;
begin

end;

//------------------------------------------------------------------------------
function TPHXOpenALStream.GetDuration: Double;
begin
  Result:= 0;
end;

//------------------------------------------------------------------------------
function TPHXOpenALStream.GetPosition: Double;
begin
  Result:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALStream.SetVolume(const Value: Single);
begin
  inherited SetVolume(Value);

end;

{$ENDREGION}

{$REGION 'TPHXOpenALDevice'}

// TPHXOpenALDevice
//==============================================================================
constructor TPHXOpenALDevice.Create;
begin
  FStreams:= TList.Create;

  InitOpenAL();

  if OpenAL.LibHandle = 0 then
  begin
    raise Exception.Create('Failed to initialize OpenAL, make shure you have installed the OpenAL drivers');
  end;

  OpenAL.ReadOpenALExtensions;

  oggext:= alIsExtensionPresent('AL_EXT_vorbis');
end;

//------------------------------------------------------------------------------
destructor TPHXOpenALDevice.Destroy;
begin
  //Release context(s)
  alcDestroyContext(FContext);
  //Close device
  alcCloseDevice(FDevice);

  FStreams.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.EnumerateDevices(List: TPHXSoundDeviceList);
var Buffer: PAnsiChar;
var Curr  : PAnsiChar;
var Start : PAnsiChar;
var Ident : Cardinal;
var Name  : String;
begin
  if alcIsExtensionPresent(nil,'ALC_ENUMERATION_EXT') = TRUE then
  begin
    Buffer:= alcGetString(nil, ALC_DEVICE_SPECIFIER);

    Curr := Buffer;
    Start:= Curr;
    Ident:= 1;
    while True do
    begin

      if (Curr^ = #0) then
      begin
        SetString(Name, Start, Curr - Start);

        List.Add(Ident, Name);

        Inc(Ident);
        Inc(Curr);

        if (Curr^ = #0) then
        begin
          Exit;
        end;
        Start:= Curr;
      end;
      Inc(Curr);
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.Initialize;
begin
  // Open device
  FDevice := alcOpenDevice(nil);
  // Create context(s)
  FContext := alcCreateContext(FDevice, nil);
  // Set active context
  alcMakeContextCurrent(FContext);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.Initialize(const Device: TPHXSoundDevice);
begin
  // Open device
  FDevice := alcOpenDevice( PAnsiChar(AnsiString(Device.Name)) );
  // Create context(s)
  FContext := alcCreateContext(FDevice, nil);
  // Set active context
  alcMakeContextCurrent(FContext);
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.Update;
var Index: Integer;
begin
  for Index := 0 to Streams.Count-1 do
  begin
    TPHXOpenALStream(Streams.List[Index]).Update;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.Pause;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.Resume;
begin

end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.Stop;
begin

end;

//------------------------------------------------------------------------------
function TPHXOpenALDevice.CreateSample: TPHXAudioSample;
begin
  Result:= TPHXOpenALSample.Create(Self);
end;

//------------------------------------------------------------------------------
function TPHXOpenALDevice.CreateStream: TPHXAudioStream;
begin
  Result:= TPHXOpenALStream.Create(Self);
end;


//------------------------------------------------------------------------------
function TPHXOpenALDevice.GetVolume: Double;
begin
  Result:= 0;
end;

//------------------------------------------------------------------------------
procedure TPHXOpenALDevice.SetVolume(const Value: Double);
begin

end;
{$ENDREGION}





procedure TPHXOpenALSample.Stop;
begin
  inherited;

end;

initialization
  RegisterAudioProvider('OpenAL', TPHXOpenALProvider);
end.
