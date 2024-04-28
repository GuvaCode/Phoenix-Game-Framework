unit main;

{$mode delphi}{$H+}

interface

uses SysUtils, Classes,

  Unzip,

  phxLogger,
  phxTypes,
  phxClasses,
  phxEvents,
  phxMath,
  phxApplication,
  phxDevice,
  phxCanvas,
  phxTexture,
  phxFont;

type

// Using the game template is the easy way to use Phoenix.
// Check the source in phxTemplate.pas to get an idea what the application class
// does.
//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Font   : TPHXFont;
    List    : TStringList;
    Textures: TPHXTextureList;
  public
    constructor Create; override;

    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

//------------------------------------------------------------------------------
TGameContent = class(TPHXContentLoader)
  private
    // The archive filename
    FArchive  : String;
    // The archive password
    FPassword : String;
    // Uziplib callback functions
    FCallbacks: TUserFunctions;
  public
    constructor Create(const Archive: String); overload;
    constructor Create(const Archive: String; const Password : String); overload;

    function ExpandFileName(const Name: string; const Kind: TPHXContentKind): string; override;
    function Exists(const Name: string; const Kind: TPHXContentKind): Boolean; override;

    function CreateStream(const Name: string; const Kind: TPHXContentKind;   const Mode: Word): TStream; override;

    // The archive filename
    property Archive: String read FArchive;
    // The archive password
    property Password: String read FPassword write FPassword;
  end;


implementation

uses

  phxProvider_OpenGL_GLFW,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
constructor TGame.Create;
begin
  inherited;
  TPHXContentManager.Loader:= TGameContent.Create('content/Archive.zip');
end;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  List:= TStringList.Create;

  // Create the device using the default provider
  Device:= TPHXDevice.Create;
  // This loads a new icon for the window
  Device.Icon:= ContentPath + 'Phoenix.bmp';
  // Initialize the device
  Device.Initialize('Phoenix Demo', 800, 600);

  // Create the canvas
  Canvas:= Device.CreateCanvas;

  Font:= TPHXFont.Create(Device, Canvas);
  Font.LoadFromFile('Calibri12.phxfnt');

  Textures:= TPHXTextureList.Create(Device);
  Textures.LoadTexture('background.png');

  // Create the timer
  Timer:= TPHXTimer.Create;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;

  Canvas.Texture:= Textures[0];
  // Change the color to white
  Canvas.Color:= clrWhite;
  // Draw a rectangle
  Canvas.FilledRectangle(100, 100, 100+256, 100+256);

  // Draw the content of the archive
  Font.TextOut(Device.Bounds, Format('FPS: %d', [TImer.FrameRate]), taTopRight);

  // Flush the canvas
  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  List.Free;
  Timer.Free;
  Canvas.Free;
  Device.Free;
  Textures.Free;
end;






// TGameContent
//==============================================================================
constructor TGameContent.Create(const Archive: String);
begin
  Create(Archive, '');
end;

//------------------------------------------------------------------------------
constructor TGameContent.Create(const Archive: String; const Password : String);
begin
  if Unzip.LibHandle = nil then
  begin
    if not Unzip.LoadUnzipLib(Unzip.LIBNAME) then
    begin
      raise Exception.Create('Failed to load ' + Unzip.LIBNAME);
    end;
  end;

  if not FileExists(Archive) then
  begin
    raise Exception.Create('Could not load archive ' + Archive);
  end;

  FArchive := Archive;
  FPassword:= Password;

  FCallbacks:= DefaultUserFunctions;
//  FCallbacks.SendApplicationMessage:= unzip_List;
//  FCallbacks.Password              := unzip_Password;
end;

//------------------------------------------------------------------------------
function TGameContent.ExpandFileName(const Name: string; const Kind: TPHXContentKind): string;
begin
  Result:= Name;
end;

//------------------------------------------------------------------------------
function TGameContent.Exists(const Name: string; const Kind: TPHXContentKind): Boolean;
var Buffer: TUzpBuffer;
begin
 // unzipInstance:= Self;

  Buffer.Size:= 0;
  Buffer.Data:= nil;

  Result:= Wiz_UnzipToMemory( PAnsiChar(AnsiString(FArchive)), PAnsiChar(AnsiString(Name)), @FCallbacks, @Buffer) = 1;

  UzpFreeMemBuffer(@Buffer);
end;

//------------------------------------------------------------------------------
function TGameContent.CreateStream(const Name: string; const Kind: TPHXContentKind; const Mode: Word): TStream;
var Buffer: TUzpBuffer;
var Status: Integer;
begin
  Buffer.Size:= 0;
  Buffer.Data:= nil;

  // Try to unzip the archive
  Status:= Wiz_UnzipToMemory(PAnsiChar( AnsiString(FArchive)), PAnsiChar(AnsiString(Name)), @FCallbacks, @Buffer);

  if Status = 1 then
  begin
    Result:= TMemoryStream.Create;

    TMemoryStream(Result).Write(Buffer.Data^, Buffer.Size);
    TMemoryStream(Result).Position:= 0;
  end else
  begin
    raise EPHXContentError.Create('Could not load content: ' + Name);
  end;

  UzpFreeMemBuffer(@Buffer);
end;


end.
