unit phxOpenGL_SDL2;

interface

{$I ../phxConfig.inc}

uses
  SysUtils, Classes, dglOpenGL, SDL2, phxLogger, phxTypes, phxClasses, phxEvents, phxDevice, phxGraphics, phxOpenGL;

const
  ProviderName = 'OpenGL.SDL2';

type

  { TPHXOpenGL_Provider_SDL2 }

  TPHXOpenGL_Provider_SDL2 = class(TPHXProvider)
  protected
    function GetName: String; override;
    function GetTarget: TPHXProviderTarget; override;
  public
    constructor Create; override;
    function CreateRenderer: IPHXDevice; override;
  end;

  { TPHXOpenGLRendererSDL2 }

  TPHXOpenGLRendererSDL2 = class(TPHXOpenGL_Renderer)
  private
    FInitialized: Boolean;
    FWindow: PSDL_Window;
    FSurface: PSDL_Surface;
    FTitle: String;
    FWidth: Integer;
    FHeight: Integer;
    FFullscreen: Boolean;
    FWindowFlags: TPHXWindowFlags;
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetFlags: TPHXWindowFlags; override;
    procedure SetTitle(const Title: String); override;
    procedure SetFlags(const Flags: TPHXWindowFlags); override;
    procedure SetIcon(const Icon: String); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EnumDisplayModes(const Modes: TPHXDisplayModes); override;
    procedure Initialize(const Parameters: TPHXDeviceParameters); override;
    procedure Reinitialize(const Parameters: TPHXDeviceParameters); override;
    procedure Finalize; override;
    procedure Update; override;
    procedure Clear; override;
    procedure Flip; override;
  end;

implementation

{ TPHXOpenGL_Provider_SDL2 }

function TPHXOpenGL_Provider_SDL2.GetName: String;
begin
  Result:=ProviderName;
end;

function TPHXOpenGL_Provider_SDL2.GetTarget: TPHXProviderTarget;
begin
  Result:=ptOpenGL_3;
end;

constructor TPHXOpenGL_Provider_SDL2.Create;
begin
  inherited Create;
end;

function TPHXOpenGL_Provider_SDL2.CreateRenderer: IPHXDevice;
begin
  Result := TPHXOpenGLRendererSDL2.Create;
end;

{ TPHXOpenGLRendererSDL2 }

function TPHXOpenGLRendererSDL2.GetWidth: Integer;
begin
  Result:= FWidth;
end;

function TPHXOpenGLRendererSDL2.GetHeight: Integer;
begin
  Result:= FHeight;
end;

function TPHXOpenGLRendererSDL2.GetFlags: TPHXWindowFlags;
begin
  Result := FWindowFlags;
end;

procedure TPHXOpenGLRendererSDL2.SetTitle(const Title: String);
begin

end;

procedure TPHXOpenGLRendererSDL2.SetFlags(const Flags: TPHXWindowFlags);
begin
  FWindowFlags := Flags;
end;

procedure TPHXOpenGLRendererSDL2.SetIcon(const Icon: String);
begin

end;

constructor TPHXOpenGLRendererSDL2.Create;
begin
  inherited Create;
  FInitialized := False;
  FWindow := nil;
  FTitle := 'Phoenix Framework';
  FWidth := 800;
  FHeight := 600;
  FFullscreen := False;
  ExitProc := @SDL_Quit;
end;

destructor TPHXOpenGLRendererSDL2.Destroy;
begin
  inherited Destroy;
end;

procedure TPHXOpenGLRendererSDL2.EnumDisplayModes(const Modes: TPHXDisplayModes);
begin

end;

procedure TPHXOpenGLRendererSDL2.Initialize(const Parameters: TPHXDeviceParameters);
begin
  FTitle := Parameters.Title;
  FWidth := Parameters.Width;
  FHeight := Parameters.Height;
  FFullscreen:= Parameters.Fullscreen;

    if SDL_Init( SDL_INIT_VIDEO ) < 0 then raise Exception.Create('Failed to initialize SDL2.');


  FWindow := SDL_CreateWindow(PChar(FTitle),SDL_WINDOWPOS_UNDEFINED,
   SDL_WINDOWPOS_UNDEFINED, FWidth, FHeight, SDL_WINDOW_SHOWN); //todo fscr

   FSurface := SDL_GetWindowSurface(FWindow);



    if (dglOpenGL.InitOpenGL <> True) then raise Exception.Create('OpenGL Initialization failed.');
     dglOpenGL.ReadExtensions;
     InitializeOpenGL;
     FInitialized := True;
end;

procedure TPHXOpenGLRendererSDL2.Reinitialize(const Parameters: TPHXDeviceParameters);
begin
 TPHXNotifications.Notify(dnContextCreated);
end;

procedure TPHXOpenGLRendererSDL2.Finalize;
begin

end;

procedure TPHXOpenGLRendererSDL2.Update;
begin
  inherited Update;
end;

procedure TPHXOpenGLRendererSDL2.Clear;
begin
  inherited Clear;
end;

procedure TPHXOpenGLRendererSDL2.Flip;
begin
  inherited Flip;
end;


initialization
  RegisterProvider(ProviderName, TPHXOpenGL_Provider_SDL2);
end.

